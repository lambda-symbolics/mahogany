/*
 * LISPBSD additions: client pid lookup, an internal status bar, output power
 * control and the idle protocols. Kept in one file so the upstream sources
 * stay close to stumpwm/mahogany master.
 */
/* LOCAL_PEEREID and struct unpcbid are hidden behind _NETBSD_SOURCE, which
 * heart's -D_POSIX_C_SOURCE would otherwise leave undefined. */
#if defined(__NetBSD__)
#define _NETBSD_SOURCE
#endif
#include <cairo/cairo.h>
#include <pango/pangocairo.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_idle_inhibit_v1.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_cursor_shape_v1.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_idle_notify_v1.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/util/log.h>

#include "render/cairo_buffer.h"
#include "render/scale_util.h"
#include <hrt/hrt_input.h>
#include <hrt/hrt_lispbsd.h>
#include <hrt/hrt_message.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_scene.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_view.h>

/* State lives here rather than in struct hrt_server: the Lisp side allocates
 * hrt_server from its own copy of the struct layout, so the C struct must not
 * grow. */
static struct wlr_idle_inhibit_manager_v1 *idle_inhibit;
static struct wlr_idle_notifier_v1 *idle_notifier;
static struct wlr_xdg_decoration_manager_v1 *decoration_manager;
static struct wlr_xdg_activation_v1 *activation;
static void lpsched_send_input(void);
static struct wl_listener new_decoration;
static struct wl_listener decoration_manager_destroy;

/* Tiled windows get no decorations at all: tell every client that asks that
 * the server draws them, and then draw nothing but the frame border. */
struct lispbsd_decoration {
    struct wlr_xdg_toplevel_decoration_v1 *decoration;
    struct wl_listener request_mode;
    struct wl_listener surface_commit;
    struct wl_listener destroy;
};

/* The mode can only be sent once the xdg surface is initialized (it schedules
 * a configure); before that wlroots asserts. */
static void decoration_set_server_side(struct wlr_xdg_toplevel_decoration_v1 *d) {
    if (d->toplevel->base->initialized) {
        wlr_xdg_toplevel_decoration_v1_set_mode(
            d, WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
    }
}

static void handle_decoration_surface_commit(struct wl_listener *listener,
                                             void *data) {
    struct lispbsd_decoration *deco =
        wl_container_of(listener, deco, surface_commit);
    if (deco->decoration->toplevel->base->initialized) {
        decoration_set_server_side(deco->decoration);
        wl_list_remove(&deco->surface_commit.link);
        wl_list_init(&deco->surface_commit.link);
    }
}

static void handle_decoration_request_mode(struct wl_listener *listener,
                                           void *data) {
    struct lispbsd_decoration *deco =
        wl_container_of(listener, deco, request_mode);
    decoration_set_server_side(deco->decoration);
}

static void handle_decoration_destroy(struct wl_listener *listener, void *data) {
    struct lispbsd_decoration *deco = wl_container_of(listener, deco, destroy);
    wl_list_remove(&deco->request_mode.link);
    wl_list_remove(&deco->surface_commit.link);
    wl_list_remove(&deco->destroy.link);
    free(deco);
}

/* wlroots asserts at display teardown that nobody still listens to the
 * manager, so let go when it announces its destruction. */
static void handle_decoration_manager_destroy(struct wl_listener *listener,
                                              void *data) {
    wl_list_remove(&new_decoration.link);
    wl_list_remove(&decoration_manager_destroy.link);
    decoration_manager = NULL;
}

static void handle_new_decoration(struct wl_listener *listener, void *data) {
    struct wlr_xdg_toplevel_decoration_v1 *decoration = data;
    struct lispbsd_decoration *deco = calloc(1, sizeof(*deco));
    if (!deco) {
        return;
    }
    deco->decoration          = decoration;
    deco->request_mode.notify = handle_decoration_request_mode;
    wl_signal_add(&decoration->events.request_mode, &deco->request_mode);
    deco->destroy.notify = handle_decoration_destroy;
    wl_signal_add(&decoration->events.destroy, &deco->destroy);
    if (decoration->toplevel->base->initialized) {
        decoration_set_server_side(decoration);
        wl_list_init(&deco->surface_commit.link);
    } else {
        deco->surface_commit.notify = handle_decoration_surface_commit;
        wl_signal_add(&decoration->toplevel->base->surface->events.commit,
                      &deco->surface_commit);
    }
}

/* cursor-shape-v1: clients name a cursor ("default", "text", ...) and the
 * compositor draws it from its own theme.  GTK 4.22 without this protocol
 * draws its built-in 32 px images scaled by cursor-size/32 the wrong way
 * round (42 px for size 24), so its cursor was twice the size of
 * everyone else's. */
static struct wlr_cursor_shape_manager_v1 *cursor_shape_manager;
static struct wl_listener cursor_shape_request;
static struct hrt_server *cursor_shape_server;

static void handle_cursor_shape_request(struct wl_listener *listener,
                                        void *data) {
    struct wlr_cursor_shape_manager_v1_request_set_shape_event *event = data;
    struct hrt_seat *seat = hrt_server_seat(cursor_shape_server);

    if (event->device_type != WLR_CURSOR_SHAPE_MANAGER_V1_DEVICE_TYPE_POINTER ||
        seat->seat->pointer_state.focused_client != event->seat_client)
        return;
    wlr_cursor_set_xcursor(seat->cursor, seat->xcursor_manager,
                           wlr_cursor_shape_v1_name(event->shape));
}

bool hrt_lispbsd_init(struct hrt_server *server) {
    cursor_shape_server = server;
    cursor_shape_manager = wlr_cursor_shape_manager_v1_create(server->wl_display, 1);
    if (cursor_shape_manager) {
        cursor_shape_request.notify = handle_cursor_shape_request;
        wl_signal_add(&cursor_shape_manager->events.request_set_shape,
                      &cursor_shape_request);
    } else {
        wlr_log(WLR_ERROR, "Could not create the cursor shape manager");
    }
    idle_inhibit = wlr_idle_inhibit_v1_create(server->wl_display);
    if (!idle_inhibit) {
        wlr_log(WLR_ERROR, "Could not create idle inhibit manager");
        return false;
    }
    idle_notifier = wlr_idle_notifier_v1_create(server->wl_display);
    if (!idle_notifier) {
        wlr_log(WLR_ERROR, "Could not create idle notifier");
        return false;
    }
    decoration_manager = wlr_xdg_decoration_manager_v1_create(server->wl_display);
    if (!decoration_manager) {
        wlr_log(WLR_ERROR, "Could not create the xdg decoration manager");
        return false;
    }
    new_decoration.notify = handle_new_decoration;
    wl_signal_add(&decoration_manager->events.new_toplevel_decoration,
                  &new_decoration);
    decoration_manager_destroy.notify = handle_decoration_manager_destroy;
    wl_signal_add(&decoration_manager->events.destroy,
                  &decoration_manager_destroy);
    /* Launchers (wmenu-run, fuzzel) insist on xdg-activation to hand focus
     * to what they start. Focus follows new windows here anyway, so the
     * global only needs to exist. */
    activation = wlr_xdg_activation_v1_create(server->wl_display);
    if (!activation) {
        wlr_log(WLR_ERROR, "Could not create xdg activation");
        return false;
    }
    /* Register with the power governor right away. */
    lpsched_send_input();
    return true;
}

pid_t hrt_view_pid(struct hrt_view *view) {
    if (!view || !view->xdg_toplevel || !view->xdg_toplevel->base ||
        !view->xdg_toplevel->base->resource) {
        return 0;
    }
    struct wl_client *client =
        wl_resource_get_client(view->xdg_toplevel->base->resource);
    if (!client) {
        return 0;
    }
    pid_t pid = 0;
#if defined(__NetBSD__) && defined(LOCAL_PEEREID)
    /* libwayland's wl_client_get_credentials only knows SO_PEERCRED (Linux);
     * NetBSD reports the peer through LOCAL_PEEREID on the socket. */
    struct unpcbid unp;
    socklen_t len = sizeof(unp);
    if (getsockopt(wl_client_get_fd(client), 0, LOCAL_PEEREID, &unp, &len) == 0) {
        return unp.unp_pid;
    }
    wlr_log(WLR_ERROR, "LOCAL_PEEREID failed on client fd %d: %s",
            wl_client_get_fd(client), strerror(errno));
#endif
    wl_client_get_credentials(client, &pid, NULL, NULL);
    return pid;
}

int hrt_idle_inhibitor_count(void) {
    if (!idle_inhibit) {
        return 0;
    }
    return wl_list_length(&idle_inhibit->inhibitors);
}

/*
 * lpschedd, the LISPBSD power governor, polls for input four times a second
 * unless a compositor reports it.  Report the start of each input burst
 * (input after LPSCHED_QUIET_MS of silence) with one datagram, so the
 * governor can sleep in between and still upshift at once.  Harmless where
 * no lpschedd listens: the send just fails.
 */
#define LPSCHED_SOCKET "/var/run/lpsched.sock"
#define LPSCHED_QUIET_MS 2000
static int lpsched_fd = -1;
static struct timespec lpsched_last_input;

static void lpsched_send_input(void) {
    struct sockaddr_un sun;
    char msg[32];
    int len;

    if (lpsched_fd < 0) {
        lpsched_fd = socket(AF_UNIX, SOCK_DGRAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
        if (lpsched_fd < 0) {
            return;
        }
    }
    memset(&sun, 0, sizeof(sun));
    sun.sun_family = AF_UNIX;
    snprintf(sun.sun_path, sizeof(sun.sun_path), "%s", LPSCHED_SOCKET);
    len = snprintf(msg, sizeof(msg), "input %d", (int)getpid());
    (void)sendto(lpsched_fd, msg, (size_t)len, 0, (struct sockaddr *)&sun,
                 sizeof(sun));
}

static void lpsched_note_input(void) {
    struct timespec now;
    long long quiet_ms;

    clock_gettime(CLOCK_MONOTONIC, &now);
    quiet_ms = (long long)(now.tv_sec - lpsched_last_input.tv_sec) * 1000 +
               (now.tv_nsec - lpsched_last_input.tv_nsec) / 1000000;
    lpsched_last_input = now;
    if (quiet_ms >= LPSCHED_QUIET_MS) {
        lpsched_send_input();
    }
}

static void (*activity_cb)(void);
static bool activity_armed;

void hrt_set_activity_callback(void (*cb)(void)) {
    activity_cb = cb;
}

void hrt_arm_activity_callback(void) {
    activity_armed = true;
}

void hrt_idle_notify_activity(struct hrt_seat *seat) {
    if (idle_notifier && seat && seat->seat) {
        wlr_idle_notifier_v1_notify_activity(idle_notifier, seat->seat);
    }
    lpsched_note_input();
    if (activity_armed && activity_cb) {
        activity_armed = false;
        activity_cb();
    }
}

void hrt_view_set_suspended(struct hrt_view *view, bool suspended) {
    /* wlroots schedules a configure, which needs an initialized surface. */
    if (view && view->xdg_toplevel && view->xdg_toplevel->base &&
        view->xdg_toplevel->base->initialized) {
        wlr_xdg_toplevel_set_suspended(view->xdg_toplevel, suspended);
    }
}

/* ---- focus follows mouse ---------------------------------------------- */

static void (*pointer_enter_cb)(struct hrt_seat *seat);
static void *pointer_last_view;

void hrt_set_pointer_enter_callback(void (*cb)(struct hrt_seat *seat)) {
    pointer_enter_cb = cb;
}

void hrt_lispbsd_pointer_motion(struct hrt_seat *seat, void *view) {
    if (view == pointer_last_view) {
        return;
    }
    pointer_last_view = view;
    if (view && pointer_enter_cb) {
        pointer_enter_cb(seat);
    }
}

/* ---- status bar ------------------------------------------------------- */

#define MAX_BARS 8

struct lispbsd_bar {
    struct hrt_output *output;
    struct wlr_scene_buffer *node;
    struct wl_listener output_destroy;
    int height;
};

static struct lispbsd_bar bars[MAX_BARS];

static void handle_bar_output_destroy(struct wl_listener *listener,
                                      void *data) {
    struct lispbsd_bar *bar = wl_container_of(listener, bar, output_destroy);
    wl_list_remove(&bar->output_destroy.link);
    /* The scene node dies with the output's scene tree. */
    bar->node   = NULL;
    bar->output = NULL;
    bar->height = 0;
}

static struct lispbsd_bar *find_bar(struct hrt_output *output, bool create) {
    for (int i = 0; i < MAX_BARS; i++) {
        if (bars[i].output == output) {
            return &bars[i];
        }
    }
    if (!create) {
        return NULL;
    }
    for (int i = 0; i < MAX_BARS; i++) {
        if (bars[i].output == NULL) {
            bars[i].output               = output;
            bars[i].node                 = NULL;
            bars[i].height               = 0;
            bars[i].output_destroy.notify = handle_bar_output_destroy;
            wl_signal_add(&output->wlr_output->events.destroy,
                          &bars[i].output_destroy);
            return &bars[i];
        }
    }
    return NULL;
}

static PangoLayout *bar_layout(cairo_t *c, const char *font, double scale,
                               const char *markup) {
    PangoLayout *layout        = pango_cairo_create_layout(c);
    PangoFontDescription *desc = pango_font_description_from_string(font);
    PangoAttrList *attrs       = pango_attr_list_new();
    pango_attr_list_insert(attrs, pango_attr_scale_new(scale));
    pango_layout_set_font_description(layout, desc);
    pango_layout_set_single_paragraph_mode(layout, true);
    pango_layout_set_attributes(layout, attrs);
    pango_layout_set_markup(layout, markup ? markup : "", -1);
    pango_attr_list_unref(attrs);
    pango_font_description_free(desc);
    return layout;
}

int hrt_bar_set(struct hrt_output *output, const char *left_markup,
                const char *right_markup, struct hrt_message_theme *theme,
                bool bottom) {
    if (!output || !output->scene || !theme) {
        return 0;
    }
    struct lispbsd_bar *bar = find_bar(output, true);
    if (!bar) {
        wlr_log(WLR_ERROR, "Too many bars");
        return 0;
    }

    struct wlr_box output_box;
    wlr_output_layout_get_box(output->server->output_layout,
                              output->wlr_output, &output_box);
    if (output_box.width <= 0) {
        return 0;
    }
    double scale = output->wlr_output->scale;
    int pad_x    = theme->message_padding;
    int pad_y    = theme->message_border_width;

    /* Measure with a throwaway surface so the buffer can be exactly as tall
     * as the text. */
    cairo_surface_t *probe = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
    cairo_t *pc            = cairo_create(probe);
    PangoLayout *left  = bar_layout(pc, theme->font, scale, left_markup);
    PangoLayout *right = bar_layout(pc, theme->font, scale, right_markup);
    int lw, lh, rw, rh;
    pango_layout_get_pixel_size(left, &lw, &lh);
    pango_layout_get_pixel_size(right, &rw, &rh);
    g_object_unref(left);
    g_object_unref(right);
    cairo_destroy(pc);
    cairo_surface_destroy(probe);

    int text_h  = lh > rh ? lh : rh;
    int buf_w   = (int)(output_box.width * scale);
    int buf_h   = text_h + 2 * (int)(pad_y * scale);
    if (buf_h < 1) {
        buf_h = 1;
    }

    struct hrt_cairo_buffer *buf = hrt_cairo_buffer_create(buf_w, buf_h);
    if (!buf) {
        return 0;
    }
    cairo_t *c = cairo_create(buf->surface);
    cairo_font_options_t *fo = cairo_font_options_create();
    cairo_font_options_set_hint_style(fo, CAIRO_HINT_STYLE_FULL);
    cairo_font_options_set_antialias(fo, CAIRO_ANTIALIAS_GRAY);
    cairo_set_font_options(c, fo);
    cairo_font_options_destroy(fo);

    cairo_set_operator(c, CAIRO_OPERATOR_SOURCE);
    cairo_set_source_rgba(c, theme->background_color[0],
                          theme->background_color[1],
                          theme->background_color[2],
                          theme->background_color[3]);
    cairo_paint(c);
    cairo_set_operator(c, CAIRO_OPERATOR_OVER);
    cairo_set_source_rgba(c, theme->font_color[0], theme->font_color[1],
                          theme->font_color[2], theme->font_color[3]);

    left  = bar_layout(c, theme->font, scale, left_markup);
    right = bar_layout(c, theme->font, scale, right_markup);
    pango_layout_get_pixel_size(left, &lw, &lh);
    pango_layout_get_pixel_size(right, &rw, &rh);
    int px = (int)(pad_x * scale);
    int py = (int)(pad_y * scale);
    cairo_move_to(c, px, py + (text_h - lh) / 2);
    pango_cairo_show_layout(c, left);
    cairo_move_to(c, buf_w - rw - px, py + (text_h - rh) / 2);
    pango_cairo_show_layout(c, right);
    g_object_unref(left);
    g_object_unref(right);
    cairo_destroy(c);
    cairo_surface_flush(buf->surface);

    struct wlr_box dest;
    if (!compute_scaled_box(buf_w, buf_h, scale, &dest)) {
        wlr_buffer_drop(&buf->base);
        return 0;
    }
    dest.width = output_box.width;

    if (!bar->node) {
        bar->node = wlr_scene_buffer_create(output->scene->top, &buf->base);
        if (!bar->node) {
            wlr_buffer_drop(&buf->base);
            return 0;
        }
    } else {
        wlr_scene_buffer_set_buffer(bar->node, &buf->base);
    }
    /* The scene node holds its own reference now. */
    wlr_buffer_drop(&buf->base);

    wlr_scene_buffer_set_filter_mode(bar->node,
                                     compute_scale_filter(&buf->base, scale));
    wlr_scene_buffer_set_dest_size(bar->node, dest.width, dest.height);
    pixman_region32_t opaque;
    pixman_region32_init(&opaque);
    pixman_region32_union_rect(&opaque, &opaque, 0, 0, dest.width, dest.height);
    wlr_scene_buffer_set_opaque_region(bar->node, &opaque);
    pixman_region32_fini(&opaque);

    int y = bottom ? output_box.y + output_box.height - dest.height
                   : output_box.y;
    wlr_scene_node_set_position(&bar->node->node, output_box.x, y);
    wlr_scene_node_set_enabled(&bar->node->node, true);
    wlr_scene_node_raise_to_top(&bar->node->node);
    bar->height = dest.height;
    return bar->height;
}

void hrt_bar_clear(struct hrt_output *output) {
    struct lispbsd_bar *bar = find_bar(output, false);
    if (!bar) {
        return;
    }
    if (bar->node) {
        wlr_scene_node_destroy(&bar->node->node);
        bar->node = NULL;
    }
    bar->height = 0;
}

/* ---- refresh rate ----------------------------------------------------- */

uint64_t hrt_output_frames_rendered(struct hrt_output *output) {
    return output ? output->frames_rendered : 0;
}

int hrt_output_refresh(struct hrt_output *output) {
    if (!output || !output->wlr_output || !output->wlr_output->current_mode) {
        return 0;
    }
    return output->wlr_output->current_mode->refresh;
}

int hrt_output_set_refresh(struct hrt_output *output, int refresh_mhz) {
    if (!output || !output->wlr_output) {
        return 0;
    }
    struct wlr_output *wlr_output = output->wlr_output;
    struct wlr_output_mode *cur = wlr_output->current_mode;
    if (!cur || wl_list_empty(&wlr_output->modes)) {
        return 0;
    }
    struct wlr_output_mode *mode, *best = NULL;
    int best_diff = INT_MAX;
    wl_list_for_each(mode, &wlr_output->modes, link) {
        if (mode->width != cur->width || mode->height != cur->height) {
            continue;
        }
        int diff = abs(mode->refresh - refresh_mhz);
        if (diff < best_diff) {
            best_diff = diff;
            best      = mode;
        }
    }
    if (!best) {
        return 0;
    }
    if (best == cur) {
        return cur->refresh;
    }
    struct wlr_output_state state;
    wlr_output_state_init(&state);
    wlr_output_state_set_mode(&state, best);
    bool ok = wlr_output_commit_state(wlr_output, &state);
    wlr_output_state_finish(&state);
    if (!ok) {
        wlr_log(WLR_ERROR, "Could not switch %s to %d mHz", wlr_output->name,
                best->refresh);
        return 0;
    }
    wlr_log(WLR_INFO, "Output %s now %d mHz", wlr_output->name, best->refresh);
    return best->refresh;
}

/* ---- output power ----------------------------------------------------- */

bool hrt_output_set_power(struct hrt_output *output, bool on) {
    if (!output || !output->wlr_output) {
        return false;
    }
    struct wlr_output *wlr_output = output->wlr_output;
    if (wlr_output->enabled == on) {
        return true;
    }
    struct wlr_output_state state;
    wlr_output_state_init(&state);
    wlr_output_state_set_enabled(&state, on);
    if (on && !wlr_output->current_mode && !wl_list_empty(&wlr_output->modes)) {
        wlr_output_state_set_mode(&state, wlr_output_preferred_mode(wlr_output));
    }
    bool ok = wlr_output_commit_state(wlr_output, &state);
    wlr_output_state_finish(&state);
    if (!ok) {
        wlr_log(WLR_ERROR, "Could not turn output %s %s", wlr_output->name,
                on ? "on" : "off");
        return false;
    }
    if (on && output->wlr_scene) {
        /* Everything must be repainted after the panel comes back. */
        wlr_output_schedule_frame(wlr_output);
    }
    wlr_log(WLR_INFO, "Output %s turned %s", wlr_output->name, on ? "on" : "off");
    return true;
}
