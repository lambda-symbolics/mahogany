/*
 * LispBSD: output mirroring, like wl-mirror but inside the compositor.  The
 * buffer each commit puts on the source output is shown, without a copy,
 * by a scene buffer node that only the target output sees: the target is
 * taken out of the output layout (no windows, no cursor, no layer surfaces)
 * and its scene output looks at a far corner of the scene where the node
 * lives.  The source draws its cursor in software meanwhile, so the pointer
 * is in the picture.  Which outputs, the fit and the region are policy in
 * Lisp (lisp/mirror.lisp), which calls hrt_mirror_set_view.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cairo.h>
#include <drm_fourcc.h>
#include <wayland-server-core.h>
#include <wlr/backend/session.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/render/wlr_texture.h>
#include <wlr/types/wlr_damage_ring.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/util/box.h>
#include <wlr/util/log.h>

#include <hrt/hrt_lispbsd.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_server.h>

/* Scene coordinates of the first mirror's corner; each further one is
 * MIRROR_STEP to the right, far from anything in the layout. */
#define MIRROR_X (-4000000)
#define MIRROR_Y (-4000000)
#define MIRROR_STEP 40000

struct hrt_mirror {
    struct hrt_output *source, *target;
    struct wlr_scene_tree *tree;
    struct wlr_scene_rect *background;
    struct wlr_scene_buffer *image;
    int x, y; /* the corner in scene coordinates */
    int slot;
    bool frozen;
    bool dead; /* an output went away: only hrt_mirror_stop is left to do */
    uint64_t frames;

    struct wl_listener source_commit;
    struct wl_listener source_destroy;
    struct wl_listener target_destroy;

    /* hrt_mirror_capture: the next target frame goes to capture_path. */
    char *capture_path;
    struct wl_listener target_commit;
};

static uint32_t used_slots;

static void capture_cancel(struct hrt_mirror *mirror) {
    if (mirror->capture_path) {
        wl_list_remove(&mirror->target_commit.link);
        free(mirror->capture_path);
        mirror->capture_path = nullptr;
    }
}

static void mirror_release(struct hrt_mirror *mirror) {
    if (mirror->dead) {
        return;
    }
    mirror->dead = true;
    capture_cancel(mirror);
    wl_list_remove(&mirror->source_commit.link);
    wl_list_remove(&mirror->source_destroy.link);
    wl_list_remove(&mirror->target_destroy.link);
    wlr_scene_node_destroy(&mirror->tree->node);
    wlr_output_lock_software_cursors(mirror->source->wlr_output, false);
    mirror->target->mirror_target = false;
    used_slots &= ~(1u << mirror->slot);
}

static void handle_source_commit(struct wl_listener *listener, void *data) {
    struct hrt_mirror *mirror = wl_container_of(listener, mirror, source_commit);
    const struct wlr_output_event_commit *event = data;

    if (mirror->frozen || !(event->state->committed & WLR_OUTPUT_STATE_BUFFER)) {
        return;
    }
    /* The node keeps a lock on the buffer, so the source's swapchain leaves
     * it alone until the next one replaces it here. */
    wlr_scene_buffer_set_buffer(mirror->image, event->state->buffer);
    mirror->frames++;
}

static void handle_output_destroy(struct hrt_mirror *mirror) {
    wlr_log(WLR_INFO, "mirror %s -> %s: an output went away",
            mirror->source->wlr_output->name, mirror->target->wlr_output->name);
    mirror_release(mirror);
}

static void handle_source_destroy(struct wl_listener *listener, void *data) {
    struct hrt_mirror *mirror = wl_container_of(listener, mirror, source_destroy);
    handle_output_destroy(mirror);
}

static void handle_target_destroy(struct wl_listener *listener, void *data) {
    struct hrt_mirror *mirror = wl_container_of(listener, mirror, target_destroy);
    handle_output_destroy(mirror);
}

struct hrt_mirror *hrt_mirror_start(struct hrt_output *source,
                                    struct hrt_output *target) {
    struct hrt_server *server = source->server;

    if (source == target || target->mirror_target || source->mirror_target) {
        wlr_log(WLR_ERROR, "mirror: %s -> %s is not a valid pair",
                source->wlr_output->name, target->wlr_output->name);
        return nullptr;
    }
    int slot = 0;
    while (slot < 32 && (used_slots & (1u << slot))) {
        slot++;
    }
    if (slot == 32) {
        return nullptr;
    }

    struct hrt_mirror *mirror = calloc(1, sizeof(*mirror));
    mirror->source = source;
    mirror->target = target;
    mirror->slot   = slot;
    mirror->x      = MIRROR_X + slot * MIRROR_STEP;
    mirror->y      = MIRROR_Y;
    used_slots |= 1u << slot;

    mirror->tree = wlr_scene_tree_create(&server->scene->tree);
    wlr_scene_node_set_position(&mirror->tree->node, mirror->x, mirror->y);
    const float black[4] = {0, 0, 0, 1};
    mirror->background = wlr_scene_rect_create(mirror->tree, 0, 0, black);
    mirror->image      = wlr_scene_buffer_create(mirror->tree, nullptr);

    /* Out of the layout: the cursor, new windows and layer surfaces never
     * reach it, and finish_configure keeps it out while mirror_target is
     * set.  The scene output stays and is pointed at the mirror. */
    target->mirror_target = true;
    if (wlr_output_layout_get(server->output_layout, target->wlr_output)) {
        wlr_output_layout_remove(server->output_layout, target->wlr_output);
    }
    wlr_scene_output_set_position(target->wlr_scene, mirror->x, mirror->y);

    wlr_output_lock_software_cursors(source->wlr_output, true);

    mirror->source_commit.notify = handle_source_commit;
    wl_signal_add(&source->wlr_output->events.commit, &mirror->source_commit);
    mirror->source_destroy.notify = handle_source_destroy;
    wl_signal_add(&source->wlr_output->events.destroy, &mirror->source_destroy);
    mirror->target_destroy.notify = handle_target_destroy;
    wl_signal_add(&target->wlr_output->events.destroy, &mirror->target_destroy);

    /* Show something before the source's next frame. */
    wlr_output_schedule_frame(source->wlr_output);
    wlr_log(WLR_INFO, "mirror %s -> %s started", source->wlr_output->name,
            target->wlr_output->name);
    return mirror;
}

void hrt_mirror_set_view(struct hrt_mirror *mirror, double src_x, double src_y,
                         double src_width, double src_height, int x, int y,
                         int width, int height, int target_width,
                         int target_height, bool smooth) {
    if (mirror->dead) {
        return;
    }
    wlr_scene_rect_set_size(mirror->background, target_width, target_height);
    struct wlr_fbox box = {
        .x = src_x, .y = src_y, .width = src_width, .height = src_height};
    wlr_scene_buffer_set_source_box(mirror->image, &box);
    wlr_scene_buffer_set_dest_size(mirror->image, width, height);
    wlr_scene_node_set_position(&mirror->image->node, x, y);
    wlr_scene_buffer_set_filter_mode(mirror->image,
                                     smooth ? WLR_SCALE_FILTER_BILINEAR
                                            : WLR_SCALE_FILTER_NEAREST);
}

void hrt_mirror_set_frozen(struct hrt_mirror *mirror, bool frozen) {
    mirror->frozen = frozen;
    if (!frozen && !mirror->dead) {
        wlr_output_schedule_frame(mirror->source->wlr_output);
    }
}

bool hrt_mirror_alive(struct hrt_mirror *mirror) {
    return !mirror->dead;
}

uint64_t hrt_mirror_frames(struct hrt_mirror *mirror) {
    return mirror->frames;
}

void hrt_mirror_stop(struct hrt_mirror *mirror, bool restore) {
    if (mirror->dead) {
        restore = false;
    }
    if (!restore) {
        mirror_release(mirror);
    } else {
        struct hrt_output *target = mirror->target;
        struct hrt_server *server = target->server;
        mirror_release(mirror);
        /* Back into the layout where configuration would have put it. */
        struct wlr_output_layout_output *l_output =
            wlr_output_layout_add_auto(server->output_layout, target->wlr_output);
        if (l_output) {
            wlr_scene_output_layout_add_output(server->scene_layout, l_output,
                                               target->wlr_scene);
        }
        wlr_log(WLR_INFO, "mirror %s -> %s stopped",
                mirror->source->wlr_output->name, target->wlr_output->name);
    }
    free(mirror);
}

/* Write what OUTPUT shows after a commit of STATE to PATH as a PNG
 * (through PATH.part, so a reader polling for PATH never sees half a file).
 * The buffer may be the source's own, scanned out with a source box scaled
 * into a destination box (the display engine scales, nothing is drawn), so
 * the picture is put together the same way here. */
static bool write_png(struct wlr_renderer *renderer, struct wlr_output *output,
                      const struct wlr_output_state *state, const char *path) {
    struct wlr_texture *texture =
        wlr_texture_from_buffer(renderer, state->buffer);
    if (!texture) {
        return false;
    }
    struct wlr_box src = {0, 0, texture->width, texture->height};
    if (!wlr_fbox_empty(&state->buffer_src_box)) {
        src = (struct wlr_box){(int)state->buffer_src_box.x,
                               (int)state->buffer_src_box.y,
                               (int)(state->buffer_src_box.width + 0.5),
                               (int)(state->buffer_src_box.height + 0.5)};
    }
    struct wlr_box dst = {0, 0, output->width, output->height};
    if (!wlr_box_empty(&state->buffer_dst_box)) {
        dst = state->buffer_dst_box;
    }

    int stride = cairo_format_stride_for_width(CAIRO_FORMAT_RGB24, src.width);
    unsigned char *data = malloc((size_t)stride * src.height);
    bool ok = wlr_texture_read_pixels(
        texture, &(struct wlr_texture_read_pixels_options){
                     .data = data, .format = DRM_FORMAT_XRGB8888,
                     .stride = stride, .src_box = src});
    wlr_texture_destroy(texture);
    if (ok) {
        cairo_surface_t *image = cairo_image_surface_create_for_data(
            data, CAIRO_FORMAT_RGB24, src.width, src.height, stride);
        cairo_surface_t *frame = cairo_image_surface_create(
            CAIRO_FORMAT_RGB24, output->width, output->height);
        cairo_t *cr = cairo_create(frame);
        cairo_set_source_rgb(cr, 0, 0, 0);
        cairo_paint(cr);
        cairo_translate(cr, dst.x, dst.y);
        cairo_scale(cr, (double)dst.width / src.width,
                    (double)dst.height / src.height);
        cairo_set_source_surface(cr, image, 0, 0);
        cairo_paint(cr);
        cairo_destroy(cr);

        char part[4096];
        snprintf(part, sizeof(part), "%s.part", path);
        ok = cairo_surface_write_to_png(frame, part) == CAIRO_STATUS_SUCCESS &&
            rename(part, path) == 0;
        cairo_surface_destroy(frame);
        cairo_surface_destroy(image);
    }
    free(data);
    return ok;
}

static void handle_target_commit(struct wl_listener *listener, void *data) {
    struct hrt_mirror *mirror = wl_container_of(listener, mirror, target_commit);
    const struct wlr_output_event_commit *event = data;

    if (!(event->state->committed & WLR_OUTPUT_STATE_BUFFER)) {
        return;
    }
    if (!write_png(mirror->target->server->renderer, mirror->target->wlr_output,
                   event->state, mirror->capture_path)) {
        wlr_log(WLR_ERROR, "mirror: could not capture %s to %s",
                mirror->target->wlr_output->name, mirror->capture_path);
    }
    capture_cancel(mirror);
}

bool hrt_mirror_capture(struct hrt_mirror *mirror, const char *path) {
    if (mirror->dead) {
        return false;
    }
    capture_cancel(mirror);
    mirror->capture_path         = strdup(path);
    mirror->target_commit.notify = handle_target_commit;
    wl_signal_add(&mirror->target->wlr_output->events.commit,
                  &mirror->target_commit);
    /* Make the target draw a frame even when nothing changed. */
    struct wlr_scene_output *scene_output = mirror->target->wlr_scene;
    wlr_damage_ring_add_whole(&scene_output->damage_ring);
    wlr_output_schedule_frame(mirror->target->wlr_output);
    return true;
}

void hrt_output_pixel_size(struct hrt_output *output, int *width,
                           int *height) {
    *width  = output->wlr_output->width;
    *height = output->wlr_output->height;
}

int hrt_outputs_rescan(struct hrt_server *server) {
    /* NetBSD's drm reports no hotplug to userland (drm_sysfs_hotplug_event
     * is empty), so udev never sends wlroots the "change" that makes it
     * look at the connectors.  Send it here: the DRM backend probes every
     * connector and announces new outputs and lost ones. */
    if (!server->session) {
        return 0;
    }
    int devices = 0;
    struct wlr_device *device, *tmp;
    wl_list_for_each_safe(device, tmp, &server->session->devices, link) {
        struct wlr_device_change_event event = {.type = WLR_DEVICE_HOTPLUG};
        wl_signal_emit_mutable(&device->events.change, &event);
        devices++;
    }
    return devices;
}
