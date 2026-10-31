/*
 * LispBSD: computer use.  A virtual pointer and keyboard inside the
 * compositor, driven from Lisp (lisp/computer-use.lisp) so a remote agent
 * can operate the desktop for GUI tests.  The pointer is attached to the
 * seat's cursor and the keyboard joins the seat's keyboard group, so an
 * injected event takes exactly the path a real one does: keybindings, focus
 * on click, sloppy focus, idle notification, then the client.
 */
#include <stdlib.h>
#include <time.h>

#include <wayland-server-core.h>
#include <xkbcommon/xkbcommon.h>
#include <wlr/interfaces/wlr_keyboard.h>
#include <wlr/interfaces/wlr_pointer.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_keyboard_group.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/box.h>
#include <wlr/util/log.h>

#include <hrt/hrt_input.h>
#include <hrt/hrt_lispbsd.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_view.h>

static const struct wlr_pointer_impl cu_pointer_impl = {
    .name = "lispbsd-computer-use-pointer",
};

static const struct wlr_keyboard_impl cu_keyboard_impl = {
    .name = "lispbsd-computer-use-keyboard",
};

static struct wlr_pointer cu_pointer;
static struct wlr_keyboard cu_keyboard;
static struct hrt_seat *cu_seat;

static uint32_t now_msec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

static bool cu_ready(struct hrt_seat *seat) {
    if (cu_seat == seat) {
        return true;
    }
    if (cu_seat != nullptr) {
        wlr_log(WLR_ERROR, "computer use: devices belong to another seat");
        return false;
    }
    wlr_pointer_init(&cu_pointer, &cu_pointer_impl, cu_pointer_impl.name);
    wlr_cursor_attach_input_device(seat->cursor, &cu_pointer.base);

    wlr_keyboard_init(&cu_keyboard, &cu_keyboard_impl, cu_keyboard_impl.name);
    wlr_keyboard_set_keymap(&cu_keyboard,
                            seat->keyboard_group->keyboard.keymap);
    if (!wlr_keyboard_group_add_keyboard(seat->keyboard_group,
                                         &cu_keyboard)) {
        wlr_log(WLR_ERROR, "computer use: keyboard not added to the group");
    }
    // A seat without real devices (headless, nested) advertises none.
    wlr_seat_set_capabilities(seat->seat,
                              seat->seat->capabilities |
                                  WL_SEAT_CAPABILITY_POINTER |
                                  WL_SEAT_CAPABILITY_KEYBOARD);
    cu_seat = seat;
    wlr_log(WLR_INFO, "computer use: virtual pointer and keyboard ready");
    return true;
}

bool hrt_cu_pointer_move(struct hrt_seat *seat, double lx, double ly) {
    if (!cu_ready(seat)) {
        return false;
    }
    struct wlr_box box;
    wlr_output_layout_get_box(seat->server->output_layout, nullptr, &box);
    if (wlr_box_empty(&box)) {
        return false;
    }
    struct wlr_pointer_motion_absolute_event ev = {
        .pointer   = &cu_pointer,
        .time_msec = now_msec(),
        .x         = (lx - box.x) / box.width,
        .y         = (ly - box.y) / box.height,
    };
    wl_signal_emit_mutable(&cu_pointer.events.motion_absolute, &ev);
    wl_signal_emit_mutable(&cu_pointer.events.frame, &cu_pointer);
    return true;
}

bool hrt_cu_pointer_button(struct hrt_seat *seat, uint32_t button,
                           bool pressed) {
    if (!cu_ready(seat)) {
        return false;
    }
    struct wlr_pointer_button_event ev = {
        .pointer   = &cu_pointer,
        .time_msec = now_msec(),
        .button    = button,
        .state     = pressed ? WL_POINTER_BUTTON_STATE_PRESSED
                             : WL_POINTER_BUTTON_STATE_RELEASED,
    };
    wl_signal_emit_mutable(&cu_pointer.events.button, &ev);
    wl_signal_emit_mutable(&cu_pointer.events.frame, &cu_pointer);
    return true;
}

bool hrt_cu_pointer_scroll(struct hrt_seat *seat, bool horizontal,
                           int clicks) {
    if (!cu_ready(seat)) {
        return false;
    }
    // One wheel click as libinput reports it: 15 units, 120 in v120.
    struct wlr_pointer_axis_event ev = {
        .pointer            = &cu_pointer,
        .time_msec          = now_msec(),
        .source             = WL_POINTER_AXIS_SOURCE_WHEEL,
        .orientation        = horizontal ? WL_POINTER_AXIS_HORIZONTAL_SCROLL
                                         : WL_POINTER_AXIS_VERTICAL_SCROLL,
        .relative_direction = WL_POINTER_AXIS_RELATIVE_DIRECTION_IDENTICAL,
        .delta              = 15.0 * clicks,
        .delta_discrete     = 120 * clicks,
    };
    wl_signal_emit_mutable(&cu_pointer.events.axis, &ev);
    wl_signal_emit_mutable(&cu_pointer.events.frame, &cu_pointer);
    return true;
}

bool hrt_cu_key(struct hrt_seat *seat, uint32_t key, bool pressed) {
    if (!cu_ready(seat)) {
        return false;
    }
    struct wlr_keyboard_key_event ev = {
        .time_msec    = now_msec(),
        .keycode      = key,
        .update_state = true,
        .state        = pressed ? WL_KEYBOARD_KEY_STATE_PRESSED
                                : WL_KEYBOARD_KEY_STATE_RELEASED,
    };
    wlr_keyboard_notify_key(&cu_keyboard, &ev);
    return true;
}

int hrt_cu_keysym_key(struct hrt_seat *seat, uint32_t keysym,
                      uint32_t *key) {
    struct xkb_keymap *keymap = seat->keyboard_group->keyboard.keymap;
    if (keymap == nullptr) {
        return -1;
    }
    xkb_keycode_t min = xkb_keymap_min_keycode(keymap);
    xkb_keycode_t max = xkb_keymap_max_keycode(keymap);
    // Level 0 anywhere beats level 1 (shifted) anywhere.
    for (int level = 0; level < 2; level++) {
        for (xkb_keycode_t kc = min; kc <= max; kc++) {
            const xkb_keysym_t *syms;
            int n = xkb_keymap_key_get_syms_by_level(keymap, kc, 0, level,
                                                     &syms);
            for (int i = 0; i < n; i++) {
                if (syms[i] == keysym) {
                    *key = kc - 8;
                    return level;
                }
            }
        }
    }
    return -1;
}

uint32_t hrt_cu_char_keysym(uint32_t codepoint) {
    return xkb_utf32_to_keysym(codepoint);
}

uint32_t hrt_cu_keysym_from_name(const char *name) {
    xkb_keysym_t sym = xkb_keysym_from_name(name, XKB_KEYSYM_NO_FLAGS);
    if (sym == XKB_KEY_NoSymbol) {
        sym = xkb_keysym_from_name(name, XKB_KEYSYM_CASE_INSENSITIVE);
    }
    return sym;
}

bool hrt_view_geometry(struct hrt_view *view, int *x, int *y, int *width,
                       int *height) {
    // The client's surface tree: the view's tree also holds the border.
    struct wlr_scene_tree *tree =
        view->xdg_scene != nullptr ? view->xdg_scene : view->scene_tree;
    *width  = view->width;
    *height = view->height;
    return wlr_scene_node_coords(&tree->node, x, y);
}

const char *hrt_view_app_id(struct hrt_view *view) {
    if (view->xdg_toplevel == nullptr) {
        return nullptr;
    }
    return view->xdg_toplevel->app_id;
}

void hrt_layout_box(struct hrt_server *server, int *x, int *y, int *width,
                    int *height) {
    struct wlr_box box;
    wlr_output_layout_get_box(server->output_layout, nullptr, &box);
    *x      = box.x;
    *y      = box.y;
    *width  = box.width;
    *height = box.height;
}
