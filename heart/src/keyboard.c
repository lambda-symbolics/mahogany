#include <wayland-util.h>
#include <wlr/util/log.h>
#include <stdio.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wayland-server-protocol.h>
#include <xkbcommon/xkbcommon-keysyms.h>
#include <xkbcommon/xkbcommon-names.h>
#include <wlr/types/wlr_seat.h>

#include <wlr/backend/multi.h>
#include <wlr/backend/session.h>
#include <wlr/types/wlr_keyboard.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_input.h>
#include <hrt/hrt_lispbsd.h>

#include "seat_impl.h"

static size_t seat_translate_keysyms(struct hrt_seat *seat,
                                     xkb_keycode_t keycode,
                                     const xkb_keysym_t **keysyms,
                                     uint32_t *modifiers) {
    struct wlr_keyboard *keyboard = &seat->keyboard_group->keyboard;
    *modifiers                    = wlr_keyboard_get_modifiers(keyboard);
    xkb_mod_mask_t consumed       = xkb_state_key_get_consumed_mods2(
        keyboard->xkb_state, keycode, XKB_CONSUMED_MODE_XKB);
    *modifiers = *modifiers & ~consumed;

    return xkb_state_key_get_syms(keyboard->xkb_state, keycode, keysyms);
}

static bool execute_hardcoded_bindings(struct hrt_server *server,
                                       const xkb_keysym_t *pressed_keysyms,
                                       uint32_t modifiers, size_t keysyms_len) {
    for (size_t i = 0; i < keysyms_len; ++i) {
        xkb_keysym_t keysym = pressed_keysyms[i];

        if (keysym >= XKB_KEY_XF86Switch_VT_1 &&
            keysym <= XKB_KEY_XF86Switch_VT_12) {
            if (wlr_backend_is_multi(server->backend)) {
                struct wlr_session *session = server->session;
                if (session) {
                    wlr_log(WLR_DEBUG, "Changing session");
                    unsigned vt = keysym - XKB_KEY_XF86Switch_VT_1 + 1;
                    wlr_session_change_vt(session, vt);
                }
            }
            return true;
        }
    }
    return false;
}

// Keys whose press a binding consumed.  The client never saw them go down,
// so it must not see them come up either, nor be told they are held when
// keyboard focus moves to it: a focus binding (Super+Right) moves focus while
// the arrow is still down, and the new window then got the arrow in its
// enter event and its release, which a video player took for a seek.
// One seat, so this lives here rather than in struct hrt_seat (which
// struct hrt_server embeds, and whose layout the Lisp bindings mirror).
static uint32_t consumed_keys[WLR_KEYBOARD_KEYS_CAP];
static size_t consumed_len;

static void consumed_add(uint32_t key) {
    for (size_t i = 0; i < consumed_len; ++i) {
        if (consumed_keys[i] == key) {
            return;
        }
    }
    if (consumed_len < WLR_KEYBOARD_KEYS_CAP) {
        consumed_keys[consumed_len++] = key;
    }
}

static bool consumed_remove(uint32_t key) {
    for (size_t i = 0; i < consumed_len; ++i) {
        if (consumed_keys[i] == key) {
            consumed_keys[i] = consumed_keys[--consumed_len];
            return true;
        }
    }
    return false;
}

size_t hrt_keyboard_forwarded_keys(struct wlr_keyboard *keyboard,
                                   uint32_t *out) {
    size_t n = 0;
    for (size_t i = 0; i < keyboard->num_keycodes; ++i) {
        uint32_t key = keyboard->keycodes[i];
        bool consumed = false;
        for (size_t j = 0; j < consumed_len; ++j) {
            if (consumed_keys[j] == key) {
                consumed = true;
                break;
            }
        }
        if (!consumed) {
            out[n++] = key;
        }
    }
    return n;
}

static void seat_handle_key(struct wl_listener *listener, void *data) {
    struct hrt_seat *seat = wl_container_of(listener, seat, keyboard_key);
    struct wlr_keyboard_key_event *event = data;
    struct hrt_server *server            = seat->server;

    xkb_keycode_t keycode = event->keycode + 8;
    hrt_idle_notify_activity(seat);

    bool handled = false;

    if (event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
        const xkb_keysym_t *translated_keysyms;
        uint32_t translated_modifiers;
        size_t translated_keysyms_len = seat_translate_keysyms(
            seat, keycode, &translated_keysyms, &translated_modifiers);

        struct hrt_keypress_info key_info = {
            .keysyms      = translated_keysyms,
            .keysyms_len  = translated_keysyms_len,
            .modifiers    = translated_modifiers,
            .wl_key_state = event->state,
        };
        handled = seat->callbacks->keyboard_keypress_event(seat, &key_info);

        if (!handled) {
            handled = execute_hardcoded_bindings(server, translated_keysyms,
                                                 translated_modifiers,
                                                 translated_keysyms_len);
        }
        if (handled) {
            consumed_add(event->keycode);
        }
    } else {
        // The release of a key whose press a binding took.
        handled = consumed_remove(event->keycode);
    }

    if (!handled) {
        wlr_seat_keyboard_notify_key(seat->seat, event->time_msec,
                                     event->keycode, event->state);
    } else {
        wlr_log(WLR_DEBUG, "Keypress handled by keybinding");
    }
}

static void seat_handle_modifiers(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "Keyboard modifier pressed");
    struct hrt_seat *seat = wl_container_of(listener, seat, keyboard_modifiers);

    wlr_seat_keyboard_notify_modifiers(
        seat->seat, &seat->keyboard_group->keyboard.modifiers);
}

static void handle_keyboard_destroy(struct wl_listener *listener, void *data) {
    struct wlr_keyboard *kb = data;
    wlr_log(WLR_DEBUG, "hrt keyboard destroyed %p", kb);
    struct hrt_seat *seat = wl_container_of(listener, seat, destroy.keyboard);

    xkb_context_unref(seat->xkb_context);
    wl_list_remove(&seat->keyboard_key.link);
    wl_list_remove(&seat->keyboard_modifiers.link);
    wl_list_remove(&seat->destroy.keyboard.link);
}

void hrt_keyboard_init(struct hrt_seat *seat) {
    seat->keyboard_group    = wlr_keyboard_group_create();
    struct wlr_keyboard *kb = &seat->keyboard_group->keyboard;

    struct xkb_rule_names rules = {0};
    seat->xkb_context           = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    struct xkb_keymap *keymap   = xkb_map_new_from_names(
        seat->xkb_context, &rules, XKB_KEYMAP_COMPILE_NO_FLAGS);
    wlr_keyboard_set_keymap(kb, keymap);
    xkb_keymap_unref(keymap);

    seat->keyboard_key.notify = seat_handle_key;
    wl_signal_add(&kb->events.key, &seat->keyboard_key);
    seat->keyboard_modifiers.notify = seat_handle_modifiers;
    wl_signal_add(&kb->events.modifiers, &seat->keyboard_modifiers);

    wlr_seat_set_keyboard(seat->seat, kb);

    seat->destroy.keyboard.notify = handle_keyboard_destroy;
    wl_signal_add(&kb->base.events.destroy, &seat->destroy.keyboard);
}

void hrt_keyboard_destroy(struct hrt_seat *seat) {
    wlr_keyboard_group_destroy(seat->keyboard_group);
}
