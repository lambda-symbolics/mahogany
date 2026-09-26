#ifndef HRT_HRT_LISPBSD_H
#define HRT_HRT_LISPBSD_H

#include <stdbool.h>
#include <stdint.h>
#include <sys/types.h>

struct hrt_server;
struct hrt_output;
struct hrt_view;
struct hrt_seat;
struct hrt_message_theme;

/**
 * Create the idle-inhibit and ext-idle-notify globals. Called once from
 * hrt_server_init.
 **/
bool hrt_lispbsd_init(struct hrt_server *server);

/**
 * The pid of the client that owns the view, from the socket peer
 * credentials. 0 when unknown.
 **/
pid_t hrt_view_pid(struct hrt_view *view);

/**
 * Draw a full-width text bar on the output's top layer. LEFT_MARKUP and
 * RIGHT_MARKUP are pango markup; the right text is right-aligned. The bar is
 * placed at the top of the output, or at the bottom when BOTTOM is true.
 * Calling it again replaces the bar's contents. Returns the bar height in
 * layout pixels, or 0 on failure.
 **/
int hrt_bar_set(struct hrt_output *output, const char *left_markup,
                const char *right_markup, struct hrt_message_theme *theme,
                bool bottom);

/**
 * Remove the bar from the output, if any.
 **/
void hrt_bar_clear(struct hrt_output *output);

/**
 * Turn the output's panel on or off (DPMS). The output stays in the layout,
 * so no frame tree is torn down; rendering simply stops while it is off.
 **/
bool hrt_output_set_power(struct hrt_output *output, bool on);

/**
 * Frames the compositor actually rendered on OUTPUT since it appeared.
 * Stays flat while nothing on screen changes.
 **/
uint64_t hrt_output_frames_rendered(struct hrt_output *output);

/**
 * The refresh rate of the output's current mode in mHz, 0 when none.
 **/
int hrt_output_refresh(struct hrt_output *output);

/**
 * Switch to the mode with the current resolution whose refresh is closest to
 * REFRESH_MHZ (e.g. 30000 or 60000). Returns the refresh of the mode chosen,
 * 0 when the output has no modes or the commit failed.
 **/
int hrt_output_set_refresh(struct hrt_output *output, int refresh_mhz);

/**
 * Number of active zwp_idle_inhibitor_v1 objects held by clients.
 **/
int hrt_idle_inhibitor_count(void);

/**
 * Report user activity to ext-idle-notify clients. Called from the input
 * handlers.
 **/
void hrt_idle_notify_activity(struct hrt_seat *seat);

#endif
