#include <hrt/hrt_output.h>

void hrt_output_resolution(struct hrt_output *output, int *width, int *height) {
    wlr_output_effective_resolution(output->wlr_output, width, height);
}

void hrt_output_position(struct hrt_output *output, int *x, int *y) {
    struct wlr_output_layout_output *l_output = wlr_output_layout_get(
        output->server->output_layout, output->wlr_output);
    /* LISPBSD: a mirror target is out of the layout. */
    *x = l_output ? l_output->x : 0;
    *y = l_output ? l_output->y : 0;
}

bool hrt_output_enabled(struct hrt_output *output) {
    return output->wlr_output->enabled;
}

char *hrt_output_name(struct hrt_output *output) {
    return output->wlr_output->name;
}

char *hrt_output_make(struct hrt_output *output) {
    return output->wlr_output->make;
}

char *hrt_output_model(struct hrt_output *output) {
    return output->wlr_output->model;
}

char *hrt_output_serial(struct hrt_output *output) {
    return output->wlr_output->serial;
}
