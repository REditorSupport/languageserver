#include "fsm.h"

/*
typedef struct {
    int single_quoted;
    int double_quoted;
    int backticked;
    int escaped;
} fsm_state;
*/

fsm_state fsm_initialize() {
    fsm_state bs = {0, 0, 0, 0};
    return bs;
}


void fsm_feed(fsm_state* state, const char c) {
    if (state->escaped == 1) {
        state->escaped = 0;
    } else if (state->backticked == 1 && c == '`') {
        state->backticked = 0;
    } else if (state->single_quoted == 1 && c == '\'') {
        state->single_quoted = 0;
    } else if (state->double_quoted == 1 && c == '\"') {
        state->double_quoted = 0;
    } else if (c == '\\') {
        state->escaped = 1;
    } else if (state->single_quoted || state->double_quoted || state->backticked) {
        // inside string or backticks
    } else if (c == '`') {
        state->backticked = 1;
    } else if (c == '\'') {
        state->single_quoted = 1;
    } else if (c == '\"') {
        state->double_quoted = 1;
    }
}
