#include "fsm.h"

/*
typedef struct {
    int single_quoted;
    int doubled_quoted;
    int escaped;
    int paren_level;
} fsm_state;
*/

fsm_state fsm_initialize() {
    fsm_state bs = {0, 0, 0, 0};
    return bs;
}


void fsm_feed(fsm_state* state, const char c) {
    /*
    It is technically not really a finite state machine because it counts, but who cares.
    */
    if (state->escaped == 1) {
        state->escaped = 0;
    } else if (state->single_quoted == 1 && c == '\'') {
        state->single_quoted = 0;
    } else if (state->doubled_quoted == 1 && c == '\"') {
        state->doubled_quoted = 0;
    } else if (c == '\\') {
        state->escaped = 1;
    } else if (c == '\'') {
        state->single_quoted = 1;
    } else if (c == '\"') {
        state->doubled_quoted = 1;
    } else if (state->single_quoted || state->doubled_quoted) {
        // inside string
    } else if (c == '(') {
        state->paren_level += 1;
    } else if (c == ')') {
        state->paren_level -= 1;
    }
}
