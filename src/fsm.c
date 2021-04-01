#include "fsm.h"

/*
typedef struct {
    int single_quoted;
    int double_quoted;
    int backticked;
    int escaped;
    int raw_parse_state;  // 1: R/r; 2: "/'; 3: (
    int raw_dashes;       // number of dashes in raw string
    int raw_dashes_running;  // used to store number of dashes temporarily while parsing
    char raw_string_token;
    int raw_string;
} fsm_state;
*/


void fsm_initialize(fsm_state* s) {
    *s = (fsm_state){0, 0, 0, 0, 0, 0, 0, 0, 0};
}


void fsm_feed(fsm_state* state, const char c) {
    if (state->raw_string == 0) {
        if (state->raw_parse_state == 1) {
            if (c == '\'') {
                state->raw_parse_state = 2;
                state->single_quoted = 1;
                state->raw_dashes_running = 0;
                return;
            } else if (c == '"') {
                state->raw_parse_state = 2;
                state->double_quoted = 1;
                state->raw_dashes_running = 0;
                return;
            } else {
                state->raw_parse_state = 0;
                state->double_quoted = 0;
                state->single_quoted = 0;
            }
        } else if (state->raw_parse_state == 2) {
            if (c == '(' || c == '[' || c == '{') {
                state->raw_parse_state = 3;
                state->raw_dashes = state->raw_dashes_running;
                state->raw_string_token = c;
                state->raw_string = 1;
            } else if (c == '-') {
                state->raw_dashes_running++;
            } else {
                state->raw_parse_state = 0;
                state->double_quoted = 0;
                state->single_quoted = 0;
            }
            return;
        }
    } else {
        if (state->raw_parse_state == 3) {
            if (state->raw_string_token == '(' && c == ')') {
                state->raw_parse_state = 2;
            } else if (state->raw_string_token == '[' && c == ']') {
                state->raw_parse_state = 2;
            } else if (state->raw_string_token == '{' && c == '}') {
                state->raw_parse_state = 2;
            } else {
                // in raw string
            }
        } else if (state->raw_parse_state == 2) {
            if (state->raw_dashes_running == 0) {
                if (state->single_quoted == 1 && c == '\'') {
                    state->single_quoted = 0;
                    state->raw_string = 0;
                    state->raw_parse_state = 0;
                } else if (state->double_quoted == 1 && c == '"') {
                    state->double_quoted = 0;
                    state->raw_string = 0;
                    state->raw_parse_state = 0;
                } else {
                    state->raw_dashes_running = state->raw_dashes;
                    state->raw_parse_state = 3;
                }
            } else if (state->raw_dashes_running > 0 && c == '-') {
                state->raw_dashes_running--;
            } else {
                state->raw_dashes_running = state->raw_dashes;
                state->raw_parse_state = 3;
            }
        }
        return;
    }
    if (state->escaped == 1) {
        state->escaped = 0;
        return;
    }
    if (state->backticked == 1 && c == '`') {
        state->backticked = 0;
        return;
    }
    if (state->single_quoted == 1 && c == '\'') {
        state->single_quoted = 0;
        return;
    }
    if (state->double_quoted == 1 && c == '\"') {
        state->double_quoted = 0;
        return;
    }

    if (c == 'R' || c == 'r') {
        state->raw_parse_state = 1;
    } else if (c == '\\') {
        state->escaped = 1;
    } else if (state->single_quoted || state->double_quoted || state->backticked) {
        // inside string or backticks
    } else if (c == '`') {
        state->backticked = 1;
    } else if (c == '\'') {
        state->single_quoted = 1;
    } else if (c == '"') {
        state->double_quoted = 1;
    }
}
