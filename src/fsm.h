#ifndef FSM_H__
#define FSM_H__

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


void fsm_initialize(fsm_state* s);
void fsm_feed(fsm_state* state, const char c);


#endif /* end of include guard: FSM_H__ */
