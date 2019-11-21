#ifndef FSM_H__
#define FSM_H__

typedef struct {
    int single_quoted;
    int double_quoted;
    int backticked;
    int escaped;
} fsm_state;


void fsm_initialize(fsm_state* s);
void fsm_feed(fsm_state* state, const char c);


#endif /* end of include guard: FSM_H__ */
