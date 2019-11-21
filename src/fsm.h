#ifndef FSM_H__
#define FSM_H__

typedef struct {
    int single_quoted;
    int doubled_quoted;
    int escaped;
    int paren_level;
} fsm_state;

fsm_state fsm_initialize();

void fsm_feed(fsm_state* state, const char c);


#endif /* end of include guard: FSM_H__ */
