#ifndef STACK_H__
#define STACK_H__

typedef struct stackitem {
    struct stackitem* prev;
    int x;
} stackitem;

typedef struct {
    stackitem* top;
} stack;

void languageserver_stack_initialize(stack* s);

void languageserver_stack_clear(stack* s);

void languageserver_stack_push(stack* s, int x);

int languageserver_stack_pop(stack* s);

#endif /* end of include guard: STACK_H__ */
