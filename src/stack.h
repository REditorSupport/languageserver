#ifndef STACK_H__
#define STACK_H__

typedef struct stackitem {
    struct stackitem* prev;
    int x;
} stackitem;

typedef struct {
    stackitem* top;
} stack;

void stack_clear(stack* s);

void stack_push(stack* s, int x);

int stack_pop(stack* s);

#endif /* end of include guard: STACK_H__ */
