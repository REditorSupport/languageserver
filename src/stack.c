#include <stdlib.h>
#include "stack.h"

// a poor man stack implemention, good enough for our purpose

/*
typedef struct stackitem {
    struct stackitem* prev;
    int x;
} stackitem;

typedef struct {
    stackitem* top;
} stack;
*/

void stack_clear(stack* s) {
    s->top = NULL;
}


void stack_push(stack* s, int x) {
    stackitem* t = s->top;
    stackitem* si = malloc (sizeof(stackitem));
    si->prev = t;
    si->x = x;
    s->top = si;
}

int stack_pop(stack* s) {
    stackitem* t = s->top;
    int ret;
    if (t == NULL) {
        ret = -1;
    } else {
        s->top = t->prev;
        ret = t->x;
        free(t);
    }
    return ret;
}
