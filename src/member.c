#include "member.h"

#include <limits.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    SEXP *nodes;
    size_t size, capacity;
} syntax_stack;

static void syntax_push(syntax_stack *stack, SEXP node) {
    if (stack->size == stack->capacity) {
        size_t capacity = stack->capacity ? stack->capacity * 2 : 64;
        if (capacity < stack->capacity || capacity > SIZE_MAX / sizeof(SEXP)) {
            Rf_error("Member syntax exceeds index capacity");
        }
        SEXP *nodes = (SEXP *) R_alloc(capacity, sizeof(SEXP));
        if (stack->size) memcpy(nodes, stack->nodes, stack->size * sizeof(SEXP));
        stack->nodes = nodes;
        stack->capacity = capacity;
    }
    stack->nodes[stack->size++] = node;
}

/* Walk syntax iteratively, including function defaults but never formal tags
 * or source-reference attributes. No bindings or executable values are read. */
static R_xlen_t syntax_names(SEXP expr, SEXP output, SEXP *seen, size_t mask) {
    syntax_stack stack = {NULL, 0, 0};
    syntax_push(&stack, expr);
    R_xlen_t count = 0;
    size_t visits = 0;
    while (stack.size) {
        SEXP node = stack.nodes[--stack.size];
        if ((++visits & 4095) == 0) R_CheckUserInterrupt();
        switch (TYPEOF(node)) {
        case SYMSXP: {
            if (node == R_MissingArg) break;
            if (output != R_NilValue) {
                size_t slot = ((uintptr_t) node >> 3) & mask;
                while (seen[slot] != R_NilValue && seen[slot] != node) slot = (slot + 1) & mask;
                if (seen[slot] == node) break;
                seen[slot] = node;
                SET_STRING_ELT(output, count, PRINTNAME(node));
            }
            if (count == R_XLEN_T_MAX) Rf_error("Member syntax exceeds index capacity");
            count++;
            break;
        }
        case LANGSXP:
        case LISTSXP:
            syntax_push(&stack, CDR(node));
            syntax_push(&stack, CAR(node));
            break;
        case EXPRSXP:
            for (R_xlen_t i = XLENGTH(node); i > 0; --i) syntax_push(&stack, VECTOR_ELT(node, i - 1));
            break;
        default:
            break;
        }
    }
    return count;
}

SEXP member_syntax_names_c(SEXP expr) {
    /* As with all.names(), ordinary lists and pairlists are not expressions. */
    if (TYPEOF(expr) == VECSXP || TYPEOF(expr) == LISTSXP) return Rf_allocVector(STRSXP, 0);
    R_xlen_t count = syntax_names(expr, R_NilValue, NULL, 0);
    if ((uint64_t) count > SIZE_MAX / (2 * sizeof(SEXP))) Rf_error("Member syntax exceeds index capacity");
    size_t capacity = 1;
    while (capacity < (size_t) count * 2) {
        if (capacity > SIZE_MAX / 2) Rf_error("Member syntax exceeds index capacity");
        capacity *= 2;
    }
    if (capacity > SIZE_MAX / sizeof(SEXP)) Rf_error("Member syntax exceeds index capacity");
    SEXP *seen = (SEXP *) R_alloc(capacity, sizeof(SEXP));
    for (size_t i = 0; i < capacity; ++i) seen[i] = R_NilValue;
    SEXP output = PROTECT(Rf_allocVector(STRSXP, count));
    R_xlen_t unique = syntax_names(expr, output, seen, capacity - 1);
    SEXP result = PROTECT(Rf_allocVector(STRSXP, unique));
    for (R_xlen_t i = 0; i < unique; ++i) SET_STRING_ELT(result, i, STRING_ELT(output, i));
    UNPROTECT(2);
    return result;
}
