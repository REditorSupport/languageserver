#include "member.h"

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* Session-local lookup only. The cache compares body/formals on every hit,
 * so address reuse cannot identify a function with different syntax. */
SEXP member_function_key_c(SEXP fn) {
    if (TYPEOF(fn) != CLOSXP) Rf_error("Expected a closure");
    char key[2 + 2 * sizeof(void *) + 1];
    snprintf(key, sizeof(key), "%p", (void *) fn);
    return Rf_mkString(key);
}

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
    /* Type dependencies are already deduplicated on annotated ASTs. Keep the
     * common unannotated call on the native path without a second R walk. */
    static SEXP type_key = NULL;
    if (!type_key) type_key = Rf_install("member_type_names");
    SEXP types = Rf_getAttrib(expr, type_key);
    if (types == R_NilValue && TYPEOF(expr) == EXPRSXP && XLENGTH(expr) == 1)
        types = Rf_getAttrib(VECTOR_ELT(expr, 0), type_key);
    R_xlen_t extra = 0;
    SEXP positions = R_NilValue;
    if (TYPEOF(types) == STRSXP) {
        /* output's unused slots are empty strings; all type names are nonempty. */
        positions = PROTECT(Rf_match(output, types, 0));
        for (R_xlen_t i = 0; i < XLENGTH(types); ++i) if (!INTEGER(positions)[i]) extra++;
    }
    if (extra > R_XLEN_T_MAX - unique) Rf_error("Member syntax exceeds index capacity");
    SEXP result = PROTECT(Rf_allocVector(STRSXP, unique + extra));
    for (R_xlen_t i = 0; i < unique; ++i) SET_STRING_ELT(result, i, STRING_ELT(output, i));
    if (extra) for (R_xlen_t i = 0; i < XLENGTH(types); ++i) {
        if (!INTEGER(positions)[i]) SET_STRING_ELT(result, unique++, STRING_ELT(types, i));
    }
    UNPROTECT(positions == R_NilValue ? 2 : 3);
    return result;
}
