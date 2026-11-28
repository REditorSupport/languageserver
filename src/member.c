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

/* Return the paths to annotated function literals, sharing their common
 * ancestors. R copies only these paths when attaching metadata. A pairlist
 * cursor advances once per child, including for very wide brace blocks. */
typedef struct {
    SEXP node, cursor;
    int slot, next, step;
} hint_frame;

typedef struct {
    int parent, slot, hint;
} hint_step;

static void *hint_grow(void *old, int *capacity, size_t width) {
    if (*capacity > INT_MAX / 2) Rf_error("Too many annotation paths");
    int next = *capacity ? *capacity * 2 : 32;
    void *array = R_alloc(next, width);
    if (*capacity) memcpy(array, old, (size_t) *capacity * width);
    *capacity = next;
    return array;
}

/* First hint whose (row, column) is at or after the target. */
static int hint_position(const int *rows, const int *cols, int n, int row, int col) {
    int lo = 0, hi = n;
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        if (rows[mid] < row || (rows[mid] == row && cols[mid] < col)) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}

SEXP member_hint_paths_c(SEXP expr, SEXP lines, SEXP columns, SEXP offset) {
    int n = LENGTH(lines), used = 0, found = 0, capacity = 0, step_capacity = 0;
    const int *rows = INTEGER(lines), *cols = INTEGER(columns);
    int row_offset = INTEGER(offset)[0], col_offset = INTEGER(offset)[1];
    hint_frame *stack = hint_grow(NULL, &capacity, sizeof(hint_frame));
    hint_step *steps = NULL;
    int depth = 1;
    stack[0] = (hint_frame) {expr, R_NilValue, 0, 0, 0};
    SEXP function = Rf_install("function");
    size_t visits = 0;
    while (depth && found < n) {
        hint_frame *f = &stack[depth - 1];
        if ((++visits & 4095) == 0) R_CheckUserInterrupt();
        if (!f->next && TYPEOF(f->node) == LANGSXP && CAR(f->node) == function) {
            SEXP args = CDR(f->node);
            if (args != R_NilValue && CDR(args) != R_NilValue && CDDR(args) != R_NilValue) {
                SEXP ref = CADDR(args);
                if (TYPEOF(ref) == INTSXP && XLENGTH(ref) >= 8) {
                    const int *p = INTEGER(ref);
                    int row = p[6] + row_offset, col = p[4] + (p[6] == 1 ? col_offset : 0);
                    int i = hint_position(rows, cols, n, row, col);
                    if (i < n && rows[i] == row && cols[i] == col) {
                        int j = depth - 1;
                        while (j > 0 && !stack[j - 1].step) j--;
                        for (; j < depth; j++) {
                            if (used == step_capacity) steps = hint_grow(steps, &step_capacity, sizeof(hint_step));
                            steps[used] = (hint_step) {j ? stack[j - 1].step : 0, stack[j].slot, 0};
                            stack[j].step = ++used;
                        }
                        steps[f->step - 1].hint = i + 1;
                        found++;
                        i++;
                    }
                    int end_row = p[7] + row_offset, end_col = p[5] + (p[7] == 1 ? col_offset : 0);
                    if (i == n || rows[i] > end_row || (rows[i] == end_row && cols[i] > end_col)) {
                        depth--;
                        continue;
                    }
                }
            }
        }
        SEXP child;
        if (TYPEOF(f->node) == EXPRSXP) {
            if (f->next >= LENGTH(f->node)) { depth--; continue; }
            child = VECTOR_ELT(f->node, f->next);
        } else {
            if (f->cursor == R_NilValue) { depth--; continue; }
            child = CAR(f->cursor);
            f->cursor = CDR(f->cursor);
        }
        int slot = ++f->next;
        if (TYPEOF(child) != LANGSXP && TYPEOF(child) != LISTSXP && TYPEOF(child) != EXPRSXP) continue;
        if (depth == capacity) stack = hint_grow(stack, &capacity, sizeof(hint_frame));
        stack[depth++] = (hint_frame) {child, TYPEOF(child) == EXPRSXP ? R_NilValue : child, slot, 0, 0};
    }
    SEXP plan = PROTECT(Rf_allocMatrix(INTSXP, used, 3));
    for (int i = 0; i < used; i++) {
        INTEGER(plan)[i] = steps[i].parent;
        INTEGER(plan)[i + (R_xlen_t) used] = steps[i].slot;
        INTEGER(plan)[i + 2 * (R_xlen_t) used] = steps[i].hint;
    }
    UNPROTECT(1);
    return plan;
}
