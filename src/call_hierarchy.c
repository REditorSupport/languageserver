#include "call_hierarchy.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
    uint64_t start;
    uint64_t end;
    int index;
    int rank;
} call_interval;

static uint64_t call_position(int line, int col) {
    return ((uint64_t) (unsigned int) line << 32) | (unsigned int) col;
}

static int call_matrix_rows(SEXP matrix) {
    SEXP dimensions = Rf_getAttrib(matrix, R_DimSymbol);
    if (TYPEOF(matrix) != INTSXP || TYPEOF(dimensions) != INTSXP ||
            XLENGTH(dimensions) != 2 || INTEGER(dimensions)[0] < 0 ||
            INTEGER(dimensions)[1] != 4 ||
            XLENGTH(matrix) != (R_xlen_t) INTEGER(dimensions)[0] * 4) {
        Rf_error("invalid call hierarchy ranges");
    }
    return INTEGER(dimensions)[0];
}

static void read_call_ranges(SEXP matrix, int size, call_interval *items,
    double *spans) {
    const int *data = INTEGER(matrix);
    for (int i = 0; i < size; ++i) {
        if ((i & 4095) == 0) R_CheckUserInterrupt();
        int line = data[i], col = data[(R_xlen_t) size + i];
        int end_line = data[(R_xlen_t) size * 2 + i];
        int end_col = data[(R_xlen_t) size * 3 + i];
        if (line < 0 || col < 0 || end_line < 0 || end_col < 0) {
            Rf_error("invalid call hierarchy positions");
        }
        items[i].start = call_position(line, col);
        items[i].end = call_position(end_line, end_col);
        if (items[i].end < items[i].start) {
            Rf_error("invalid call hierarchy positions");
        }
        items[i].index = i;
        items[i].rank = 0;
        if (spans != NULL) {
            /* Preserve the existing provider's span ordering and tie rule. */
            spans[i] = ((double) end_line - line) * 1000000 + end_col - col;
        }
    }
}

static int call_start_compare(const void *left, const void *right) {
    const call_interval *a = left, *b = right;
    if (a->start != b->start) return a->start < b->start ? -1 : 1;
    return a->index < b->index ? -1 : a->index > b->index;
}

static int call_end_compare(const void *left, const void *right) {
    const call_interval *a = left, *b = right;
    if (a->end != b->end) return a->end > b->end ? -1 : 1;
    return a->index < b->index ? -1 : a->index > b->index;
}

static int narrower_call(int left, int right, const double *spans) {
    if (left == 0) return right;
    if (right == 0) return left;
    if (spans[left - 1] != spans[right - 1]) {
        return spans[left - 1] < spans[right - 1] ? left : right;
    }
    return left < right ? left : right;
}

/* Sweep backwards over call ends. Definitions whose ends cover the call
 * are inserted into a tree ordered by start position. A prefix query then
 * finds the narrowest definition also covering the call's start. This is
 * O((calls + definitions) log definitions), including overlapping ranges,
 * and does not assume that incoming calls arrive in source order. */
SEXP call_hierarchy_containers_c(SEXP occurrences, SEXP definitions) {
    int n = call_matrix_rows(occurrences);
    int d = call_matrix_rows(definitions);
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n));
    for (int i = 0; i < n; ++i) INTEGER(result)[i] = 0;
    if (n == 0 || d == 0) {
        UNPROTECT(1);
        return result;
    }
    call_interval *calls = (call_interval *) R_alloc(n, sizeof(call_interval));
    call_interval *defs = (call_interval *) R_alloc(d, sizeof(call_interval));
    double *spans = (double *) R_alloc(d, sizeof(double));
    uint64_t *starts = (uint64_t *) R_alloc(d, sizeof(uint64_t));
    read_call_ranges(occurrences, n, calls, NULL);
    read_call_ranges(definitions, d, defs, spans);
    qsort(defs, d, sizeof(call_interval), call_start_compare);
    for (int i = 0; i < d; ++i) {
        starts[i] = defs[i].start;
        defs[i].rank = i;
    }
    qsort(defs, d, sizeof(call_interval), call_end_compare);
    qsort(calls, n, sizeof(call_interval), call_end_compare);

    size_t base = 1;
    while (base < (size_t) d) base *= 2;
    if (base > SIZE_MAX / (2 * sizeof(int))) Rf_error("call hierarchy index too large");
    int *tree = (int *) R_alloc(base * 2, sizeof(int));
    for (size_t i = 0; i < base * 2; ++i) tree[i] = 0;
    int next = 0;
    for (int i = 0; i < n; ++i) {
        if ((i & 4095) == 0) R_CheckUserInterrupt();
        while (next < d && defs[next].end >= calls[i].end) {
            size_t node = base + defs[next].rank;
            tree[node] = defs[next].index + 1;
            while (node > 1) {
                node /= 2;
                tree[node] = narrower_call(tree[node * 2], tree[node * 2 + 1], spans);
            }
            ++next;
        }
        int low = 0, high = d;
        while (low < high) {
            int middle = low + (high - low) / 2;
            if (starts[middle] <= calls[i].start) low = middle + 1;
            else high = middle;
        }
        int best = 0;
        size_t left = base, right = base + low;
        while (left < right) {
            if (left & 1) best = narrower_call(best, tree[left++], spans);
            if (right & 1) best = narrower_call(best, tree[--right], spans);
            left /= 2;
            right /= 2;
        }
        INTEGER(result)[calls[i].index] = best;
    }
    UNPROTECT(1);
    return result;
}
