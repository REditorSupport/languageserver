#include "navigation.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

static int position_before(int line, int col, int other_line, int other_col) {
    return line < other_line || (line == other_line && col < other_col);
}

/* Terminal tokens are ordered and do not overlap, except that a cursor at
 * their shared boundary belongs to both tokens. Search by token end so the
 * first token retains precedence, with the established string exception. */
SEXP navigation_find_token_c(
    SEXP line1, SEXP col1, SEXP line2, SEXP col2,
    SEXP is_string, SEXP point
) {
    R_xlen_t size = XLENGTH(line1);
    if (size > INT_MAX || !Rf_isInteger(line1) || !Rf_isInteger(col1) ||
            !Rf_isInteger(line2) || !Rf_isInteger(col2) ||
            !Rf_isLogical(is_string) || !Rf_isInteger(point) ||
            XLENGTH(col1) != size || XLENGTH(line2) != size ||
            XLENGTH(col2) != size || XLENGTH(is_string) != size ||
            XLENGTH(point) != 2) {
        Rf_error("invalid navigation index");
    }
    int line = INTEGER(point)[0];
    int col = INTEGER(point)[1];
    if (line == NA_INTEGER || col == NA_INTEGER || col < 1) {
        return Rf_ScalarInteger(0);
    }
    int low = 0, high = (int) size;
    while (low < high) {
        int middle = low + (high - low) / 2;
        if (position_before(INTEGER(line2)[middle], INTEGER(col2)[middle],
                line, col - 1)) {
            low = middle + 1;
        } else {
            high = middle;
        }
    }
    int first = -1;
    for (int i = low; i < size; ++i) {
        if (position_before(line, col, INTEGER(line1)[i], INTEGER(col1)[i])) {
            break;
        }
        if (first < 0) first = i;
        if (LOGICAL(is_string)[i] == TRUE) return Rf_ScalarInteger(i + 1);
    }
    return Rf_ScalarInteger(first + 1);
}

typedef struct {
    int group;
    int index;
    uint64_t start;
    uint64_t end;
} reference_interval;

static uint64_t source_position(int line, int col) {
    return ((uint64_t) (unsigned int) line << 32) | (unsigned int) col;
}

static int interval_compare(const void *left, const void *right) {
    const reference_interval *a = left;
    const reference_interval *b = right;
    if (a->group != b->group) return a->group < b->group ? -1 : 1;
    if (a->start != b->start) return a->start < b->start ? -1 : 1;
    /* At equal starts, the narrower scope takes precedence. */
    if (a->end != b->end) return a->end > b->end ? -1 : 1;
    return a->index < b->index ? -1 : a->index > b->index;
}

static void heap_push(int *heap, int size, int value) {
    while (size > 0) {
        int parent = (size - 1) / 2;
        if (heap[parent] >= value) break;
        heap[size] = heap[parent];
        size = parent;
    }
    heap[size] = value;
}

static void heap_pop(int *heap, int size) {
    int value = heap[size];
    int parent = 0;
    while (parent < size / 2) {
        int child = parent * 2 + 1;
        if (child + 1 < size && heap[child + 1] > heap[child]) ++child;
        if (value >= heap[child]) break;
        heap[parent] = heap[child];
        parent = child;
    }
    heap[parent] = value;
}

/* Resolve all occurrences with a sweep over scopes grouped by symbol. A
 * max-heap keeps the last-starting live definition; expired nested scopes
 * are discarded lazily. This avoids rescanning every name and walking
 * backwards through all earlier, already-ended sibling scopes. */
SEXP reference_resolve_local_c(
    SEXP occurrence_group, SEXP occurrence_line, SEXP occurrence_col,
    SEXP definition_group, SEXP line1, SEXP col1, SEXP line2, SEXP col2
) {
    R_xlen_t n = XLENGTH(occurrence_group);
    R_xlen_t d = XLENGTH(definition_group);
    if (n > INT_MAX || d > INT_MAX ||
            !Rf_isInteger(occurrence_group) || !Rf_isInteger(occurrence_line) ||
            !Rf_isInteger(occurrence_col) || !Rf_isInteger(definition_group) ||
            !Rf_isInteger(line1) || !Rf_isInteger(col1) ||
            !Rf_isInteger(line2) || !Rf_isInteger(col2) ||
            XLENGTH(occurrence_line) != n || XLENGTH(occurrence_col) != n ||
            XLENGTH(line1) != d || XLENGTH(col1) != d ||
            XLENGTH(line2) != d || XLENGTH(col2) != d) {
        Rf_error("invalid reference scope index");
    }
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n));
    reference_interval *occurrences = (reference_interval *) R_alloc(
        n, sizeof(reference_interval));
    reference_interval *definitions = (reference_interval *) R_alloc(
        d, sizeof(reference_interval));
    int *heap = (int *) R_alloc(d, sizeof(int));
    int n_occurrences = 0, n_definitions = 0;
    for (int i = 0; i < n; ++i) {
        INTEGER(result)[i] = 0;
        int group = INTEGER(occurrence_group)[i];
        if (group <= 0) continue;
        reference_interval *item = &occurrences[n_occurrences++];
        item->group = group;
        item->index = i;
        item->start = source_position(
            INTEGER(occurrence_line)[i], INTEGER(occurrence_col)[i]);
        item->end = item->start;
    }
    for (int i = 0; i < d; ++i) {
        int group = INTEGER(definition_group)[i];
        if (group <= 0) continue;
        reference_interval *item = &definitions[n_definitions++];
        item->group = group;
        item->index = i;
        item->start = source_position(INTEGER(line1)[i], INTEGER(col1)[i]);
        item->end = source_position(INTEGER(line2)[i], INTEGER(col2)[i]);
    }
    qsort(occurrences, n_occurrences, sizeof(reference_interval), interval_compare);
    qsort(definitions, n_definitions, sizeof(reference_interval), interval_compare);
    int next = 0, heap_size = 0, group = 0;
    for (int i = 0; i < n_occurrences; ++i) {
        if ((i & 4095) == 0) R_CheckUserInterrupt();
        reference_interval *occurrence = &occurrences[i];
        if (group != occurrence->group) {
            group = occurrence->group;
            heap_size = 0;
            while (next < n_definitions && definitions[next].group < group) ++next;
        }
        while (next < n_definitions && definitions[next].group == group &&
                definitions[next].start <= occurrence->start) {
            heap_push(heap, heap_size++, next++);
        }
        while (heap_size > 0 && definitions[heap[0]].end < occurrence->start) {
            heap_pop(heap, --heap_size);
        }
        if (heap_size > 0) {
            INTEGER(result)[occurrence->index] = definitions[heap[0]].index + 1;
        }
    }
    UNPROTECT(1);
    return result;
}
