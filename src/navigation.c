#include "navigation.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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
    for (int i = 0; i < n; ++i) INTEGER(result)[i] = 0;
    if (n == 0 || d == 0) {
        UNPROTECT(1);
        return result;
    }
    reference_interval *occurrences = (reference_interval *) R_alloc(
        n, sizeof(reference_interval));
    reference_interval *definitions = (reference_interval *) R_alloc(
        d, sizeof(reference_interval));
    int *heap = (int *) R_alloc(d, sizeof(int));
    int n_occurrences = 0, n_definitions = 0;
    for (int i = 0; i < n; ++i) {
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
    if (n_occurrences == 0 || n_definitions == 0) {
        UNPROTECT(1);
        return result;
    }
    if (n_occurrences > 1) {
        qsort(occurrences, n_occurrences, sizeof(reference_interval), interval_compare);
    }
    if (n_definitions > 1) {
        qsort(definitions, n_definitions, sizeof(reference_interval), interval_compare);
    }
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

/* All indexes crossing the parse-worker boundary contain ordinary vectors.
 * Row adjacency follows XML's source nesting, including detached comments. */
static SEXP navigation_column(SEXP data, const char *name, SEXPTYPE type) {
    SEXP names = Rf_getAttrib(data, R_NamesSymbol);
    if (TYPEOF(data) != VECSXP || TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(data)) Rf_error("invalid syntax index");
    for (R_xlen_t i = 0; i < XLENGTH(names); ++i) {
        if (!strcmp(CHAR(STRING_ELT(names, i)), name)) {
            SEXP value = VECTOR_ELT(data, i);
            if (TYPEOF(value) != type) Rf_error("invalid syntax column: %s", name);
            return value;
        }
    }
    Rf_error("missing syntax column: %s", name);
    return R_NilValue;
}

static const int *navigation_integer_rows(SEXP data, const char *name, R_xlen_t size) {
    SEXP value = navigation_column(data, name, INTSXP);
    if (XLENGTH(value) != size) Rf_error("invalid syntax column length: %s", name);
    return INTEGER(value);
}

static int navigation_row(int row, int n) {
    if (row <= 0 || row > n) Rf_error("invalid navigation row");
    return row - 1;
}

static int syntax_named(SEXP tokens, int row, const char *name) {
    return STRING_ELT(tokens, row) != NA_STRING && !strcmp(CHAR(STRING_ELT(tokens, row)), name);
}

static int syntax_expr(SEXP tokens, int row) {
    return syntax_named(tokens, row, "expr") || syntax_named(tokens, row, "expr_or_assign_or_help");
}

static int syntax_function(SEXP tokens, int row) {
    return syntax_named(tokens, row, "FUNCTION") || syntax_named(tokens, row, "'\\\\'") ||
        syntax_named(tokens, row, "OP-LAMBDA");
}

static SEXP named_integer_columns(const char **names, int count, int n) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
    SEXP labels = PROTECT(Rf_allocVector(STRSXP, count));
    for (int i = 0; i < count; ++i) {
        SET_STRING_ELT(labels, i, Rf_mkChar(names[i]));
        SET_VECTOR_ELT(result, i, Rf_allocVector(INTSXP, n));
        memset(INTEGER(VECTOR_ELT(result, i)), 0, (size_t) n * sizeof(int));
    }
    Rf_setAttrib(result, R_NamesSymbol, labels);
    UNPROTECT(2);
    return result;
}

static SEXP navigation_rows(const int *rows, int n) {
    SEXP result = Rf_allocVector(INTSXP, n);
    if (n) memcpy(INTEGER(result), rows, (size_t) n * sizeof(int));
    return result;
}

SEXP syntax_index_c(SEXP data, SEXP order) {
    SEXP tokens = navigation_column(data, "token", STRSXP);
    SEXP terminal = navigation_column(data, "terminal", LGLSXP);
    SEXP line1 = navigation_column(data, "line1", INTSXP);
    SEXP col1 = navigation_column(data, "col1", INTSXP);
    SEXP line2 = navigation_column(data, "line2", INTSXP);
    SEXP col2 = navigation_column(data, "col2", INTSXP);
    SEXP ids_column = navigation_column(data, "id", INTSXP);
    SEXP parents_column = navigation_column(data, "parent", INTSXP);
    R_xlen_t size = XLENGTH(tokens);
    if (size > INT_MAX || XLENGTH(ids_column) != size || XLENGTH(parents_column) != size || TYPEOF(order) != INTSXP || XLENGTH(order) != size ||
            XLENGTH(terminal) != size || XLENGTH(line1) != size || XLENGTH(col1) != size ||
            XLENGTH(line2) != size || XLENGTH(col2) != size) Rf_error("invalid syntax data lengths");
    int n = (int) size;
    for (int i = 0; i < n; ++i) {
        if (LOGICAL(terminal)[i] == NA_LOGICAL || INTEGER(line1)[i] < 1 || INTEGER(line2)[i] < 1 ||
                INTEGER(col1)[i] < 1 || INTEGER(col2)[i] < 1 ||
                position_before(INTEGER(line2)[i], INTEGER(col2)[i], INTEGER(line1)[i], INTEGER(col1)[i])) Rf_error("invalid syntax range");
    }
    const char *names[] = {"parent_row", "first_child", "next_sibling", "order_rank",
        "function_scope", "top", "function_close", "order", "roots", "terminals",
        "child_start", "child_end", "children", "max_depth", "id_row", "id_first", "id_next", "id_function_scope", "scope_row"};
    SEXP result = PROTECT(named_integer_columns(names, 19, n));
    int *parent = INTEGER(VECTOR_ELT(result, 0));
    int *first = INTEGER(VECTOR_ELT(result, 1));
    int *next = INTEGER(VECTOR_ELT(result, 2));
    int *rank = INTEGER(VECTOR_ELT(result, 3));
    int *scope = INTEGER(VECTOR_ELT(result, 4));
    int *top = INTEGER(VECTOR_ELT(result, 5));
    int *closing = INTEGER(VECTOR_ELT(result, 6));
    int *stack = (int *) R_alloc((size_t) n + 1, sizeof(int));
    int *last = (int *) R_alloc((size_t) n + 1, sizeof(int));
    int *roots = (int *) R_alloc((size_t) n, sizeof(int));
    int *terminals = (int *) R_alloc((size_t) n, sizeof(int));
    int *function = (int *) R_alloc((size_t) n, sizeof(int));
    memset(last, 0, ((size_t) n + 1) * sizeof(int));
    memset(function, 0, (size_t) n * sizeof(int));
    int depth = 0, root_n = 0, terminal_n = 0, max_depth = 0;
    for (int at = 0; at < n; ++at) {
        if ((at & 16383) == 0) R_CheckUserInterrupt();
        int row = navigation_row(INTEGER(order)[at], n);
        if (rank[row]) Rf_error("invalid syntax order");
        rank[row] = at + 1;
        while (depth > 0) {
            int enclosing = stack[depth - 1];
            if (!position_before(INTEGER(line2)[enclosing], INTEGER(col2)[enclosing],
                    INTEGER(line1)[row], INTEGER(col1)[row])) break;
            --depth;
        }
        int ancestor = depth ? stack[depth - 1] + 1 : 0;
        parent[row] = ancestor;
        if (ancestor) {
            if (!first[ancestor - 1]) first[ancestor - 1] = row + 1;
        } else roots[root_n++] = row + 1;
        if (last[ancestor]) next[last[ancestor] - 1] = row + 1;
        last[ancestor] = row + 1;
        if (syntax_function(tokens, row) && ancestor) function[ancestor - 1] = 1;
        if ((syntax_named(tokens, row, "')'") || syntax_named(tokens, row, "OP-RIGHT-PAREN")) &&
                ancestor && function[ancestor - 1] && !closing[ancestor - 1]) {
            closing[ancestor - 1] = row + 1;
        }
        if (LOGICAL(terminal)[row]) terminals[terminal_n++] = row + 1;
        else {stack[depth++] = row; if (depth > max_depth) max_depth = depth;}
    }
    for (int at = 0; at < n; ++at) {
        int row = INTEGER(order)[at] - 1, ancestor = parent[row];
        if (ancestor) {
            scope[row] = function[ancestor - 1] ? ancestor : scope[ancestor - 1];
            top[row] = top[ancestor - 1];
        } else top[row] = syntax_expr(tokens, row) ? row + 1 : 0;
    }
    SET_VECTOR_ELT(result, 7, order);
    SET_VECTOR_ELT(result, 8, navigation_rows(roots, root_n));
    SET_VECTOR_ELT(result, 9, navigation_rows(terminals, terminal_n));
    int *starts = INTEGER(VECTOR_ELT(result, 10));
    int *ends = INTEGER(VECTOR_ELT(result, 11));
    int *children = INTEGER(VECTOR_ELT(result, 12)), child_n = 0;
    for (int row = 0; row < n; ++row) {
        starts[row] = child_n + 1;
        for (int child = first[row]; child; child = next[child-1]) children[child_n++] = child;
        ends[row] = child_n;
    }
    SET_VECTOR_ELT(result, 12, navigation_rows(children, child_n));
    SET_VECTOR_ELT(result, 13, Rf_ScalarInteger(max_depth));
    const int *ids = INTEGER(ids_column);
    const int *parents = INTEGER(parents_column);
    int max_id = 0;
    for (int i = 0; i < n; ++i) {
        if (ids[i] < 0 || ids[i] == INT_MAX || parents[i] == INT_MAX) Rf_error("invalid syntax id");
        if (ids[i] > max_id) max_id = ids[i];
        if (parents[i] > max_id) max_id = parents[i];
    }
    for (int k = 14; k <= 17; ++k) {
        SET_VECTOR_ELT(result, k, Rf_allocVector(INTSXP, k == 16 ? n : (R_xlen_t) max_id+1));
    }
    int *id_row = INTEGER(VECTOR_ELT(result, 14));
    int *id_first = INTEGER(VECTOR_ELT(result, 15));
    int *id_next = INTEGER(VECTOR_ELT(result, 16));
    int *id_scope = INTEGER(VECTOR_ELT(result, 17));
    int *id_last = (int *) R_alloc((size_t) max_id+1, sizeof(int));
    for (int i = 0; i <= max_id; ++i) {id_row[i] = id_first[i] = id_last[i] = -1; id_scope[i] = 0;}
    for (int i = 0; i < n; ++i) {
        id_next[i] = -1;
        if (id_row[ids[i]] >= 0) Rf_error("duplicate syntax id");
        id_row[ids[i]] = i;
        if (parents[i] >= 0) {
            if (id_first[parents[i]] < 0) id_first[parents[i]] = i;
            else id_next[id_last[parents[i]]] = i;
            id_last[parents[i]] = i;
            if (syntax_function(tokens, i)) id_scope[parents[i]] = 1;
        }
    }
    int *scope_rows = INTEGER(VECTOR_ELT(result, 18));
    int *pending_scope = (int *) R_alloc((size_t) n, sizeof(int));
    for (int i = 0; i < n; ++i) scope_rows[i] = -2;
    for (int i = 0; i < n; ++i) {
        if (scope_rows[i] != -2) continue;
        int row = i, pending_n = 0;
        while (scope_rows[row] == -2) {
            int ancestor = parents[row] > 0 ? id_row[parents[row]] : -1;
            if (ancestor < 0) {scope_rows[row] = parents[row] == 0 ? row : -1; break;}
            if (id_scope[ids[ancestor]]) {scope_rows[row] = ancestor; break;}
            if (pending_n == n) Rf_error("cyclic syntax parents");
            pending_scope[pending_n++] = row;
            row = ancestor;
        }
        while (pending_n) scope_rows[pending_scope[--pending_n]] = scope_rows[row];
    }

    UNPROTECT(1);
    return result;
}

SEXP navigation_index_c(SEXP syntax) {
    SEXP tokens = navigation_column(syntax, "token", STRSXP);
    if (XLENGTH(tokens) > INT_MAX) Rf_error("syntax index is too large");
    int n = (int) XLENGTH(tokens);
    const int *first = navigation_integer_rows(syntax, "first_child", n);
    const int *next = navigation_integer_rows(syntax, "next_sibling", n);
    const int *parent = navigation_integer_rows(syntax, "parent_row", n);
    const int *close = navigation_integer_rows(syntax, "function_close", n);
    const int *order = navigation_integer_rows(syntax, "order", n);
    const int *rank = navigation_integer_rows(syntax, "order_rank", n);
    for (int i = 0; i < n; ++i) {
        if (first[i] < 0 || first[i] > n || next[i] < 0 || next[i] > n ||
                parent[i] < 0 || parent[i] > n || close[i] < 0 || close[i] > n ||
                rank[i] <= 0 || rank[i] > n) Rf_error("invalid navigation adjacency");
        int row = navigation_row(order[i], n);
        if (rank[row] != i + 1) Rf_error("invalid navigation order");
        if (next[i] && (parent[next[i]-1] != parent[i] || rank[next[i]-1] <= rank[i])) Rf_error("invalid navigation sibling");
        if (first[i] && (parent[first[i]-1] != i+1 || rank[first[i]-1] <= rank[i])) Rf_error("invalid navigation child");
        if (parent[i] && rank[parent[i]-1] >= rank[i]) Rf_error("invalid navigation parent");
    }
    int *node = (int *) R_alloc((size_t) n, sizeof(int));
    int *name = (int *) R_alloc((size_t) n, sizeof(int));
    int *value = (int *) R_alloc((size_t) n, sizeof(int));
    int *fun = (int *) R_alloc((size_t) n, sizeof(int));
    int *scope = (int *) R_alloc((size_t) n, sizeof(int));
    int *scope_name = (int *) R_alloc((size_t) n, sizeof(int));
    int *scope_hover = (int *) R_alloc((size_t) n, sizeof(int));
    int count = 0, scope_n = 0;
    for (int at = 0; at < n; ++at) {
        int row = order[at] - 1;
        if ((at & 16383) == 0) R_CheckUserInterrupt();
        int left = syntax_named(tokens, row, "LEFT_ASSIGN") || syntax_named(tokens, row, "EQ_ASSIGN");
        int right = syntax_named(tokens, row, "RIGHT_ASSIGN");
        if ((left || right) && parent[row]) {
            int container = parent[row] - 1, before = 0, after = 0, passed = 0;
            for (int child = first[container]; child; child = next[child - 1]) {
                if (child == row + 1) {passed = 1; continue;}
                if (!syntax_named(tokens, child - 1, "expr")) continue;
                if (passed && !after) after = child;
                if (!passed) before = child;
            }
            int name_expr = left ? before : after;
            int value_expr = left ? after : before;
            int symbol = name_expr ? first[name_expr - 1] : 0;
            if (symbol && !next[symbol - 1] && syntax_named(tokens, symbol - 1, "SYMBOL") && value_expr) {
                node[count] = container + 1;
                name[count] = symbol;
                value[count] = value_expr;
                fun[count] = 0;
                if (left) for (int child = first[value_expr - 1]; child; child = next[child - 1]) {
                    if (syntax_function(tokens, child - 1)) {fun[count] = value_expr; break;}
                }
                ++count;
            }
        }
        if (syntax_named(tokens, row, "SYMBOL_FORMALS") && parent[row] && close[parent[row]-1]) {
            int container = parent[row] - 1;
            scope[scope_n] = container + 1;
            scope_name[scope_n] = row + 1;
            scope_hover[scope_n++] = container + 1;
        }
        if (syntax_named(tokens, row, "SYMBOL") && parent[row] &&
                syntax_named(tokens, parent[row] - 1, "forcond") && parent[parent[row] - 1]) {
            scope[scope_n] = parent[parent[row] - 1];
            scope_name[scope_n] = row + 1;
            scope_hover[scope_n++] = row + 1;
        }
    }
    /* XML visits the outer assignment before a chained inner assignment,
     * while assignment operators themselves are visited in reverse order. */
    reference_interval *sorted = (reference_interval *) R_alloc((size_t) count, sizeof(reference_interval));
    for (int i = 0; i < count; ++i) {sorted[i].group = 0; sorted[i].index = i; sorted[i].start = rank[node[i]-1]; sorted[i].end = 0;}
    if (count > 1) qsort(sorted, count, sizeof(reference_interval), interval_compare);
    const char *names[] = {"node", "name", "value", "function", "scope", "scope_name", "scope_hover"};
    SEXP result = PROTECT(named_integer_columns(names, 7, count));
    int *columns[] = {node, name, value, fun};
    for (int k = 0; k < 4; ++k) for (int i = 0; i < count; ++i) {
        INTEGER(VECTOR_ELT(result, k))[i] = columns[k][sorted[i].index];
    }
    SET_VECTOR_ELT(result, 4, navigation_rows(scope, scope_n));
    SET_VECTOR_ELT(result, 5, navigation_rows(scope_name, scope_n));
    SET_VECTOR_ELT(result, 6, navigation_rows(scope_hover, scope_n));
    UNPROTECT(1);
    return result;
}

static int syntax_contains(int row, int line, int col,
        const int *line1, const int *col1, const int *line2, const int *col2) {
    return !position_before(line, col, line1[row], col1[row]) &&
        !position_before(line2[row], col2[row], line, col - 1);
}

/* Only containing child branches are visited. Sibling functions and large
 * statement lists are skipped with binary search over their source ranges. */
SEXP syntax_enclosing_c(SEXP syntax, SEXP point) {
    SEXP roots = navigation_column(syntax, "roots", INTSXP);
    SEXP tokens = navigation_column(syntax, "token", STRSXP);
    if (XLENGTH(tokens) > INT_MAX || XLENGTH(roots) > INT_MAX) Rf_error("syntax index is too large");
    int n = (int) XLENGTH(tokens);
    const int *line1 = navigation_integer_rows(syntax, "line1", n);
    const int *col1 = navigation_integer_rows(syntax, "col1", n);
    const int *line2 = navigation_integer_rows(syntax, "line2", n);
    const int *col2 = navigation_integer_rows(syntax, "col2", n);
    if (TYPEOF(point) != INTSXP || XLENGTH(point) != 2) Rf_error("invalid syntax point");
    int line = INTEGER(point)[0], col = INTEGER(point)[1];
    if (line == NA_INTEGER || col == NA_INTEGER || line < 1 || col < 1) return Rf_allocVector(INTSXP, 0);
    int depth = Rf_asInteger(navigation_column(syntax, "max_depth", INTSXP));
    if (TYPEOF(point) != INTSXP || XLENGTH(point) != 2 || depth < 0 || depth > (INT_MAX-4)/2) Rf_error("invalid syntax point");
    int capacity = depth * 2 + 4;
    int count = 0, pending_n = 0, visited = 0;
    int *result = (int *) R_alloc((size_t) capacity, sizeof(int));
    int *pending = (int *) R_alloc((size_t) capacity, sizeof(int));
    const int *starts = navigation_integer_rows(syntax, "child_start", n);
    const int *ends = navigation_integer_rows(syntax, "child_end", n);
    const int *children = INTEGER(navigation_column(syntax, "children", INTSXP));
    SEXP terminal_column = navigation_column(syntax, "terminal", LGLSXP);
    if (XLENGTH(terminal_column) != n) Rf_error("invalid syntax terminal length");
    const int *terminal = LOGICAL(terminal_column);
    int child_count = (int) XLENGTH(navigation_column(syntax, "children", INTSXP));
    int low = 0, high = (int) XLENGTH(roots);
    while (low < high) {
        int middle = low + (high-low)/2, row = navigation_row(INTEGER(roots)[middle], n);
        if (position_before(line2[row], col2[row], line, col-1)) low = middle+1;
        else high = middle;
    }
    int root_end = low;
    while (root_end < XLENGTH(roots)) {
        int row = navigation_row(INTEGER(roots)[root_end], n);
        if (position_before(line, col, line1[row], col1[row])) break;
        ++root_end;
    }
    for (int i = root_end-1; i >= low; --i) {
        int row = navigation_row(INTEGER(roots)[i], n);
        if (syntax_named(tokens, row, "expr") && syntax_contains(row, line, col, line1,col1,line2,col2)) {
            if (pending_n == capacity) Rf_error("invalid overlapping syntax roots");
            pending[pending_n++] = row;
        }
    }
    while (pending_n) {
        if (++visited > n) Rf_error("cyclic syntax children");
        int row = pending[--pending_n];
        if (syntax_named(tokens, row, "expr")) {
            if (count == capacity) Rf_error("invalid overlapping syntax scopes");
            result[count++] = row+1;
        }
        int begin = pending_n;
        int child_low = starts[row]-1, child_high = ends[row];
        if (child_low < 0 || child_low > child_high || child_high > child_count) Rf_error("invalid syntax child bounds");
        while (child_low < child_high) {
            int middle = child_low + (child_high-child_low)/2, child = navigation_row(children[middle], n);
            if (position_before(line2[child], col2[child], line, col-1)) child_low = middle+1;
            else child_high = middle;
        }
        for (int at = child_low; at < ends[row]; ++at) {
            int nested = navigation_row(children[at], n);
            if (position_before(line, col, line1[nested], col1[nested])) break;
            if (!terminal[nested]) {
                if (pending_n == capacity) Rf_error("invalid overlapping syntax children");
                pending[pending_n++] = nested;
            }
        }
        for (int a = begin, b = pending_n-1; a < b; ++a,--b) {int swap = pending[a];pending[a]=pending[b];pending[b]=swap;}
    }
    SEXP answer = navigation_rows(result, count);
    return answer;
}

SEXP navigation_select_c(SEXP syntax, SEXP index, SEXP candidates, SEXP point, SEXP mode, SEXP last) {
    SEXP node_column = navigation_column(index, "node", INTSXP);
    R_xlen_t row_count = XLENGTH(navigation_column(syntax, "token", STRSXP));
    if (XLENGTH(node_column) > INT_MAX || row_count > INT_MAX) Rf_error("navigation index is too large");
    int declarations = (int) XLENGTH(node_column);
    int rows = (int) row_count;
    const int *nodes = INTEGER(node_column);
    const int *names = navigation_integer_rows(index, "name", declarations);
    const int *values = navigation_integer_rows(index, "value", declarations);
    const int *functions = navigation_integer_rows(index, "function", declarations);
    const int *line1 = navigation_integer_rows(syntax, "line1", rows);
    const int *col1 = navigation_integer_rows(syntax, "col1", rows);
    const int *line2 = navigation_integer_rows(syntax, "line2", rows);
    const int *col2 = navigation_integer_rows(syntax, "col2", rows);
    if (TYPEOF(candidates) != INTSXP || XLENGTH(candidates) > INT_MAX ||
            TYPEOF(point) != INTSXP || XLENGTH(point) != 3 ||
            INTEGER(point)[2] < 0 || INTEGER(point)[2] > XLENGTH(navigation_column(syntax, "token", STRSXP))) Rf_error("invalid navigation query");
    int n = (int) XLENGTH(candidates), line = INTEGER(point)[0], token = INTEGER(point)[2]-1;
    int kind = Rf_asInteger(mode);
    if (kind < 1 || kind > 3) Rf_error("invalid navigation mode");
    int signature = kind == 3, final = Rf_asLogical(last) == TRUE;
    int *result = (int *) R_alloc((size_t) (final ? 1 : n), sizeof(int));
    int low = 0, high = n, count = 0;
    while (low < high) {
        int middle = low + (high-low)/2, declaration = navigation_row(INTEGER(candidates)[middle], declarations);
        if (line1[navigation_row(nodes[declaration], rows)] <= line) low = middle+1; else high=middle;
    }
    for (int at = final ? low-1 : 0; final ? at >= 0 : at < low; final ? --at : ++at) {
        int i = navigation_row(INTEGER(candidates)[at], declarations);
        int name = navigation_row(names[i], rows), value = navigation_row(values[i], rows);
        if (line1[name] > line) continue;
        if (signature && !functions[i]) continue;
        if (!signature && token >= 0 &&
                !position_before(line1[token], col1[token], line1[value], col1[value]) &&
                !position_before(line2[value], col2[value], line2[token], col2[token])) continue;
        int selected = signature ? functions[i] : nodes[i];
        navigation_row(selected, rows);
        result[count++] = selected;
        if (final) break;
    }
    return navigation_rows(result, count);
}
