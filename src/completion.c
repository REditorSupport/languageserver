#include "completion.h"

#include <limits.h>
#include <string.h>
#include <Rversion.h>

/* Inspect binding kind before looking up a value. In particular, findVarInFrame
 * must never reach an active binding. Unforced promises are returned as missing
 * unless a package metadata worker explicitly requests materialization. */
SEXP member_binding_c(SEXP env, SEXP name, SEXP materialize) {
    if (TYPEOF(env) != ENVSXP || TYPEOF(name) != STRSXP || XLENGTH(name) != 1 ||
            STRING_ELT(name, 0) == NA_STRING) Rf_error("Invalid binding");
    SEXP sym = Rf_installChar(STRING_ELT(name, 0));
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
#if R_VERSION >= R_Version(4, 6, 0)
    R_BindingType_t kind = R_GetBindingType(sym, env);
    if (kind == R_BindingTypeActive) {
        SET_VECTOR_ELT(result, 0, Rf_mkString("active"));
        SET_VECTOR_ELT(result, 1, R_ActiveBindingFunction(sym, env));
    } else if (kind == R_BindingTypeValue || kind == R_BindingTypeForced ||
            (kind == R_BindingTypeDelayed && Rf_asLogical(materialize) == TRUE)) {
        SEXP value = PROTECT(R_getVar(sym, env, FALSE));
        SET_VECTOR_ELT(result, 0, Rf_mkString("value"));
        SET_VECTOR_ELT(result, 1, value);
        UNPROTECT(1);
    } else {
        SET_VECTOR_ELT(result, 0, Rf_mkString("deferred"));
    }
#else
    /* Older R binding-kind APIs require an existing name. Enumerating names
     * does not force promises or invoke active bindings. */
    SEXP binding_names = PROTECT(R_lsInternal3(env, TRUE, FALSE));
    int present = 0;
    for (R_xlen_t i = 0; i < XLENGTH(binding_names); ++i) {
        if (strcmp(CHAR(STRING_ELT(binding_names, i)), CHAR(STRING_ELT(name, 0))) == 0) {
            present = 1;
            break;
        }
    }
    UNPROTECT(1);
    if (!present) {
        SET_VECTOR_ELT(result, 0, Rf_mkString("deferred"));
    } else if (R_BindingIsActive(sym, env)) {
        SET_VECTOR_ELT(result, 0, Rf_mkString("active"));
#if R_VERSION >= R_Version(4, 0, 0)
        SET_VECTOR_ELT(result, 1, R_ActiveBindingFunction(sym, env));
#endif
    } else {
        SEXP value = Rf_findVarInFrame(env, sym);
        if (TYPEOF(value) == PROMSXP && PRVALUE(value) != R_UnboundValue) {
            value = PRVALUE(value);
        } else if (TYPEOF(value) == PROMSXP && Rf_asLogical(materialize) == TRUE) {
            /* This branch is only used on the selected package namespace in
             * a background worker, never on document or getter bindings. */
            value = Rf_eval(sym, env);
        }
        if (value == R_UnboundValue || TYPEOF(value) == PROMSXP) {
            SET_VECTOR_ELT(result, 0, Rf_mkString("deferred"));
        } else {
            SET_VECTOR_ELT(result, 0, Rf_mkString("value"));
            SET_VECTOR_ELT(result, 1, value);
        }
    }
#endif
    UNPROTECT(1);
    return result;
}

typedef enum {
    TOKEN_OTHER = 0,
    TOKEN_EXPR,
    TOKEN_SYMBOL,
    TOKEN_SYMBOL_SUB,
    TOKEN_SYMBOL_FORMALS,
    TOKEN_SYMBOL_FUNCTION_CALL,
    TOKEN_FUNCTION,
    TOKEN_LAMBDA,
    TOKEN_LEFT_ASSIGN,
    TOKEN_RIGHT_ASSIGN,
    TOKEN_EQ_ASSIGN,
    TOKEN_FORCOND,
    TOKEN_DOLLAR
} completion_token_kind;

static completion_token_kind classify_token(SEXP value) {
    if (value == NA_STRING) {
        return TOKEN_OTHER;
    }

    const char *token = CHAR(value);
    if (strcmp(token, "expr") == 0) return TOKEN_EXPR;
    if (strcmp(token, "SYMBOL") == 0) return TOKEN_SYMBOL;
    if (strcmp(token, "SYMBOL_SUB") == 0) return TOKEN_SYMBOL_SUB;
    if (strcmp(token, "SYMBOL_FORMALS") == 0) return TOKEN_SYMBOL_FORMALS;
    if (strcmp(token, "SYMBOL_FUNCTION_CALL") == 0) return TOKEN_SYMBOL_FUNCTION_CALL;
    if (strcmp(token, "FUNCTION") == 0) return TOKEN_FUNCTION;
    if (strcmp(token, "'\\\\'") == 0) return TOKEN_LAMBDA;
    if (strcmp(token, "LEFT_ASSIGN") == 0) return TOKEN_LEFT_ASSIGN;
    if (strcmp(token, "RIGHT_ASSIGN") == 0) return TOKEN_RIGHT_ASSIGN;
    if (strcmp(token, "EQ_ASSIGN") == 0) return TOKEN_EQ_ASSIGN;
    if (strcmp(token, "forcond") == 0) return TOKEN_FORCOND;
    if (strcmp(token, "'$'") == 0) return TOKEN_DOLLAR;
    return TOKEN_OTHER;
}

static int is_before(
    int row,
    int assignment,
    const int *line1,
    const int *col1,
    const int *line2,
    const int *col2
) {
    return line2[row] < line1[assignment] ||
        (line2[row] == line1[assignment] && col2[row] < col1[assignment]);
}

static int is_after(
    int row,
    int assignment,
    const int *line1,
    const int *col1,
    const int *line2,
    const int *col2
) {
    return line1[row] > line2[assignment] ||
        (line1[row] == line2[assignment] && col1[row] > col2[assignment]);
}

static int simple_symbol_row(
    int expr_row,
    const int *id,
    const int *first_child,
    const int *next_sibling,
    const completion_token_kind *kind
) {
    int child = first_child[id[expr_row]];
    if (child >= 0 && next_sibling[child] < 0 && kind[child] == TOKEN_SYMBOL) {
        return child;
    }
    return -1;
}

static int is_function_expr(
    int expr_row,
    const int *id,
    const int *first_child,
    const int *next_sibling,
    const completion_token_kind *kind
) {
    int child = first_child[id[expr_row]];
    while (child >= 0) {
        if (kind[child] == TOKEN_FUNCTION || kind[child] == TOKEN_LAMBDA) {
            return 1;
        }
        child = next_sibling[child];
    }
    return 0;
}

static int top_row(
    int row,
    const int *parent,
    const int *row_by_id,
    int max_id
) {
    int parent_id = parent[row];
    while (parent_id != 0) {
        if (parent_id < 0 || parent_id > max_id || row_by_id[parent_id] < 0) {
            return -1;
        }
        row = row_by_id[parent_id];
        parent_id = parent[row];
    }
    return row;
}

static SEXP row_vector(const int *rows, int size) {
    SEXP result = Rf_allocVector(INTSXP, size);
    int *out = INTEGER(result);
    for (int i = 0; i < size; i++) {
        out[i] = rows[i] + 1;
    }
    return result;
}

typedef struct {
    const char **labels;
    const char **sort_text;
    const char *token;
    size_t token_length;
} completion_order_context;

static int starts_with_token(
    const char *label,
    const completion_order_context *context
) {
    return strlen(label) >= context->token_length &&
        strncmp(label, context->token, context->token_length) == 0;
}

/* Negative means a should sort before b. */
static int compare_completion_candidates(
    int a,
    int b,
    const completion_order_context *context
) {
    int a_prefix = starts_with_token(context->labels[a], context);
    int b_prefix = starts_with_token(context->labels[b], context);
    if (a_prefix != b_prefix) return b_prefix - a_prefix;

    int comparison = strcmp(context->sort_text[a], context->sort_text[b]);
    if (comparison != 0) return comparison;
    return a < b ? -1 : a > b;
}

/* The heap root is the worst currently selected candidate. */
static void completion_heap_sift_up(
    int *heap,
    int child,
    const completion_order_context *context
) {
    while (child > 0) {
        int parent = (child - 1) / 2;
        if (compare_completion_candidates(
                heap[parent], heap[child], context) >= 0) break;
        int value = heap[parent];
        heap[parent] = heap[child];
        heap[child] = value;
        child = parent;
    }
}

static void completion_heap_sift_down(
    int *heap,
    int size,
    const completion_order_context *context
) {
    int parent = 0;
    while (1) {
        int left = parent * 2 + 1;
        if (left >= size) break;
        int right = left + 1;
        int worse = left;
        if (right < size && compare_completion_candidates(
                heap[left], heap[right], context) < 0) {
            worse = right;
        }
        if (compare_completion_candidates(
                heap[parent], heap[worse], context) >= 0) break;
        int value = heap[parent];
        heap[parent] = heap[worse];
        heap[worse] = value;
        parent = worse;
    }
}

SEXP completion_select_c(
    SEXP labels,
    SEXP sort_text,
    SEXP token,
    SEXP limit
) {
    if (!Rf_isString(labels) || !Rf_isString(sort_text) ||
            !Rf_isString(token) || XLENGTH(token) != 1 ||
            !Rf_isInteger(limit) || XLENGTH(limit) != 1) {
        Rf_error("invalid completion selector arguments");
    }
    R_xlen_t n_long = XLENGTH(labels);
    if (n_long > INT_MAX || XLENGTH(sort_text) != n_long) {
        Rf_error("invalid completion selector lengths");
    }
    int n = (int) n_long;
    int max_selected = INTEGER(limit)[0];
    if (max_selected == NA_INTEGER || max_selected < 0) {
        Rf_error("completion selector limit must be non-negative");
    }
    if (max_selected > n) max_selected = n;

    if (STRING_ELT(token, 0) == NA_STRING) {
        Rf_error("completion selector token must not be missing");
    }
    const char **label_text = (const char **)
        R_alloc((size_t) n, sizeof(const char *));
    const char **sort_text_values = (const char **)
        R_alloc((size_t) n, sizeof(const char *));
    for (int i = 0; i < n; i++) {
        if (STRING_ELT(labels, i) == NA_STRING ||
                STRING_ELT(sort_text, i) == NA_STRING) {
            Rf_error("completion selector candidates must not be missing");
        }
        label_text[i] = Rf_translateCharUTF8(STRING_ELT(labels, i));
        sort_text_values[i] = Rf_translateCharUTF8(STRING_ELT(sort_text, i));
    }
    const char *token_text = Rf_translateCharUTF8(STRING_ELT(token, 0));

    completion_order_context context = {
        label_text,
        sort_text_values,
        token_text,
        strlen(token_text)
    };

    int *heap = (int *) R_alloc((size_t) max_selected, sizeof(int));
    int heap_size = 0;
    for (int i = 0; i < n; i++) {
        if ((i & 16383) == 0) R_CheckUserInterrupt();
        if (heap_size < max_selected) {
            heap[heap_size] = i;
            completion_heap_sift_up(heap, heap_size, &context);
            heap_size++;
        } else if (max_selected > 0 && compare_completion_candidates(
                i, heap[0], &context) < 0) {
            heap[0] = i;
            completion_heap_sift_down(heap, heap_size, &context);
        }
    }

    /* Sorting at most max_completions entries is cheap and deterministic. */
    for (int i = 1; i < heap_size; i++) {
        int value = heap[i];
        int position = i;
        while (position > 0 && compare_completion_candidates(
                value, heap[position - 1], &context) < 0) {
            heap[position] = heap[position - 1];
            position--;
        }
        heap[position] = value;
    }

    return row_vector(heap, heap_size);
}

SEXP completion_parse_index_c(
    SEXP id,
    SEXP parent,
    SEXP token,
    SEXP line1,
    SEXP col1,
    SEXP line2,
    SEXP col2
) {
    if (!Rf_isInteger(id) || !Rf_isInteger(parent) ||
            !Rf_isInteger(line1) || !Rf_isInteger(col1) ||
            !Rf_isInteger(line2) || !Rf_isInteger(col2) ||
            !Rf_isString(token)) {
        Rf_error("completion parse columns have invalid types");
    }

    R_xlen_t n_long = XLENGTH(id);
    if (n_long > INT_MAX) {
        Rf_error("completion parse data is too large");
    }
    int n = (int) n_long;
    if (XLENGTH(parent) != n || XLENGTH(token) != n ||
            XLENGTH(line1) != n || XLENGTH(col1) != n ||
            XLENGTH(line2) != n || XLENGTH(col2) != n) {
        Rf_error("completion parse columns have different lengths");
    }

    const int *id_ptr = INTEGER(id);
    const int *parent_ptr = INTEGER(parent);
    const int *line1_ptr = INTEGER(line1);
    const int *col1_ptr = INTEGER(col1);
    const int *line2_ptr = INTEGER(line2);
    const int *col2_ptr = INTEGER(col2);

    int max_id = 0;
    for (int i = 0; i < n; i++) {
        if (id_ptr[i] < 0) {
            Rf_error("completion parse ids must be non-negative");
        }
        if (id_ptr[i] > max_id) max_id = id_ptr[i];
        if (parent_ptr[i] > max_id) max_id = parent_ptr[i];
    }
    if (max_id == INT_MAX) {
        Rf_error("completion parse id is too large");
    }

    int *row_by_id = (int *) R_alloc((size_t) max_id + 1, sizeof(int));
    int *first_child = (int *) R_alloc((size_t) max_id + 1, sizeof(int));
    int *last_child = (int *) R_alloc((size_t) max_id + 1, sizeof(int));
    int *next_sibling = (int *) R_alloc((size_t) n, sizeof(int));
    int *parent_has_dollar = (int *) R_alloc((size_t) max_id + 1, sizeof(int));
    completion_token_kind *kind = (completion_token_kind *)
        R_alloc((size_t) n, sizeof(completion_token_kind));

    for (int i = 0; i <= max_id; i++) {
        row_by_id[i] = -1;
        first_child[i] = -1;
        last_child[i] = -1;
        parent_has_dollar[i] = 0;
    }
    for (int i = 0; i < n; i++) {
        next_sibling[i] = -1;
        kind[i] = classify_token(STRING_ELT(token, i));

        int node_id = id_ptr[i];
        if (node_id > 0) {
            if (row_by_id[node_id] >= 0) {
                Rf_error("completion parse ids must be unique");
            }
            row_by_id[node_id] = i;
        }

        int parent_id = parent_ptr[i];
        if (parent_id >= 0) {
            if (first_child[parent_id] < 0) {
                first_child[parent_id] = i;
            } else {
                next_sibling[last_child[parent_id]] = i;
            }
            last_child[parent_id] = i;
            if (kind[i] == TOKEN_DOLLAR) {
                parent_has_dollar[parent_id] = 1;
            }
        }
    }

    int *symbol_name = (int *) R_alloc((size_t) n, sizeof(int));
    int *symbol_range = (int *) R_alloc((size_t) n, sizeof(int));
    int *function_name = (int *) R_alloc((size_t) n, sizeof(int));
    int *function_range = (int *) R_alloc((size_t) n, sizeof(int));
    int *formal_name = (int *) R_alloc((size_t) n, sizeof(int));
    int *formal_range = (int *) R_alloc((size_t) n, sizeof(int));
    int *token_rows = (int *) R_alloc((size_t) n, sizeof(int));
    int *empty_token_rows = (int *) R_alloc((size_t) n, sizeof(int));
    int symbol_count = 0;
    int function_count = 0;
    int formal_count = 0;
    int token_count = 0;
    int empty_token_count = 0;

    for (int i = 0; i < n; i++) {
        if ((i & 16383) == 0) R_CheckUserInterrupt();

        if (kind[i] == TOKEN_SYMBOL || kind[i] == TOKEN_SYMBOL_SUB ||
                kind[i] == TOKEN_SYMBOL_FORMALS ||
                kind[i] == TOKEN_SYMBOL_FUNCTION_CALL) {
            token_rows[token_count++] = i;
        }
        if (kind[i] == TOKEN_SYMBOL_SUB ||
                (kind[i] == TOKEN_SYMBOL && parent_ptr[i] >= 0 &&
                    parent_has_dollar[parent_ptr[i]])) {
            empty_token_rows[empty_token_count++] = i;
        }

        if (kind[i] == TOKEN_SYMBOL_FORMALS) {
            int parent_id = parent_ptr[i];
            if (parent_id < 0 || parent_id > max_id) continue;
            int range_row = row_by_id[parent_id];
            if (range_row >= 0) {
                formal_name[formal_count] = i;
                formal_range[formal_count++] = range_row;
            }
            continue;
        }

        if (kind[i] != TOKEN_LEFT_ASSIGN &&
                kind[i] != TOKEN_RIGHT_ASSIGN &&
                kind[i] != TOKEN_EQ_ASSIGN) {
            continue;
        }

        if (parent_ptr[i] < 0 || parent_ptr[i] > max_id) continue;
        int before = -1;
        int after = -1;
        int sibling = first_child[parent_ptr[i]];
        while (sibling >= 0) {
            if (kind[sibling] == TOKEN_EXPR) {
                if (is_before(sibling, i, line1_ptr, col1_ptr, line2_ptr, col2_ptr)) {
                    before = sibling;
                } else if (after < 0 && is_after(
                        sibling, i, line1_ptr, col1_ptr, line2_ptr, col2_ptr)) {
                    after = sibling;
                }
            }
            sibling = next_sibling[sibling];
        }

        int is_left = kind[i] != TOKEN_RIGHT_ASSIGN;
        int name_expr = is_left ? before : after;
        int value_expr = is_left ? after : before;
        if (name_expr < 0 || value_expr < 0) continue;

        int name_row = simple_symbol_row(
            name_expr, id_ptr, first_child, next_sibling, kind);
        if (name_row < 0) continue;
        int range_row = top_row(name_row, parent_ptr, row_by_id, max_id);
        if (range_row < 0) continue;

        if (is_function_expr(value_expr, id_ptr, first_child, next_sibling, kind)) {
            function_name[function_count] = name_row;
            function_range[function_count++] = range_row;
        } else {
            symbol_name[symbol_count] = name_row;
            symbol_range[symbol_count++] = range_row;
        }
    }

    /* Keep loop variables after assignments to preserve provider ordering. */
    for (int i = 0; i < n; i++) {
        if (kind[i] != TOKEN_FORCOND) continue;
        int child = first_child[id_ptr[i]];
        while (child >= 0) {
            if (kind[child] == TOKEN_SYMBOL) {
                int range_row = top_row(child, parent_ptr, row_by_id, max_id);
                if (range_row >= 0) {
                    symbol_name[symbol_count] = child;
                    symbol_range[symbol_count++] = range_row;
                }
            }
            child = next_sibling[child];
        }
    }

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 8));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 8));
    const char *result_names[] = {
        "symbol_name", "symbol_range", "function_name", "function_range",
        "formal_name", "formal_range", "token", "empty_token"
    };
    const int *result_rows[] = {
        symbol_name, symbol_range, function_name, function_range,
        formal_name, formal_range, token_rows, empty_token_rows
    };
    int result_sizes[] = {
        symbol_count, symbol_count, function_count, function_count,
        formal_count, formal_count, token_count, empty_token_count
    };

    for (int i = 0; i < 8; i++) {
        SET_STRING_ELT(names, i, Rf_mkChar(result_names[i]));
        SEXP value = PROTECT(row_vector(result_rows[i], result_sizes[i]));
        SET_VECTOR_ELT(result, i, value);
        UNPROTECT(1);
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}
