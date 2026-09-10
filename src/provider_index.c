#include "semantic.h"

#include <limits.h>
#include <string.h>

typedef enum {
    P_OTHER, P_EXPR, P_SYMBOL, P_FORMAL, P_CALL, P_PACKAGE, P_FUNCTION,
    P_LEFT_ASSIGN, P_RIGHT_ASSIGN, P_EQ_ASSIGN, P_OPEN, P_CLOSE,
    P_COMMA, P_SUB, P_EQ_SUB, P_MEMBER, P_COMMENT
} provider_token;

typedef struct {
    int n, max_id;
    const int *id, *parent, *terminal;
    int *row, *first, *next;
    provider_token *kind;
} provider_tree;

static SEXP parse_column(SEXP data, const char *name, SEXPTYPE type) {
    if (TYPEOF(data) != VECSXP) Rf_error("Expected parse data columns");
    SEXP names = Rf_getAttrib(data, R_NamesSymbol);
    for (R_xlen_t i = 0; i < XLENGTH(names); i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            SEXP result = VECTOR_ELT(data, i);
            if (TYPEOF(result) != type) Rf_error("Invalid parse data column: %s", name);
            return result;
        }
    }
    Rf_error("Missing parse data column: %s", name);
    return R_NilValue;
}

static provider_token provider_kind(SEXP value) {
    if (value == NA_STRING) return P_OTHER;
    const char *name = CHAR(value);
    if (!strcmp(name, "expr") || !strcmp(name, "expr_or_assign_or_help")) return P_EXPR;
    if (!strcmp(name, "SYMBOL")) return P_SYMBOL;
    if (!strcmp(name, "SYMBOL_FORMALS")) return P_FORMAL;
    if (!strcmp(name, "SYMBOL_FUNCTION_CALL")) return P_CALL;
    if (!strcmp(name, "SYMBOL_PACKAGE")) return P_PACKAGE;
    if (!strcmp(name, "FUNCTION") || !strcmp(name, "'\\\\'")) return P_FUNCTION;
    if (!strcmp(name, "LEFT_ASSIGN")) return P_LEFT_ASSIGN;
    if (!strcmp(name, "RIGHT_ASSIGN")) return P_RIGHT_ASSIGN;
    if (!strcmp(name, "EQ_ASSIGN")) return P_EQ_ASSIGN;
    if (!strcmp(name, "'('")) return P_OPEN;
    if (!strcmp(name, "')'")) return P_CLOSE;
    if (!strcmp(name, "','")) return P_COMMA;
    if (!strcmp(name, "SYMBOL_SUB")) return P_SUB;
    if (!strcmp(name, "EQ_SUB")) return P_EQ_SUB;
    if (!strcmp(name, "'$'") || !strcmp(name, "'@'")) return P_MEMBER;
    if (!strcmp(name, "COMMENT")) return P_COMMENT;
    return P_OTHER;
}

/* Parse ids are dense integer identifiers, not data-frame row numbers.
 * Adjacency is built once and preserves source order amongst siblings. */
static provider_tree provider_tree_init(SEXP data) {
    SEXP ids = parse_column(data, "id", INTSXP);
    SEXP parents = parse_column(data, "parent", INTSXP);
    SEXP tokens = parse_column(data, "token", STRSXP);
    SEXP terminal = parse_column(data, "terminal", LGLSXP);
    R_xlen_t n = XLENGTH(ids);
    if (n > INT_MAX || XLENGTH(parents) != n || XLENGTH(tokens) != n ||
            XLENGTH(terminal) != n) Rf_error("Invalid parse data lengths");
    provider_tree tree;
    tree.n = (int) n;
    tree.max_id = 0;
    tree.id = INTEGER(ids);
    tree.parent = INTEGER(parents);
    tree.terminal = LOGICAL(terminal);
    for (int i = 0; i < tree.n; i++) {
        if (tree.id[i] < 0 || tree.id[i] == INT_MAX || tree.parent[i] == INT_MAX) {
            Rf_error("Invalid parse data id");
        }
        if (tree.id[i] > tree.max_id) tree.max_id = tree.id[i];
        if (tree.parent[i] > tree.max_id) tree.max_id = tree.parent[i];
    }
    size_t capacity = (size_t) tree.max_id + 1;
    tree.row = (int *) R_alloc(capacity, sizeof(int));
    tree.first = (int *) R_alloc(capacity, sizeof(int));
    int *last = (int *) R_alloc(capacity, sizeof(int));
    tree.next = (int *) R_alloc((size_t) tree.n, sizeof(int));
    tree.kind = (provider_token *) R_alloc((size_t) tree.n, sizeof(provider_token));
    for (int i = 0; i <= tree.max_id; i++) {
        tree.row[i] = tree.first[i] = last[i] = -1;
    }
    for (int i = 0; i < tree.n; i++) {
        int id = tree.id[i], parent = tree.parent[i];
        if (tree.row[id] >= 0) Rf_error("Parse data ids must be unique");
        tree.row[id] = i;
        tree.next[i] = -1;
        tree.kind[i] = provider_kind(STRING_ELT(tokens, i));
        /* Negative comment parents are source references, not tree edges. */
        if (parent > 0) {
            if (tree.first[parent] < 0) tree.first[parent] = i;
            else tree.next[last[parent]] = i;
            last[parent] = i;
        }
    }
    return tree;
}

static int tree_parent(const provider_tree *tree, int row) {
    int parent = tree->parent[row];
    return parent > 0 ? tree->row[parent] : -1;
}

static int simple_symbol(const provider_tree *tree, int row) {
    int child = tree->first[tree->id[row]];
    if (child >= 0 && tree->next[child] < 0 && tree->kind[child] == P_SYMBOL) return child;
    return -1;
}

static SEXP provider_rows(const int *rows, int n, int offset) {
    SEXP result = Rf_allocVector(INTSXP, n);
    for (int i = 0; i < n; i++) INTEGER(result)[i] = rows[i] + offset;
    return result;
}

SEXP function_assignment_ids_c(SEXP data) {
    provider_tree tree = provider_tree_init(data);
    int *function_expr = (int *) R_alloc((size_t) tree.max_id + 1, sizeof(int));
    memset(function_expr, 0, ((size_t) tree.max_id + 1) * sizeof(int));
    for (int i = 0; i < tree.n; i++) {
        if (tree.kind[i] == P_FUNCTION && tree.parent[i] > 0) function_expr[tree.parent[i]] = 1;
    }
    int *result = (int *) R_alloc((size_t) tree.n, sizeof(int));
    int count = 0;
    for (int i = 0; i < tree.n; i++) {
        if ((i & 16383) == 0) R_CheckUserInterrupt();
        provider_token kind = tree.kind[i];
        if (kind != P_LEFT_ASSIGN && kind != P_RIGHT_ASSIGN && kind != P_EQ_ASSIGN) continue;
        if (tree.parent[i] <= 0) continue;
        int before = -1, after = -1, before_n = 0, after_n = 0, passed = 0;
        for (int child = tree.first[tree.parent[i]]; child >= 0; child = tree.next[child]) {
            if (child == i) { passed = 1; continue; }
            if (passed) { after = child; after_n++; }
            else { before = child; before_n++; }
        }
        int rhs = kind == P_RIGHT_ASSIGN ? before : after;
        int rhs_n = kind == P_RIGHT_ASSIGN ? before_n : after_n;
        if (rhs_n != 1) continue;
        /* Unwrap only syntactic parentheses, ignoring comments inside them. */
        int depth = 0;
        while (!function_expr[tree.id[rhs]]) {
            int children[3], child_n = 0;
            for (int child = tree.first[tree.id[rhs]]; child >= 0; child = tree.next[child]) {
                if (tree.kind[child] == P_COMMENT) continue;
                if (child_n < 3) children[child_n] = child;
                child_n++;
                if (child_n > 3) break;
            }
            if (child_n != 3 || tree.kind[children[0]] != P_OPEN ||
                    tree.kind[children[2]] != P_CLOSE) break;
            rhs = children[1];
            if (++depth > tree.n) Rf_error("Cyclic parse data");
        }
        if (!function_expr[tree.id[rhs]]) continue;
        passed = 0;
        for (int child = tree.first[tree.parent[i]]; child >= 0; child = tree.next[child]) {
            if (child == i) { passed = 1; continue; }
            if ((kind == P_RIGHT_ASSIGN ? !passed : passed) || tree.kind[child] != P_EXPR) continue;
            int symbol = -1, terminals = 0;
            for (int leaf = tree.first[tree.id[child]]; leaf >= 0; leaf = tree.next[leaf]) {
                if (tree.terminal[leaf]) { terminals++; symbol = leaf; }
            }
            if (terminals == 1 && tree.kind[symbol] == P_SYMBOL) result[count++] = tree.id[symbol];
        }
    }
    return provider_rows(result, count, 0);
}

SEXP range_provider_index_c(SEXP data) {
    provider_tree tree = provider_tree_init(data);
    int n = tree.n;
    int *pending = (int *) R_alloc((size_t) n, sizeof(int));
    int *queue = (int *) R_alloc((size_t) n, sizeof(int));
    int *callee = (int *) R_alloc((size_t) n, sizeof(int));
    int *package = (int *) R_alloc((size_t) n, sizeof(int));
    int *member = (int *) R_alloc((size_t) n, sizeof(int));
    memset(pending, 0, (size_t) n * sizeof(int));
    for (int i = 0; i < n; i++) {
        int parent = tree_parent(&tree, i);
        if (parent >= 0) pending[parent]++;
        callee[i] = tree.kind[i] == P_CALL ? i : -1;
        package[i] = tree.kind[i] == P_PACKAGE ? i : -1;
        member[i] = tree.kind[i] == P_MEMBER;
    }
    int queued = 0;
    for (int i = 0; i < n; i++) if (!pending[i]) queue[queued++] = i;
    /* Postorder propagation makes finding the first function/package token
     * and rejecting member calls linear even for deeply nested callees. */
    for (int head = 0; head < queued; head++) {
        int row = queue[head], parent = tree_parent(&tree, row);
        if (parent < 0) continue;
        if (callee[row] >= 0 && (callee[parent] < 0 || callee[row] < callee[parent])) callee[parent] = callee[row];
        if (package[row] >= 0 && (package[parent] < 0 || package[row] < package[parent])) package[parent] = package[row];
        member[parent] |= member[row];
        if (!--pending[parent]) queue[queued++] = parent;
    }
    if (queued != n) Rf_error("Cyclic parse data");

    enum { VARIABLES, CALLS, FUNCTIONS, PACKAGES, FIRST_ARG, LAST_ARG,
        ARG_CALL, ARG_FIRST, ARG_NAME, ARG_SYMBOL, FIELDS };
    int *rows[FIELDS];
    for (int i = 0; i < FIELDS; i++) rows[i] = (int *) R_alloc((size_t) n, sizeof(int));
    int variables_n = 0, calls_n = 0, args_n = 0;
    for (int i = 0; i < n; i++) {
        if ((i & 16383) == 0) R_CheckUserInterrupt();
        if (tree.kind[i] == P_SYMBOL || tree.kind[i] == P_FORMAL) rows[VARIABLES][variables_n++] = i;
        if (tree.kind[i] != P_EXPR) continue;
        int callee_expr = tree.first[tree.id[i]];
        if (callee_expr < 0 || tree.kind[callee_expr] != P_EXPR ||
                callee[callee_expr] < 0 || member[callee_expr]) continue;
        int open = tree.next[callee_expr];
        if (open < 0 || tree.kind[open] != P_OPEN) continue;
        int close = -1;
        for (int child = tree.next[open]; child >= 0; child = tree.next[child]) {
            if (tree.kind[child] == P_CLOSE) close = child;
        }
        if (close < 0) continue;
        rows[CALLS][calls_n] = i;
        rows[FUNCTIONS][calls_n] = callee[callee_expr];
        rows[PACKAGES][calls_n] = package[callee_expr];
        rows[FIRST_ARG][calls_n] = args_n;
        int first = -1, last = -1, name = -1;
        for (int child = tree.next[open]; child >= 0; child = tree.next[child]) {
            if (child == close || tree.kind[child] == P_COMMA) {
                rows[ARG_CALL][args_n] = calls_n;
                rows[ARG_FIRST][args_n] = first;
                rows[ARG_NAME][args_n] = name;
                rows[ARG_SYMBOL][args_n] = first >= 0 && first == last &&
                    tree.kind[first] == P_EXPR ? simple_symbol(&tree, first) : -1;
                args_n++;
                first = last = name = -1;
                if (child == close) break;
            } else {
                if (first < 0) first = child;
                last = child;
                int next = tree.next[child];
                if (name < 0 && tree.kind[child] == P_SUB && next >= 0 &&
                        tree.kind[next] == P_EQ_SUB) name = child;
            }
        }
        rows[LAST_ARG][calls_n] = args_n - 1;
        calls_n++;
    }
    const char *field_names[FIELDS] = {
        "variable", "call", "function", "package", "first_argument", "last_argument",
        "argument_call", "argument_first", "argument_name", "argument_symbol"
    };
    SEXP result = PROTECT(Rf_allocVector(VECSXP, FIELDS));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, FIELDS));
    for (int i = 0; i < FIELDS; i++) {
        int size = i == VARIABLES ? variables_n : i <= LAST_ARG ? calls_n : args_n;
        SET_VECTOR_ELT(result, i, provider_rows(rows[i], size, 1));
        SET_STRING_ELT(names, i, Rf_mkChar(field_names[i]));
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}
