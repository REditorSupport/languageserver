#include "index.h"
#include <R_ext/Utils.h>

/* Inspect syntax only: project code is never evaluated for indexing. */
static int is_source_call(SEXP head) {
    if (TYPEOF(head) == SYMSXP) {
        return head == Rf_install("source") || head == Rf_install("sys.source");
    }
    if (TYPEOF(head) != LANGSXP || Rf_length(head) != 3 ||
        CAR(head) != Rf_install("::") || CADR(head) != Rf_install("base")) {
        return 0;
    }
    SEXP name = CADDR(head);
    return name == Rf_install("source") || name == Rf_install("sys.source");
}

typedef enum { SOURCE_CALLS, ASSIGNMENT_CALLS } call_kind;

static void collect_calls(SEXP node, call_kind kind, SEXP result, R_xlen_t *count,
    unsigned int *visited) {
    if (TYPEOF(node) != LANGSXP && TYPEOF(node) != EXPRSXP) return;
    R_CheckStack();
    if (++*visited % 1024 == 0) R_CheckUserInterrupt();
    if (TYPEOF(node) == EXPRSXP) {
        for (R_xlen_t i = 0; i < XLENGTH(node); ++i) {
            collect_calls(VECTOR_ELT(node, i), kind, result, count, visited);
        }
        return;
    }
    SEXP head = CAR(node);
    /* Assignments inside nested functions belong to their own lexical scopes. */
    if (kind == ASSIGNMENT_CALLS && head == R_FunctionSymbol) return;
    int matches = kind == SOURCE_CALLS ? is_source_call(head) :
        head == Rf_install("<-") || head == Rf_install("=") || head == Rf_install(":=");
    if (matches) {
        if (result != R_NilValue) SET_VECTOR_ELT(result, *count, node);
        ++*count;
    }
    /* Computed callees can contain immediately invoked function bodies or
     * other expressions. Visit them before arguments in source order. */
    collect_calls(head, kind, result, count, visited);
    for (SEXP args = CDR(node); args != R_NilValue; args = CDR(args)) {
        collect_calls(CAR(args), kind, result, count, visited);
    }
}

static SEXP matching_calls(SEXP expressions, call_kind kind) {
    R_xlen_t count = 0;
    unsigned int visited = 0;
    collect_calls(expressions, kind, R_NilValue, &count, &visited);
    if (!count) return Rf_allocVector(VECSXP, 0);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
    count = 0;
    collect_calls(expressions, kind, result, &count, &visited);
    UNPROTECT(1);
    return result;
}

SEXP source_calls_c(SEXP expressions) {
    return matching_calls(expressions, SOURCE_CALLS);
}

SEXP member_assignment_calls_c(SEXP expressions) {
    return matching_calls(expressions, ASSIGNMENT_CALLS);
}

/* Index of the first changed physical line at or after a source position. */
static R_xlen_t gap_at_or_after(SEXP gaps, int line) {
    R_xlen_t lo = 0, hi = XLENGTH(gaps);
    while (lo < hi) {
        R_xlen_t mid = lo + (hi - lo) / 2;
        if (INTEGER(gaps)[mid] < line) lo = mid + 1; else hi = mid;
    }
    return lo;
}

/* Keep statement order and source ranges when opening a standalone brace.
 * The body of a function, call, assignment or conditional stays intact. */
static void collect_statements(SEXP node, SEXP ref, SEXP srcref, int keep_refs, SEXP gaps,
    SEXP statements, SEXP refs, R_xlen_t *count) {
    int incomplete = 0;
    if (gaps != R_NilValue && XLENGTH(gaps)) {
        if (TYPEOF(ref) != INTSXP || XLENGTH(ref) < 8) return;
        int first = INTEGER(ref)[6] - 1, last = INTEGER(ref)[7] - 1;
        R_xlen_t at = gap_at_or_after(gaps, first);
        if (at < XLENGTH(gaps) && INTEGER(gaps)[at] <= last) {
            incomplete = 1;
            /* Both physical delimiters must have survived unchanged. */
            if (INTEGER(gaps)[at] == first) return;
            at = gap_at_or_after(gaps, last);
            if (at < XLENGTH(gaps) && INTEGER(gaps)[at] == last) return;
        }
    }
    if (TYPEOF(node) == LANGSXP && CAR(node) == R_BraceSymbol && CDR(node) != R_NilValue) {
        SEXP nested = keep_refs ? Rf_getAttrib(node, srcref) : R_NilValue;
        /* A computed call to `{` need not have source references for the
         * arguments. Retain the whole call when child ranges are unavailable. */
        if (!keep_refs || (TYPEOF(nested) == VECSXP && XLENGTH(nested) == Rf_length(node))) {
            R_CheckStack();
            R_xlen_t i = 1; /* The first source reference is the opening brace. */
            for (SEXP args = CDR(node); args != R_NilValue; args = CDR(args), ++i) {
                collect_statements(CAR(args), keep_refs ? VECTOR_ELT(nested, i) : R_NilValue,
                    srcref, keep_refs, gaps, statements, refs, count);
            }
            return;
        }
    }
    if (incomplete) return;
    if (statements != R_NilValue) {
        SET_VECTOR_ELT(statements, *count, node);
        if (keep_refs) SET_VECTOR_ELT(refs, *count, ref);
    }
    ++*count;
}

SEXP top_level_statements_c(SEXP expressions, SEXP gaps) {
    if (TYPEOF(expressions) != EXPRSXP) Rf_error("expected an expression vector");
    if (TYPEOF(gaps) != INTSXP && gaps != R_NilValue) Rf_error("expected integer line numbers");
    R_xlen_t n = XLENGTH(expressions), i = 0;
    if (gaps == R_NilValue || !XLENGTH(gaps)) {
        for (; i < n; ++i) {
            SEXP node = VECTOR_ELT(expressions, i);
            if (TYPEOF(node) == LANGSXP && CAR(node) == R_BraceSymbol) break;
        }
    }
    if (i == n) return expressions;
    SEXP srcref = Rf_install("srcref");
    SEXP original = Rf_getAttrib(expressions, srcref);
    int keep_refs = TYPEOF(original) == VECSXP && XLENGTH(original) == n;
    R_xlen_t count = i;
    for (; i < n; ++i) {
        collect_statements(VECTOR_ELT(expressions, i), keep_refs ? VECTOR_ELT(original, i) : R_NilValue,
            srcref, keep_refs, gaps, R_NilValue, R_NilValue, &count);
    }
    SEXP statements = PROTECT(Rf_allocVector(EXPRSXP, count));
    SEXP refs = PROTECT(keep_refs ? Rf_allocVector(VECSXP, count) : R_NilValue);
    count = 0;
    for (i = 0; i < n; ++i) {
        collect_statements(VECTOR_ELT(expressions, i), keep_refs ? VECTOR_ELT(original, i) : R_NilValue,
            srcref, keep_refs, gaps, statements, refs, &count);
    }
    if (keep_refs) Rf_setAttrib(statements, srcref, refs);
    UNPROTECT(2);
    return statements;
}
