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
