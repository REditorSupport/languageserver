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

static void collect_source_calls(SEXP node, SEXP result, R_xlen_t *count,
    unsigned int *visited) {
    if (TYPEOF(node) != LANGSXP && TYPEOF(node) != EXPRSXP) return;
    R_CheckStack();
    if (++*visited % 1024 == 0) R_CheckUserInterrupt();
    if (TYPEOF(node) == EXPRSXP) {
        for (R_xlen_t i = 0; i < XLENGTH(node); ++i) {
            collect_source_calls(VECTOR_ELT(node, i), result, count, visited);
        }
        return;
    }
    if (is_source_call(CAR(node))) {
        if (result != R_NilValue) SET_VECTOR_ELT(result, *count, node);
        ++*count;
    }
    for (SEXP args = CDR(node); args != R_NilValue; args = CDR(args)) {
        collect_source_calls(CAR(args), result, count, visited);
    }
}

SEXP source_calls_c(SEXP expressions) {
    R_xlen_t count = 0;
    unsigned int visited = 0;
    collect_source_calls(expressions, R_NilValue, &count, &visited);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
    count = 0;
    collect_source_calls(expressions, result, &count, &visited);
    UNPROTECT(1);
    return result;
}
