#include "document.h"
#include "pairlist.h"

static const R_CallMethodDef CallEntries[] = {
    {"document_backward_search", (DL_FUNC) &document_backward_search, 4},
    {"pairlist_car", (DL_FUNC) &pairlist_car, 1},
    {"pairlist_cdr", (DL_FUNC) &pairlist_cdr, 1},
    {"pairlist_last", (DL_FUNC) &pairlist_last, 1},
    {"pairlist_push", (DL_FUNC) &pairlist_push, 2},
    {"pairlist_append", (DL_FUNC) &pairlist_append, 2},
    {"pairlist_update", (DL_FUNC) &pairlist_update, 2},
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
