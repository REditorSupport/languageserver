#include "document.h"
#include "pairlist.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>

SEXP stdin_is_empty() {

    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    DWORD rd;

    if (PeekNamedPipe(h, NULL, 0, NULL, &rd, NULL)) {
        return Rf_ScalarLogical(rd == 0);
    }
    return Rf_ScalarLogical(1);
}

#endif

static const R_CallMethodDef CallEntries[] = {
    {"document_backward_search", (DL_FUNC) &document_backward_search, 4},
    {"pairlist_car", (DL_FUNC) &pairlist_car, 1},
    {"pairlist_cdr", (DL_FUNC) &pairlist_cdr, 1},
    {"pairlist_last", (DL_FUNC) &pairlist_last, 1},
    {"pairlist_append", (DL_FUNC) &pairlist_append, 2},
    {"pairlist_update", (DL_FUNC) &pairlist_update, 2},
#if defined(_WIN32) || defined(_WIN64)
    {"stdin_is_empty", (DL_FUNC) &stdin_is_empty, 0},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
