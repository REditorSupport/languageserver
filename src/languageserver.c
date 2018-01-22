#include "languageserver.h"

static const R_CallMethodDef CallEntries[] = {
    {"document_backward_search", (DL_FUNC) &document_backward_search, 4},
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
