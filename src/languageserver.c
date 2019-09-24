#include "document.h"
#include "reader.h"


#if !defined(_WIN32) && !defined(_WIN64)

#include <unistd.h> /* for getppid */

SEXP do_getppid() {
    int ppid;
    ppid = (int) getppid();
    return Rf_ScalarInteger(ppid);
}
#endif

static const R_CallMethodDef CallEntries[] = {
    {"content_backward_search", (DL_FUNC) &content_backward_search, 5},
    {"stdin_read_char", (DL_FUNC) &stdin_read_char, 1},
    {"stdin_read_line", (DL_FUNC) &stdin_read_line},
#if !defined(_WIN32) && !defined(_WIN64)
    {"do_getppid", (DL_FUNC) &do_getppid},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
