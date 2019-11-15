#include "search.h"
#include "reader.h"


#if !defined(_WIN32) && !defined(_WIN64)

#include <unistd.h> /* for getppid */

static int ppid = -1;

SEXP process_is_detached() {
    if (ppid == -1) {
        ppid = (int) getppid();
    }
    return Rf_ScalarInteger(ppid != (int) getppid());
}
#endif

static const R_CallMethodDef CallEntries[] = {
    {"backward_search", (DL_FUNC) &backward_search, 5},
    {"enclosed_by_quotes", (DL_FUNC) &enclosed_by_quotes, 2},
    {"stdin_read_char", (DL_FUNC) &stdin_read_char, 1},
    {"stdin_read_line", (DL_FUNC) &stdin_read_line},
#if !defined(_WIN32) && !defined(_WIN64)
    {"process_is_detached", (DL_FUNC) &process_is_detached},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
