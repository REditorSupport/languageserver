#include "search.h"
#include "reader.h"

#ifdef _WIN32

# include <fcntl.h>
# include <io.h>
# include <stdio.h>

#else

#include <unistd.h> /* for getppid */

static int ppid = -1;

SEXP process_is_detached(void) {
    if (ppid == -1) {
        ppid = (int) getppid();
    }
    return Rf_ScalarInteger(ppid != (int) getppid());
}
#endif

static const R_CallMethodDef CallEntries[] = {
    {"find_unbalanced_bracket", (DL_FUNC) &find_unbalanced_bracket, 4},
    {"enclosed_by_quotes", (DL_FUNC) &enclosed_by_quotes, 2},
    {"detect_comments", (DL_FUNC) &detect_comments, 2},
    {"stdin_read_char", (DL_FUNC) &stdin_read_char, 1},
    {"stdin_read_line", (DL_FUNC) &stdin_read_line},
#if !defined(_WIN32)
    {"process_is_detached", (DL_FUNC) &process_is_detached},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
#ifdef _WIN32
    _setmode(_fileno(stdout), _O_BINARY);
#endif
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
