#include "document.h"

#if defined(_WIN32) || defined(_WIN64)

#include <string.h>
#include <windows.h>

char* buf = NULL;
int bufsize = 1024;
int bufpos = 0;

SEXP stdin_read_char(SEXP _n) {
    int n = Rf_asInteger(_n);
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    DWORD bytes_avail, bytes_read;

    if (buf == NULL) {
        buf = (char*) malloc((bufsize + 1) * sizeof(char));
    }
    while (bufsize < n) {
        bufsize = bufsize * 2;
        buf = (char*) realloc(buf, (bufsize + 1) * sizeof(char));
    }

    if (PeekNamedPipe(h, NULL, 0, NULL, &bytes_avail, NULL)) {
        if (bytes_avail > 0) {
            ReadFile(h, buf, n, &bytes_read, NULL);
            buf[bytes_read] = '\0';
            return Rf_mkString(buf);
        }
        return Rf_allocVector(STRSXP, 0);
    } else {
        Rf_error("stdin connection close");
    }
}


SEXP stdin_read_line() {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    DWORD bytes_avail, bytes_read;
    char c;

    if (buf == NULL) {
        buf = (char*) malloc((bufsize + 1) * sizeof(char));
    }

    if (PeekNamedPipe(h, NULL, 0, NULL, &bytes_avail, NULL)) {
        if (bytes_avail > 0) {
            while (bytes_avail > bufpos) {
                ReadFile(h, &c, 1, &bytes_read, NULL);
                if (bytes_read == 1) {
                    buf[bufpos] = c;
                    if (c == '\n') {
                        buf[bufpos] = '\0';
                        if (bufpos > 0 && buf[bufpos - 1] == '\r') {
                            buf[bufpos - 1] = '\0';
                        }
                        bufpos = 0;
                        return Rf_mkString(buf);
                    }
                    bufpos++;
                    if (bufpos >= bufsize) {
                        bufsize = bufsize * 2;
                        buf = realloc(buf, (bufsize + 1) * sizeof(char));
                    }
                } else {
                    Rf_error("unexpected error");
                }
            }
        }
        return Rf_allocVector(STRSXP, 0);
    } else {
        Rf_error("stdin connection close");
    }
}

#else

#include <unistd.h> /* for getppid */

SEXP do_getppid() {
    int ppid;
    ppid = (int) getppid();
    return Rf_ScalarInteger(ppid);
}
#endif

static const R_CallMethodDef CallEntries[] = {
    {"document_backward_search", (DL_FUNC) &document_backward_search, 5},
#if defined(_WIN32) || defined(_WIN64)
    {"stdin_read_char", (DL_FUNC) &stdin_read_char, 1},
    {"stdin_read_line", (DL_FUNC) &stdin_read_line},
#else
    {"do_getppid", (DL_FUNC) &do_getppid},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
