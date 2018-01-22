#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <regex>

extern "C" {
#include "languageserver.h"
}

SEXP document_backward_search(SEXP document, SEXP _row, SEXP _col, SEXP _char) {
    int row = Rf_asInteger(_row);
    int col = Rf_asInteger(_col);
    char c = CHAR(Rf_asChar(_char))[0];
    SEXP loc;

    int i, j;
    SEXP ds;
    const char* d;
    char dj;
    int found = 0;
    int nparen = 0;
    int in_dquote = 0;
    int in_squote = 0;

    for (i = row; i>=0; i--) {
        ds = STRING_ELT(document, i);
        d = Rf_translateCharUTF8(ds);
        if (i < row) col = R_nchar(ds, Bytes, FALSE, FALSE, "error");
        for (j = col; j>=0; j--) {
            dj = d[j];
            // TODO: handle comments
            if (!in_dquote && !in_squote) {
                if (nparen == 0 && dj == c) {
                    found = 1;
                    break;
                } else if (dj == '(') {
                    nparen ++;
                } else if (dj == ')') {
                    nparen --;
                }
            } else if (!in_squote && dj == '"') {
                in_dquote = 1 - in_dquote;
            } else if (!in_dquote && dj == '\'') {
                in_squote = 1 - in_squote;
            }
        }
        if (found) break;
    }

    loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = i;
    INTEGER(loc)[1] = j;
    UNPROTECT(1);
    return loc;
}
