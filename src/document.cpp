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

    int i, j, k;
    int maxj;
    SEXP ds;
    const char* d;
    unsigned char dj;
    int found = 0;
    int nparen = 0;
    int in_dquote = 0;
    int in_squote = 0;

    for (i = row; i>=0; i--) {
        ds = STRING_ELT(document, i);
        d = Rf_translateCharUTF8(ds);
        maxj = strlen(d);
        j = 0;
        k = 0;
        while (j < maxj) {
            dj = d[j];
            if (dj < 0x80 || 0xbf < dj) {
                if (i == row && k == col) break;
                k++;
            }
            j++;
        }
        while (j >= 0) {
            dj = d[j];
            if (0x80 <= dj && dj <= 0xbf) {
                j--;
                continue;
            }
            // TODO: handle comments
            // TODO: handle escaped quotes within quotes
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
            j--;
            k--;
        }
        if (found) break;
    }

    loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = i;
    INTEGER(loc)[1] = k;
    UNPROTECT(1);
    return loc;
}
