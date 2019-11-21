#include <ctype.h>
#include "search.h"
#include "fsm.h"


static int is_empty(const char *s) {
    while (*s != '\0') {
        if (!isspace((unsigned char)*s))
            return 0;
        s++;
    }
    return 1;
}

SEXP backward_search(SEXP content, SEXP _row, SEXP _col, SEXP _char, SEXP _skip_el) {
    int row = Rf_asInteger(_row);
    int col = Rf_asInteger(_col);
    char c = CHAR(Rf_asChar(_char))[0];
    int skip = Rf_asInteger(_skip_el);

    SEXP loc;

    int i, j, k;
    int maxj;
    SEXP ds;
    const char* d;
    unsigned char dj;
    int found = 0;
    int nparen = 0;
    int in_dquote;
    int in_squote;
    k = 0;

    for (i = row; i>=0; i--) {
        ds = STRING_ELT(content, i);
        d = Rf_translateCharUTF8(ds);
        if (skip && i < row && is_empty(d)) {
            // skip empty row when search backward
            i = -1;
            j = -1;
            break;
        }
        maxj = strlen(d);

        // first search forward until the `col` character (only for the last line)
        // or until a comment sign
        j = 0;
        k = 0;
        in_dquote = 0;
        in_squote = 0;
        while (j < maxj) {
            dj = d[j];
            if (0x80 <= dj && dj <= 0xbf) {
                j++;
                continue;
            }
            if (i == row && k == col) break;

            if (!in_squote && dj == '"') {
                if (in_dquote) {
                    if (j == 0 || d[j - 1] != '\\') {
                        in_dquote = 0;
                    }
                } else {
                    in_dquote = 1;
                }
            } else if (!in_dquote && dj == '\'') {
                if (in_squote) {
                    if (j == 0 || d[j - 1] != '\\') {
                        in_squote = 0;
                    }
                } else {
                    in_squote = 1;
                }
            } else if (!in_dquote && !in_squote && dj == '#') {
                break;
            }
            j++;
            k++;
        }

        // then search backward until an unbalanced "("
        in_dquote = 0;
        in_squote = 0;
        nparen = 0;
        while (j >= 0) {
            dj = d[j];
            if (0x80 <= dj && dj <= 0xbf) {
                j--;
                continue;
            }
            if (!in_squote && dj == '"') {
                if (in_dquote) {
                    if (j == 0 || d[j - 1] != '\\') {
                        in_dquote = 0;
                    }
                } else {
                    in_dquote = 1;
                }
            } else if (!in_dquote && dj == '\'') {
                if (in_squote) {
                    if (j == 0 || d[j - 1] != '\\') {
                        in_squote = 0;
                    }
                } else {
                    in_squote = 1;
                }
            } else if (!in_dquote && !in_squote && c == '(') {
                if (nparen == 0 && dj == '(') {
                    found = 1;
                    break;
                } else if (dj == '(') {
                    nparen++;
                } else if (dj == ')') {
                    nparen--;
                }
            }
            j--;
            k--;
        }
        if (found) break;
        // if nothing was found, continue to the previous line
    }

    loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = i;
    INTEGER(loc)[1] = k;
    UNPROTECT(1);
    return loc;
}


SEXP enclosed_by_quotes(SEXP s, SEXP _col) {
    int col = Rf_asInteger(_col);
    SEXP ds = STRING_ELT(s, 0);
    const char* d = Rf_translateCharUTF8(ds);
    unsigned char dj;
    int n = strlen(d);

    // search forward until the `pos` character or until a comment sign
    int j = 0;
    int k = 0;
    fsm_state state = fsm_initialize();

    while (j < n && k <= col) {
        dj = d[j];
        if (0x80 <= dj && dj <= 0xbf) {
            j++;
            continue;
        }
        fsm_feed(&state, dj);
        j++;
        k++;
    }

    int enclosed = state.single_quoted || state.doubled_quoted;
    return Rf_ScalarLogical(enclosed);
}
