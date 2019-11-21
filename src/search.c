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
    char chr = CHAR(Rf_asChar(_char))[0];
    int skip = Rf_asInteger(_skip_el);

    int i, j, k;
    int n;
    const char* c;
    unsigned char cj;
    fsm_state state;

    int found = 0;
    int nparen = 0;
    int in_dquote;
    int in_squote;

    for (i = row; i >= 0; i--) {
        c = Rf_translateCharUTF8(STRING_ELT(content, i));
        if (skip && i < row && is_empty(c)) {
            // skip empty row when search backward
            i = -1;
            j = -1;
            break;
        }

        n = strlen(c);
        // search forward until the `col` character or # sign
        j = 0;
        k = 0;
        state = fsm_initialize();

        while (j < n && (i < row || k <= col)) {
            cj = c[j];
            if (0x80 <= cj && cj <= 0xbf) {
                j++;
                continue;
            }
            if (!state.single_quoted && !state.double_quoted && cj == '#') break;
            fsm_feed(&state, cj);
            j++;
            k++;
        }
        j--;
        k--;

        // then search backward until an unbalanced "("
        in_dquote = 0;
        in_squote = 0;
        nparen = 0;
        while (j >= 0) {
            cj = c[j];
            if (0x80 <= cj && cj <= 0xbf) {
                j--;
                continue;
            }
            if (!in_squote && cj == '"') {
                if (in_dquote) {
                    if (j == 0 || c[j - 1] != '\\') {
                        in_dquote = 0;
                    }
                } else {
                    in_dquote = 1;
                }
            } else if (!in_dquote && cj == '\'') {
                if (in_squote) {
                    if (j == 0 || c[j - 1] != '\\') {
                        in_squote = 0;
                    }
                } else {
                    in_squote = 1;
                }
            } else if (!in_dquote && !in_squote && chr == '(') {
                if (nparen == 0 && cj == '(') {
                    found = 1;
                    break;
                } else if (cj == '(') {
                    nparen++;
                } else if (cj == ')') {
                    nparen--;
                }
            }
            j--;
            k--;
        }
        if (found) break;
        // if nothing was found, continue to the previous line
    }

    SEXP loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = i;
    INTEGER(loc)[1] = k;
    UNPROTECT(1);
    return loc;
}


SEXP enclosed_by_quotes(SEXP _s, SEXP _col) {
    int col = Rf_asInteger(_col);
    const char* c = Rf_translateCharUTF8(STRING_ELT(_s, 0));
    unsigned char cj;
    int n = strlen(c);

    // search forward until the `pos` character
    int j = 0;
    int k = 0;
    fsm_state state = fsm_initialize();

    while (j < n && k <= col) {
        cj = c[j];
        if (0x80 <= cj && cj <= 0xbf) {
            j++;
            continue;
        }
        fsm_feed(&state, cj);
        j++;
        k++;
    }

    int enclosed = state.single_quoted || state.double_quoted;
    return Rf_ScalarLogical(enclosed);
}
