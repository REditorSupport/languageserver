#include <ctype.h>
#include "search.h"
#include "fsm.h"
#include "stack.h"


static int is_empty(const char *s) {
    while (*s != '\0') {
        if (!isspace((unsigned char)*s))
            return 0;
        s++;
    }
    return 1;
}

SEXP find_unbalanced_bracket(SEXP content, SEXP _row, SEXP _col, SEXP _skip_el) {
    int row = Rf_asInteger(_row);
    int col = Rf_asInteger(_col);
    int skip = Rf_asInteger(_skip_el);

    int i, j, k;
    int n;
    const char* c;
    unsigned char cj;
    fsm_state state;
    stack pos;
    stack codept_pos;
    int nbracket = 0;
    int nunbalanced = 0;
    char brac[2] = " \x00";

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
        fsm_initialize(&state);
        stack_initialize(&pos);
        stack_initialize(&codept_pos);
        while (j < n && (i < row || k <= col)) {
            cj = c[j];
            if (0x80 <= cj && cj <= 0xbf) {
                j++;
                continue;
            }
            if (!state.single_quoted && !state.double_quoted && !state.backticked & !state.escaped) {
                if (cj == '#') {
                    break;
                } else if (cj == '(' || cj == '[' || cj == '{') {
                    nbracket += 1;
                    stack_push(&pos, j);
                    stack_push(&codept_pos, k);
                } else if (cj == ')' || cj == ']' || cj == '}') {
                    if (nbracket > 0) {
                        nbracket -= 1;
                        stack_pop(&pos);
                        stack_pop(&codept_pos);
                    } else {
                        nunbalanced += 1;
                    }
                }
            }
            fsm_feed(&state, cj);
            j++;
            k++;
        }
        while (nunbalanced > 0 && nbracket > 0) {
            stack_pop(&pos);
            stack_pop(&codept_pos);
            nunbalanced -= 1;
            nbracket -= 1;
        }
        j = stack_pop(&pos);
        k = stack_pop(&codept_pos);
        stack_clear(&pos);
        stack_clear(&codept_pos);
        if (nbracket >= 1 && k >= 0) {
            brac[0] = c[j];
            break;
        }
        if (i < row && (state.single_quoted || state.double_quoted)) {
            // do not search further if an unmatched quote is detected
            i = k = -1;
            break;
        }
    }
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = i;
    INTEGER(loc)[1] = k;
    SET_VECTOR_ELT(out, 0, loc);
    SEXP bracket = PROTECT(Rf_mkString((const char*) brac));
    SET_VECTOR_ELT(out, 1, bracket);
    UNPROTECT(3);
    return out;
}


SEXP enclosed_by_quotes(SEXP _s, SEXP _col) {
    int col = Rf_asInteger(_col);
    const char* c = Rf_translateCharUTF8(STRING_ELT(_s, 0));
    unsigned char cj;
    int n = strlen(c);

    // search forward until the `col` character or # sign
    int j = 0;
    int k = 0;

    fsm_state state;
    fsm_initialize(&state);
    while (j < n && k <= col) {
        cj = c[j];
        if (0x80 <= cj && cj <= 0xbf) {
            j++;
            continue;
        }
        if (!state.single_quoted && !state.double_quoted && !state.backticked && !state.escaped && cj == '#') break;
        fsm_feed(&state, cj);
        j++;
        k++;
    }

    int enclosed = state.single_quoted || state.double_quoted;
    return Rf_ScalarLogical(enclosed);
}

SEXP detect_comments(SEXP content, SEXP _row) {
    int row = Rf_asInteger(_row);
    int out = row;
    int i = row - 1;
    int is_comment = 0;
    const char *c;
    while (i >= 0) {
        c = Rf_translateCharUTF8(STRING_ELT(content, i));
        is_comment = 0;
        while (*c != '\0') {
            if (isspace((unsigned char)*c)) {
            } else if (*c == '#') {
                is_comment = 1;
                out = i;
                break;
            } else {
                break;
            }
            c++;
        }
        if (!is_comment) {
            break;
        }
        i--;
    }
    return Rf_ScalarInteger(out);
}
