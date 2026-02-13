#include "token.h"

#include <ctype.h>
#include <string.h>

static int is_word_char(unsigned char c) {
    return (c == '_' || isalnum(c));
}

static int is_token_char(unsigned char c) {
    return (c == '.' || is_word_char(c));
}

static int is_token_start(unsigned char c) {
    return (c == '.' || isalpha(c));
}

static SEXP make_str(const char *text, int start, int len) {
    if (len <= 0) {
        return Rf_mkString("");
    }
    return Rf_ScalarString(Rf_mkCharLenCE(text + start, len, CE_UTF8));
}

SEXP scan_token_c(SEXP line, SEXP col, SEXP forward) {
    if (!Rf_isString(line) || Rf_length(line) != 1) {
        Rf_error("line must be a single character string");
    }
    if (!Rf_isInteger(col) || Rf_length(col) != 1) {
        Rf_error("col must be a single integer");
    }

    const char *text = Rf_translateCharUTF8(STRING_ELT(line, 0));
    int len = (int) strlen(text);
    int idx = INTEGER(col)[0];
    int do_forward = Rf_asLogical(forward);

    if (idx < 0) {
        idx = 0;
    }
    if (idx > len) {
        idx = len;
    }

    int right_start = idx;
    int right_end = right_start;

    if (do_forward) {
        while (right_end < len && is_token_char((unsigned char) text[right_end])) {
            right_end++;
        }
    }

    int right_len = right_end - right_start;
    int end = idx + right_len;

    SEXP full_token = Rf_mkString("");
    SEXP right_token = make_str(text, right_start, right_len);
    SEXP package = Rf_mkString("");
    SEXP accessor = Rf_mkString("");
    SEXP token = Rf_mkString("");

    if (end > 0) {
        if (end > len) {
            end = len;
        }
        int end_idx = end - 1;

        if (end_idx >= 0 && is_token_char((unsigned char) text[end_idx])) {
            int start = end_idx;
            while (start >= 0 && is_token_char((unsigned char) text[start])) {
                start--;
            }
            start++;

            if (start >= 0 && is_token_start((unsigned char) text[start])) {
                if (!(start > 0 && text[start - 1] == '$')) {
                    int token_len = end_idx - start + 1;
                    token = make_str(text, start, token_len);
                    full_token = token;

                    if (start >= 2 && text[start - 1] == ':' && text[start - 2] == ':') {
                        int acc_len = 2;
                        int acc_start = start - 2;
                        if (start >= 3 && text[start - 3] == ':') {
                            acc_len = 3;
                            acc_start = start - 3;
                        }

                        int pkg_end = acc_start - 1;
                        if (pkg_end >= 0) {
                            int p = pkg_end;
                            while (p >= 0 && (isalnum((unsigned char) text[p]) || text[p] == '.')) {
                                p--;
                            }
                            int pkg_start = p + 1;
                            int pkg_len = pkg_end - pkg_start + 1;

                            if (pkg_len >= 2 && isalpha((unsigned char) text[pkg_start])) {
                                package = make_str(text, pkg_start, pkg_len);
                                accessor = make_str(text, acc_start, acc_len);
                                full_token = make_str(text, pkg_start, end_idx - pkg_start + 1);
                            }
                        }
                    }
                }
            }
        }
    }

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(out, 0, full_token);
    SET_VECTOR_ELT(out, 1, right_token);
    SET_VECTOR_ELT(out, 2, package);
    SET_VECTOR_ELT(out, 3, accessor);
    SET_VECTOR_ELT(out, 4, token);

    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, Rf_mkChar("full_token"));
    SET_STRING_ELT(names, 1, Rf_mkChar("right_token"));
    SET_STRING_ELT(names, 2, Rf_mkChar("package"));
    SET_STRING_ELT(names, 3, Rf_mkChar("accessor"));
    SET_STRING_ELT(names, 4, Rf_mkChar("token"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(2);
    return out;
}
