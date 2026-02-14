#include "token.h"

#include <ctype.h>
#include <string.h>
#include <wctype.h>
#include <stdint.h>

static int utf8_decode(const char *text, int len, int idx, unsigned int *cp) {
    unsigned char c = (unsigned char) text[idx];

    if (c < 0x80) {
        *cp = c;
        return idx + 1;
    }

    if ((c & 0xE0) == 0xC0 && idx + 1 < len) {
        unsigned char c1 = (unsigned char) text[idx + 1];
        if ((c1 & 0xC0) == 0x80) {
            *cp = ((c & 0x1F) << 6) | (c1 & 0x3F);
            return idx + 2;
        }
    }

    if ((c & 0xF0) == 0xE0 && idx + 2 < len) {
        unsigned char c1 = (unsigned char) text[idx + 1];
        unsigned char c2 = (unsigned char) text[idx + 2];
        if (((c1 & 0xC0) == 0x80) && ((c2 & 0xC0) == 0x80)) {
            *cp = ((c & 0x0F) << 12) | ((c1 & 0x3F) << 6) | (c2 & 0x3F);
            return idx + 3;
        }
    }

    if ((c & 0xF8) == 0xF0 && idx + 3 < len) {
        unsigned char c1 = (unsigned char) text[idx + 1];
        unsigned char c2 = (unsigned char) text[idx + 2];
        unsigned char c3 = (unsigned char) text[idx + 3];
        if (((c1 & 0xC0) == 0x80) && ((c2 & 0xC0) == 0x80) && ((c3 & 0xC0) == 0x80)) {
            *cp = ((c & 0x07) << 18) | ((c1 & 0x3F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F);
            return idx + 4;
        }
    }

    *cp = 0;
    return idx + 1;
}

static int utf8_prev_index(const char *text, int idx) {
    if (idx <= 0) {
        return 0;
    }
    int i = idx - 1;
    while (i > 0 && ((unsigned char) text[i] & 0xC0) == 0x80) {
        i--;
    }
    return i;
}

static int byte_index_for_cp(const char *text, int len, int cp_index) {
    if (cp_index <= 0) {
        return 0;
    }
    int i = 0;
    int count = 0;
    unsigned int cp = 0;
    while (i < len && count < cp_index) {
        i = utf8_decode(text, len, i, &cp);
        count++;
    }
    return i;
}

static int is_token_char_cp(unsigned int cp) {
    if (cp == '.' || cp == '_') {
        return 1;
    }
    if (cp <= (unsigned int) WINT_MAX) {
        return iswalnum((wint_t) cp);
    }
    return 0;
}

static int is_token_start_cp(unsigned int cp) {
    if (cp == '.') {
        return 1;
    }
    if (cp <= (unsigned int) WINT_MAX) {
        return iswalpha((wint_t) cp);
    }
    return 0;
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
    int col_idx = INTEGER(col)[0];
    int do_forward = Rf_asLogical(forward);

    if (col_idx < 0) {
        col_idx = 0;
    }

    int right_start = byte_index_for_cp(text, len, col_idx);
    if (right_start > len) {
        right_start = len;
    }

    int right_end = right_start;
    if (do_forward) {
        unsigned int cp = 0;
        int next = right_end;
        while (next < len) {
            int tmp = utf8_decode(text, len, next, &cp);
            if (!is_token_char_cp(cp)) {
                break;
            }
            next = tmp;
        }
        right_end = next;
    }

    int right_len = right_end - right_start;
    int end = do_forward ? right_end : right_start;

    SEXP full_token = Rf_mkString("");
    SEXP right_token = make_str(text, right_start, right_len);
    SEXP package = Rf_mkString("");
    SEXP accessor = Rf_mkString("");
    SEXP token = Rf_mkString("");

    if (right_len == 0) {
        int acc_len = 0;
        if (right_start >= 2 && text[right_start - 1] == ':' && text[right_start - 2] == ':') {
            acc_len = 2;
            if (right_start >= 3 && text[right_start - 3] == ':') {
                acc_len = 3;
            }
        }

        if (acc_len > 0) {
            int acc_start = right_start - acc_len;
            int pkg_end = acc_start - 1;
            if (pkg_end >= 0) {
                int p = pkg_end;
                while (p >= 0) {
                    unsigned char ch = (unsigned char) text[p];
                    if (ch & 0x80) {
                        break;
                    }
                    if (!(isalnum(ch) || ch == '.')) {
                        break;
                    }
                    p--;
                }
                int pkg_start = p + 1;
                int pkg_len = pkg_end - pkg_start + 1;
                if (pkg_len >= 2 && isalpha((unsigned char) text[pkg_start])) {
                    package = make_str(text, pkg_start, pkg_len);
                    accessor = make_str(text, acc_start, acc_len);
                    full_token = make_str(text, pkg_start, right_start - pkg_start);
                }
            }
        }
    }

    if (end > 0) {
        if (end > len) {
            end = len;
        }

        int end_idx = utf8_prev_index(text, end);
        if (end_idx >= 0 && end_idx < len) {
            unsigned int cp = 0;
            utf8_decode(text, len, end_idx, &cp);

            if (is_token_char_cp(cp)) {
                int start = end_idx;
                while (start > 0) {
                    int prev = utf8_prev_index(text, start);
                    unsigned int prev_cp = 0;
                    utf8_decode(text, len, prev, &prev_cp);
                    if (!is_token_char_cp(prev_cp)) {
                        break;
                    }
                    start = prev;
                }

                unsigned int start_cp = 0;
                utf8_decode(text, len, start, &start_cp);
                if (is_token_start_cp(start_cp)) {
                    if (!(start > 0 && text[start - 1] == '$')) {
                        int token_len = end - start;
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
                                while (p >= 0) {
                                    unsigned char ch = (unsigned char) text[p];
                                    if (ch & 0x80) {
                                        break;
                                    }
                                    if (!(isalnum(ch) || ch == '.')) {
                                        break;
                                    }
                                    p--;
                                }
                                int pkg_start = p + 1;
                                int pkg_len = pkg_end - pkg_start + 1;

                                if (pkg_len >= 2 && isalpha((unsigned char) text[pkg_start])) {
                                    package = make_str(text, pkg_start, pkg_len);
                                    accessor = make_str(text, acc_start, acc_len);
                                    full_token = make_str(text, pkg_start, end - pkg_start);
                                }
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
