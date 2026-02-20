#include "match.h"

#include <ctype.h>
#include <string.h>

static unsigned char tolower_ascii(unsigned char c) {
    if (c >= 'A' && c <= 'Z') {
        return (unsigned char) (c + ('a' - 'A'));
    }
    return c;
}

static int contains_case_insensitive(const char *text, const char *pattern, int pat_len) {
    if (pat_len == 0) {
        return 1;
    }

    int text_len = (int) strlen(text);
    if (pat_len > text_len) {
        return 0;
    }

    for (int i = 0; i <= text_len - pat_len; i++) {
        int j = 0;
        while (j < pat_len) {
            unsigned char tc = tolower_ascii((unsigned char) text[i + j]);
            unsigned char pc = tolower_ascii((unsigned char) pattern[j]);
            if (tc != pc) {
                break;
            }
            j++;
        }
        if (j == pat_len) {
            return 1;
        }
    }

    return 0;
}

static int subseq_case_insensitive(const char *text, const char *pattern, int pat_len) {
    if (pat_len == 0) {
        return 1;
    }

    int pi = 0;
    int ti = 0;
    int text_len = (int) strlen(text);

    while (ti < text_len && pi < pat_len) {
        unsigned char tc = tolower_ascii((unsigned char) text[ti]);
        unsigned char pc = tolower_ascii((unsigned char) pattern[pi]);
        if (tc == pc) {
            pi++;
        }
        ti++;
    }

    return (pi == pat_len);
}

SEXP match_with_c(SEXP x, SEXP token) {
    if (!Rf_isString(x)) {
        Rf_error("x must be a character vector");
    }
    if (!Rf_isString(token) || Rf_length(token) != 1) {
        Rf_error("token must be a single character string");
    }

    SEXP out = PROTECT(Rf_allocVector(LGLSXP, Rf_length(x)));
    int *out_ptr = LOGICAL(out);

    if (STRING_ELT(token, 0) == NA_STRING) {
        for (int i = 0; i < Rf_length(x); i++) {
            out_ptr[i] = NA_LOGICAL;
        }
        UNPROTECT(1);
        return out;
    }

    const char *pat = Rf_translateCharUTF8(STRING_ELT(token, 0));
    int pat_len = (int) strlen(pat);

    for (int i = 0; i < Rf_length(x); i++) {
        SEXP item = STRING_ELT(x, i);
        if (item == NA_STRING) {
            out_ptr[i] = NA_LOGICAL;
            continue;
        }
        const char *text = Rf_translateCharUTF8(item);
        out_ptr[i] = contains_case_insensitive(text, pat, pat_len);
    }

    UNPROTECT(1);
    return out;
}

SEXP fuzzy_find_c(SEXP x, SEXP pattern) {
    if (!Rf_isString(x)) {
        Rf_error("x must be a character vector");
    }
    if (!Rf_isString(pattern) || Rf_length(pattern) != 1) {
        Rf_error("pattern must be a single character string");
    }

    SEXP out = PROTECT(Rf_allocVector(LGLSXP, Rf_length(x)));
    int *out_ptr = LOGICAL(out);

    if (STRING_ELT(pattern, 0) == NA_STRING) {
        for (int i = 0; i < Rf_length(x); i++) {
            out_ptr[i] = NA_LOGICAL;
        }
        UNPROTECT(1);
        return out;
    }

    const char *pat = Rf_translateCharUTF8(STRING_ELT(pattern, 0));
    int pat_len = (int) strlen(pat);

    for (int i = 0; i < Rf_length(x); i++) {
        SEXP item = STRING_ELT(x, i);
        if (item == NA_STRING) {
            out_ptr[i] = NA_LOGICAL;
            continue;
        }
        const char *text = Rf_translateCharUTF8(item);
        out_ptr[i] = subseq_case_insensitive(text, pat, pat_len);
    }

    UNPROTECT(1);
    return out;
}
