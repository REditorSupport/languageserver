#include "signature.h"

#include <limits.h>
#include <string.h>

static int space(unsigned char ch) {
    return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' ||
        ch == '\v' || ch == '\f';
}

static int identifier(unsigned char ch) {
    return ch >= 128 || (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') ||
        ch == '_' || ch == '.';
}

/* Return the first byte after a quoted or raw string, or at if not a string.
 * Parsing source text never evaluates defaults or user expressions. */
static int literal_end(const char *text, int at, int size) {
    unsigned char quote = (unsigned char) text[at];
    if ((quote == 'r' || quote == 'R') && at + 1 < size &&
            text[at + 1] == '"' && (at == 0 || !identifier(text[at - 1]))) {
        int open = at + 2;
        while (open < size && text[open] == '-') open++;
        if (open < size && (text[open] == '(' || text[open] == '[' ||
                text[open] == '{')) {
            char close = text[open] == '(' ? ')' : text[open] == '[' ? ']' : '}';
            int dashes = open - at - 2;
            for (int i = open + 1; i < size; i++) {
                if ((i & 65535) == 0) R_CheckUserInterrupt();
                if (text[i] != close) continue;
                int j = i + 1;
                while (j < size && j - i - 1 < dashes && text[j] == '-') j++;
                if (j < size && j - i - 1 == dashes && text[j] == '"') {
                    return j + 1;
                }
            }
            return size;
        }
    }
    if (quote != '\'' && quote != '"' && quote != '`') return at;
    for (int i = at + 1; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        if (text[i] == '\\' && i + 1 < size) i++;
        else if ((unsigned char) text[i] == quote) return i + 1;
    }
    return size;
}

static const char *single_string(SEXP value, const char *name) {
    if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1 ||
            STRING_ELT(value, 0) == NA_STRING) {
        Rf_error("%s must be a non-missing character string", name);
    }
    return Rf_translateCharUTF8(STRING_ELT(value, 0));
}

/* Count UTF-16 units in successive byte ranges, without allocating a mapping
 * for every character of a potentially very large default expression. */
static int utf16_advance(const char *text, int *cursor, int end, int units) {
    while (*cursor < end) {
        unsigned char ch = (unsigned char) text[(*cursor)++];
        if ((ch & 0xc0) != 0x80) units += ch >= 0xf0 ? 2 : 1;
    }
    return units;
}

SEXP signature_info_c(SEXP signature) {
    const char *text = single_string(signature, "signature");
    size_t size_long = strlen(text);
    if (size_long > INT_MAX) Rf_error("signature is too large");
    int size = (int) size_long;
    int open = 0;
    while (open < size && text[open] != '(') {
        int end = literal_end(text, open, size);
        open = end > open ? end : open + 1;
    }

    /* A comma provides a cheap upper bound; actual ranges are only recorded
     * for separators outside nested expressions, comments and strings. */
    int capacity = 1;
    for (int i = open; i < size; i++) if (text[i] == ',') capacity++;
    int *starts = (int *) R_alloc((size_t) capacity, sizeof(int));
    int *ends = (int *) R_alloc((size_t) capacity, sizeof(int));
    int *name_ends = (int *) R_alloc((size_t) capacity, sizeof(int));
    int count = 0;
    int start = open + 1;
    int name_end = -1;
    int depth = 0;
    int closed = 0;
    for (int i = start; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int end = literal_end(text, i, size);
        if (end > i) {
            i = end - 1;
            continue;
        }
        char ch = text[i];
        if (ch == '#') {
            while (i < size && text[i] != '\n') i++;
            continue;
        }
        if ((ch == ',' || ch == ')') && depth == 0) {
            int left = start;
            int right = i;
            while (left < right && space(text[left])) left++;
            while (right > left && space(text[right - 1])) right--;
            if (left < right) {
                starts[count] = left;
                ends[count] = right;
                int named = name_end < 0 ? right : name_end;
                while (named > left && space(text[named - 1])) named--;
                name_ends[count++] = named;
            }
            if (ch == ')') {
                closed = 1;
                break;
            }
            start = i + 1;
            name_end = -1;
        } else if (ch == '=' && depth == 0 && name_end < 0) {
            name_end = i;
        } else if (ch == '(' || ch == '[' || ch == '{') {
            depth++;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            depth--;
        }
    }
    if (!closed) count = 0;

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(result_names, 0, Rf_mkChar("names"));
    SET_STRING_ELT(result_names, 1, Rf_mkChar("parameters"));
    Rf_setAttrib(result, R_NamesSymbol, result_names);
    SEXP names = PROTECT(Rf_allocVector(STRSXP, count));
    SEXP parameters = PROTECT(Rf_allocVector(VECSXP, count));
    SEXP label_name = PROTECT(Rf_mkString("label"));
    int byte_cursor = 0;
    int units = 0;
    for (int i = 0; i < count; i++) {
        SET_STRING_ELT(names, i, Rf_mkCharLenCE(
            text + starts[i], name_ends[i] - starts[i], CE_UTF8));
        SEXP parameter = PROTECT(Rf_allocVector(VECSXP, 1));
        SEXP label = PROTECT(Rf_allocVector(INTSXP, 2));
        units = utf16_advance(text, &byte_cursor, starts[i], units);
        INTEGER(label)[0] = units;
        units = utf16_advance(text, &byte_cursor, ends[i], units);
        INTEGER(label)[1] = units;
        SET_VECTOR_ELT(parameter, 0, label);
        Rf_setAttrib(parameter, R_NamesSymbol, label_name);
        SET_VECTOR_ELT(parameters, i, parameter);
        UNPROTECT(2);
    }
    SET_VECTOR_ELT(result, 0, names);
    SET_VECTOR_ELT(result, 1, parameters);
    UNPROTECT(5);
    return result;
}

static int name_matches(const char *text, int left, int right, const char *name) {
    int name_size = (int) strlen(name);
    int name_left = 0;
    if (right > left + 1 && (text[left] == '`' || text[left] == '\'' ||
            text[left] == '"') && text[right - 1] == text[left]) {
        left++;
        right--;
    }
    if (name_size > 1 && (name[0] == '`' || name[0] == '\'' ||
            name[0] == '"') && name[name_size - 1] == name[0]) {
        name_left++;
        name_size--;
    }
    while (left < right && name_left < name_size) {
        if (text[left] == '\\' && left + 1 < right) left++;
        if (name[name_left] == '\\' && name_left + 1 < name_size) name_left++;
        if (text[left++] != name[name_left++]) return 0;
    }
    return left == right && name_left == name_size;
}

SEXP active_parameter_c(SEXP call_text, SEXP parameters) {
    const char *text = single_string(call_text, "call text");
    if (TYPEOF(parameters) != STRSXP) Rf_error("parameters must be character");
    size_t size_long = strlen(text);
    if (size_long > INT_MAX) Rf_error("call text is too large");
    int size = (int) size_long;
    int depth = 0;
    int count = 0;
    int start = 0;
    int name_end = -1;
    for (int i = 0; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int end = literal_end(text, i, size);
        if (end > i) {
            i = end - 1;
            continue;
        }
        char ch = text[i];
        if (ch == '#') {
            int comment = i;
            while (i < size && text[i] != '\n') i++;
            int before = start;
            while (before < comment && space(text[before])) before++;
            if (before == comment) start = i;
        } else if (ch == ',' && depth == 0) {
            count++;
            start = i + 1;
            name_end = -1;
        } else if (ch == '=' && depth == 0 && name_end < 0 &&
                (i + 1 == size || text[i + 1] != '=') &&
                (i == 0 || (text[i - 1] != '=' && text[i - 1] != '!' &&
                    text[i - 1] != '<' && text[i - 1] != '>'))) {
            name_end = i;
        } else if (ch == '(' || ch == '[' || ch == '{') {
            depth++;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            depth--;
        }
    }
    if (name_end >= 0) {
        while (start < name_end && space(text[start])) start++;
        while (name_end > start && space(text[name_end - 1])) name_end--;
        for (R_xlen_t i = 0; i < XLENGTH(parameters); i++) {
            if (STRING_ELT(parameters, i) != NA_STRING && name_matches(
                    text, start, name_end,
                    Rf_translateCharUTF8(STRING_ELT(parameters, i)))) {
                return Rf_ScalarInteger((int) i);
            }
        }
    }
    for (R_xlen_t i = 0; i < XLENGTH(parameters); i++) {
        if (STRING_ELT(parameters, i) != NA_STRING &&
                strcmp(CHAR(STRING_ELT(parameters, i)), "...") == 0 &&
                count >= i) return Rf_ScalarInteger((int) i);
    }
    return Rf_ScalarInteger(count);
}
