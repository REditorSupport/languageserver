#include "signature.h"
#include "fsm.h"

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
            (text[at + 1] == '"' || text[at + 1] == '\'') &&
            (at == 0 || !identifier(text[at - 1]))) {
        char raw_quote = text[at + 1];
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
                if (j < size && j - i - 1 == dashes && text[j] == raw_quote) {
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
        if (text[open] == '#') {
            while (open < size && text[open] != '\n') open++;
        } else {
            int end = literal_end(text, open, size);
            open = end > open ? end : open + 1;
        }
    }

    /* A comma provides a cheap upper bound; actual ranges are only recorded
     * for separators outside nested expressions, comments and strings. */
    int capacity = 1;
    for (int i = open; i < size; i++) if (text[i] == ',') capacity++;
    int *starts = (int *) R_alloc((size_t) capacity, sizeof(int));
    int *ends = (int *) R_alloc((size_t) capacity, sizeof(int));
    int *name_ends = (int *) R_alloc((size_t) capacity, sizeof(int));
    int count = 0;
    int left = -1;
    int right = -1;
    int name_end = -1;
    int depth = 0;
    int closed = 0;
    for (int i = open + 1; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int end = literal_end(text, i, size);
        if (end > i) {
            if (left < 0) left = i;
            right = end;
            i = end - 1;
            continue;
        }
        char ch = text[i];
        if (ch == '#') {
            while (i < size && text[i] != '\n') i++;
            continue;
        }
        if ((ch == ',' || ch == ')') && depth == 0) {
            if (left >= 0) {
                starts[count] = left;
                ends[count] = right;
                name_ends[count++] = name_end >= 0 ? name_end : right;
            }
            if (ch == ')') {
                closed = 1;
                break;
            }
            left = -1;
            right = -1;
            name_end = -1;
            continue;
        }
        if (ch == '=' && depth == 0 && name_end < 0) {
            name_end = right >= 0 ? right : i;
        } else if (ch == '(' || ch == '[' || ch == '{') {
            depth++;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            depth--;
        }
        if (!space((unsigned char) ch)) {
            if (left < 0) left = i;
            right = i + 1;
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

/* Inspect the tokens immediately before the call head. Walking back through
 * names and accessors handles qualified functions without inheriting the pipe
 * of an enclosing call. Comments and quoted operator text are not operators. */
static int call_pipe(SEXP content, int row, int col) {
    enum { OTHER, NAME, ACCESS, NATIVE_PIPE, MAGRITTR_PIPE };
    int want_name = 1;
    for (int line = row; line >= 0; line--) {
        if (STRING_ELT(content, line) == NA_STRING) return 0;
        const char *text = Rf_translateCharUTF8(STRING_ELT(content, line));
        int size = (int) strlen(text);
        if (line == row) size = col;
        int *tokens = (int *) R_alloc((size_t) size + 1, sizeof(int));
        int count = 0;
        for (int i = 0; i < size;) {
            if ((i & 65535) == 0) R_CheckUserInterrupt();
            if (space(text[i])) { i++; continue; }
            if (text[i] == '#') break;
            int end = literal_end(text, i, size);
            int token = OTHER;
            if (end > i) {
                token = NAME;
            } else if (identifier(text[i])) {
                token = NAME;
                end = i + 1;
                while (end < size && identifier(text[end])) end++;
            } else if (text[i] == '$' || text[i] == '@') {
                token = ACCESS;
                end = i + 1;
            } else if (i + 1 < size && text[i] == ':' && text[i + 1] == ':') {
                token = ACCESS;
                end = i + 2;
                if (end < size && text[end] == ':') end++;
            } else if (i + 1 < size && text[i] == '|' && text[i + 1] == '>') {
                token = NATIVE_PIPE;
                end = i + 2;
            } else if (text[i] == '%') {
                end = i + 1;
                while (end < size && text[end] != '%') end++;
                if (end < size) end++;
                if ((end - i == 3 && memcmp(text + i, "%>%", 3) == 0) ||
                        (end - i == 4 && (memcmp(text + i, "%<>%", 4) == 0 ||
                            memcmp(text + i, "%T>%", 4) == 0))) {
                    token = MAGRITTR_PIPE;
                }
            } else {
                end = i + 1;
            }
            tokens[count++] = token;
            i = end;
        }
        for (int i = count - 1; i >= 0; i--) {
            if (want_name && tokens[i] == NAME) want_name = 0;
            else if (!want_name && tokens[i] == ACCESS) want_name = 1;
            else if (!want_name && tokens[i] == NATIVE_PIPE) return 1;
            else if (!want_name && tokens[i] == MAGRITTR_PIPE) return 2;
            else return 0;
        }
    }
    return 0;
}

/* Only piped calls need text after the cursor. Find their closing parenthesis
 * before allocating, so a short call does not copy the rest of the document. */
static const char *pipe_call_text(SEXP content, int row, int col) {
    fsm_state state;
    fsm_initialize(&state);
    int depth = 0;
    int last_row = row;
    int last_col = col;
    size_t size = 0;
    for (int line = row; line < Rf_length(content); line++) {
        if (STRING_ELT(content, line) == NA_STRING) break;
        const char *text = Rf_translateCharUTF8(STRING_ELT(content, line));
        int start = line == row ? col : 0;
        int i = start;
        for (; text[i]; i++) {
            if ((i & 65535) == 0) R_CheckUserInterrupt();
            char ch = text[i];
            if (!state.single_quoted && !state.double_quoted && !state.backticked) {
                if (ch == '#') { i += (int) strlen(text + i); break; }
                if (ch == ')' && depth == 0) break;
                if (ch == '(' || ch == '[' || ch == '{') depth++;
                else if (ch == ')' || ch == ']' || ch == '}') depth--;
            }
            fsm_feed(&state, ch);
        }
        size += (size_t) (i - start) + (line > row);
        if (size > INT_MAX) Rf_error("call text is too large");
        last_row = line;
        last_col = i;
        if (text[i] == ')') break;
        fsm_feed(&state, '\n');
    }
    char *result = (char *) R_alloc(size + 1, sizeof(char));
    size_t at = 0;
    for (int line = row; line <= last_row; line++) {
        const char *text = Rf_translateCharUTF8(STRING_ELT(content, line));
        int start = line == row ? col : 0;
        int end = line == last_row ? last_col : (int) strlen(text);
        if (line > row) result[at++] = '\n';
        memcpy(result + at, text + start, (size_t) (end - start));
        at += (size_t) (end - start);
    }
    result[at] = '\0';
    return result;
}

static int parameter_match(const char *text, int start, int end, SEXP parameters) {
    while (start < end && space(text[start])) start++;
    while (end > start && space(text[end - 1])) end--;
    for (int i = 0; i < Rf_length(parameters); i++) {
        if (STRING_ELT(parameters, i) != NA_STRING && name_matches(
                text, start, end, Rf_translateCharUTF8(STRING_ELT(parameters, i)))) {
            return i;
        }
    }
    return -1;
}

static int pipe_active_parameter(const char *text, int cursor, SEXP parameters, int pipe) {
    int size = (int) strlen(text);
    int nparameters = Rf_length(parameters);
    int *used = (int *) R_alloc((size_t) nparameters + 1, sizeof(int));
    memset(used, 0, ((size_t) nparameters + 1) * sizeof(int));
    int depth = 0, start = 0, name_end = -1;
    int value_tokens = 0, placeholder = 0, has_placeholder = 0;
    int positional = 0, active_name = -1, active = 0;
    for (int i = 0; i <= size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        char ch = text[i];
        if (i == size || (ch == ',' && depth == 0)) {
            int matched = name_end < 0 ? -1 : parameter_match(text, start, name_end, parameters);
            int current = start <= cursor && cursor <= i;
            if (matched >= 0) used[matched] = 1;
            if (current && name_end >= 0 && name_end < cursor) active_name = matched;
            if (i < cursor) {
                active++;
                if (name_end < 0) positional++;
            }
            if (placeholder && value_tokens == 1 && (pipe == 2 || name_end >= 0)) {
                has_placeholder = 1;
            }
            start = i + 1;
            name_end = -1;
            value_tokens = placeholder = 0;
            continue;
        }
        if (space(ch)) continue;
        if (ch == '#') {
            int comment = i;
            while (i < size && text[i] != '\n') i++;
            int before = start;
            while (before < comment && space(text[before])) before++;
            if (before == comment) start = i;
            i--;
            continue;
        }
        int end = literal_end(text, i, size);
        if (end > i) {
            value_tokens++;
            i = end - 1;
            continue;
        }
        if (ch == '=' && depth == 0 && name_end < 0 &&
                text[i + 1] != '=' && (i == 0 || (text[i - 1] != '=' &&
                    text[i - 1] != '!' && text[i - 1] != '<' && text[i - 1] != '>'))) {
            name_end = i;
            value_tokens = placeholder = 0;
            continue;
        }
        value_tokens++;
        if (identifier(ch)) {
            end = i + 1;
            while (end < size && identifier(text[end])) end++;
            if (depth == 0 && value_tokens == 1 && end == i + 1 &&
                    ch == (pipe == 1 ? '_' : '.')) placeholder = 1;
            i = end - 1;
        } else if (ch == '(' || ch == '[' || ch == '{') depth++;
        else if (ch == ')' || ch == ']' || ch == '}') depth--;
    }
    if (active_name >= 0) return active_name;
    positional += !has_placeholder;
    for (int i = 0; i < nparameters; i++) {
        if (STRING_ELT(parameters, i) != NA_STRING &&
                strcmp(CHAR(STRING_ELT(parameters, i)), "...") == 0) return i;
        if (!used[i] && positional-- == 0) return i;
    }
    return active + !has_placeholder;
}

SEXP active_parameter_c(SEXP call_text, SEXP parameters, SEXP content,
    SEXP start_row, SEXP start_col) {
    const char *text = single_string(call_text, "call text");
    if (TYPEOF(parameters) != STRSXP) Rf_error("parameters must be character");
    size_t size_long = strlen(text);
    if (size_long > INT_MAX) Rf_error("call text is too large");
    int size = (int) size_long;
    int row = Rf_asInteger(start_row);
    int col = Rf_asInteger(start_col);
    if (TYPEOF(content) == STRSXP && row >= 0 && row < Rf_length(content) &&
            col >= 0 && STRING_ELT(content, row) != NA_STRING) {
        const char *line = Rf_translateCharUTF8(STRING_ELT(content, row));
        int byte = 0;
        for (int point = 0; line[byte] && point < col; point++) {
            byte++;
            while (((unsigned char) line[byte] & 0xc0) == 0x80) byte++;
        }
        int pipe = line[byte] == '(' ? call_pipe(content, row, byte) : 0;
        if (pipe) return Rf_ScalarInteger(pipe_active_parameter(
            pipe_call_text(content, row, byte + 1), size, parameters, pipe));
    }
    int depth = 0;
    int count = 0;
    int left = -1;
    int right = -1;
    int name_end = -1;
    for (int i = 0; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int end = literal_end(text, i, size);
        if (end > i) {
            if (left < 0) left = i;
            right = end;
            i = end - 1;
            continue;
        }
        char ch = text[i];
        if (ch == '#') {
            while (i < size && text[i] != '\n') i++;
            continue;
        }
        if (ch == ',' && depth == 0) {
            count++;
            left = -1;
            right = -1;
            name_end = -1;
            continue;
        }
        if (ch == '=' && depth == 0 && name_end < 0 &&
                (i + 1 == size || text[i + 1] != '=') &&
                (i == 0 || (text[i - 1] != '=' && text[i - 1] != '!' &&
                    text[i - 1] != '<' && text[i - 1] != '>'))) {
            name_end = right >= 0 ? right : i;
        } else if (ch == '(' || ch == '[' || ch == '{') {
            depth++;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            depth--;
        }
        if (!space((unsigned char) ch)) {
            if (left < 0) left = i;
            right = i + 1;
        }
    }
    if (left >= 0 && name_end > left) {
        for (R_xlen_t i = 0; i < XLENGTH(parameters); i++) {
            if (STRING_ELT(parameters, i) != NA_STRING && name_matches(
                    text, left, name_end,
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
