#include "signature.h"
#include "fsm.h"

#include <R_ext/Parse.h>
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

typedef struct {
    int row;
    int col;
    char bracket;
} signature_bracket;

typedef struct {
    fsm_state state;
    signature_bracket *brackets;
    int depth;
    int capacity;
    int first_row;
    int row;
    int valid;
} signature_call_cache;

static void signature_call_cache_finalizer(SEXP object) {
    signature_call_cache *cache = R_ExternalPtrAddr(object);
    if (cache != NULL) {
        if (cache->brackets != NULL) R_Free(cache->brackets);
        R_Free(cache);
        R_ClearExternalPtr(object);
    }
}

static void signature_call_cache_init(SEXP object) {
    signature_call_cache *cache = R_Calloc(1, signature_call_cache);
    R_SetExternalPtrAddr(object, cache);
    R_RegisterCFinalizerEx(object, signature_call_cache_finalizer, TRUE);
}

SEXP new_signature_call_cache_c(void) {
    SEXP object = PROTECT(R_MakeExternalPtr(NULL,
        Rf_install("languageserver.signature_call_cache"), R_NilValue));
    signature_call_cache_init(object);
    UNPROTECT(1);
    return object;
}

static void signature_call_scan_line(signature_call_cache *cache,
    const char *text, int row, int limit) {
    int col = 0;
    int special = 0;
    for (int i = 0; text[i]; i++) {
        unsigned char ch = (unsigned char) text[i];
        if ((ch & 0xc0) == 0x80) continue;
        if (col >= limit) break;
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int current_col = col++;
        if (special) {
            if (ch == '%') special = 0;
            continue;
        }
        fsm_state *state = &cache->state;
        if (!state->single_quoted && !state->double_quoted &&
                !state->backticked && !state->escaped) {
            if (ch == '#') break;
            if (ch == '%' && strchr(text + i + 1, '%') != NULL) {
                special = 1;
            } else if (ch == '(' || ch == '[' || ch == '{') {
                if (cache->depth == cache->capacity) {
                    if (cache->capacity > INT_MAX / 2) Rf_error("too many open brackets");
                    int capacity = cache->capacity ? cache->capacity * 2 : 16;
                    cache->brackets = R_Realloc(cache->brackets, capacity, signature_bracket);
                    cache->capacity = capacity;
                }
                cache->brackets[cache->depth++] = (signature_bracket) {row, current_col, (char) ch};
            } else if ((ch == ')' || ch == ']' || ch == '}') && cache->depth) {
                cache->depth--;
            }
        }
        fsm_feed(state, (char) ch);
    }
}

/* Cache the lexical state and open brackets through the preceding line. This
 * keeps edits on the cursor line cheap while correctly handling strings that
 * span lines, even before the current document can be parsed. */
SEXP signature_call_bracket_c(SEXP content, SEXP row_, SEXP col_,
    SEXP first_row_, SEXP object) {
    if (TYPEOF(content) != STRSXP || TYPEOF(object) != EXTPTRSXP ||
            R_ExternalPtrTag(object) != Rf_install("languageserver.signature_call_cache")) {
        Rf_error("invalid signature call scan input");
    }
    if (R_ExternalPtrAddr(object) == NULL) signature_call_cache_init(object);
    signature_call_cache *cache = R_ExternalPtrAddr(object);
    int row = Rf_asInteger(row_);
    int col = Rf_asInteger(col_);
    int first_row = Rf_asInteger(first_row_);
    if (row < 0 || row >= Rf_length(content) || col < 0 ||
            first_row < 0 || first_row > row) Rf_error("invalid signature call position");

    SEXP previous = R_ExternalPtrProtected(object);
    int matches = cache->valid && cache->row == row && cache->first_row == first_row;
    if (matches && previous != content) {
        for (int i = first_row; i < row; i++) {
            if (STRING_ELT(previous, i) != STRING_ELT(content, i)) {
                matches = 0;
                break;
            }
        }
    }
    if (!matches) {
        cache->valid = 0;
        cache->depth = 0;
        fsm_initialize(&cache->state);
        for (int i = first_row; i < row; i++) {
            if (STRING_ELT(content, i) == NA_STRING) continue;
            signature_call_scan_line(cache, Rf_translateCharUTF8(STRING_ELT(content, i)), i, INT_MAX);
            fsm_feed(&cache->state, '\n');
        }
        cache->row = row;
        cache->first_row = first_row;
    }
    R_SetExternalPtrProtected(object, content);
    int depth = cache->depth;
    fsm_state state = cache->state;
    signature_bracket *saved = (signature_bracket *) R_alloc((size_t) depth, sizeof(signature_bracket));
    if (depth) memcpy(saved, cache->brackets, (size_t) depth * sizeof(signature_bracket));
    cache->valid = 0;
    if (STRING_ELT(content, row) != NA_STRING) {
        signature_call_scan_line(cache, Rf_translateCharUTF8(STRING_ELT(content, row)), row, col);
    }
    signature_bracket found = cache->depth ? cache->brackets[cache->depth - 1] :
        (signature_bracket) {-1, -1, ' '};
    cache->state = state;
    cache->depth = depth;
    if (depth) memcpy(cache->brackets, saved, (size_t) depth * sizeof(signature_bracket));
    cache->valid = 1;

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP loc = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(loc)[0] = found.row;
    INTEGER(loc)[1] = found.col;
    SET_VECTOR_ELT(out, 0, loc);
    char bracket[] = {found.bracket, '\0'};
    SET_VECTOR_ELT(out, 1, Rf_mkString(bracket));
    UNPROTECT(2);
    return out;
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

static int special_end(const char *text, int at, int size) {
    if (text[at] != '%') return at;
    for (int i = at + 1; i < size && text[i] != '\n'; i++) {
        if (text[i] == '%') return i + 1;
    }
    return at;
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
        if (end == i) end = special_end(text, i, size);
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

static int name_matches(const char *text, int left, int right, const char *name,
    int partial) {
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
    return left == right && (partial || name_left == name_size);
}

typedef struct signature_literal_parse {
    SEXP text;
    ParseStatus status;
} signature_literal_parse;

static SEXP parse_signature_literal(void *data) {
    signature_literal_parse *literal = (signature_literal_parse *) data;
    return R_ParseVector(literal->text, -1, &literal->status, R_NilValue);
}

static SEXP invalid_signature_literal(SEXP error, void *data) {
    (void) error;
    signature_literal_parse *literal = (signature_literal_parse *) data;
    literal->status = PARSE_ERROR;
    return R_NilValue;
}

/* Decode only an isolated literal/name token. The parser supplies R's escape
 * and raw-string rules; expressions and calls are rejected and never evaluated.
 * Keep the legacy byte matcher separate for clients without nullable support. */
static const char *signature_literal_name(const char *text, int start, int end) {
    if (start >= end) return NULL;
    int quoted = text[start] == '`' || text[start] == '\'' || text[start] == '"';
    int raw = (text[start] == 'r' || text[start] == 'R') && start + 1 < end &&
        (text[start + 1] == '\'' || text[start + 1] == '"');
    if (!quoted && !raw) {
        char *name = (char *) R_alloc((size_t) (end - start) + 1, sizeof(char));
        memcpy(name, text + start, (size_t) (end - start));
        name[end - start] = '\0';
        return name;
    }
    SEXP source = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(text + start,
        end - start, CE_UTF8)));
    signature_literal_parse literal = {source, PARSE_NULL};
    SEXP parsed = PROTECT(R_tryCatchError(parse_signature_literal, &literal,
        invalid_signature_literal, &literal));
    const char *name = NULL;
    if (literal.status == PARSE_OK && TYPEOF(parsed) == EXPRSXP &&
            XLENGTH(parsed) == 1) {
        SEXP value = VECTOR_ELT(parsed, 0);
        SEXP string = R_NilValue;
        if (TYPEOF(value) == SYMSXP) string = PRINTNAME(value);
        else if (TYPEOF(value) == STRSXP && XLENGTH(value) == 1 &&
                STRING_ELT(value, 0) != NA_STRING) string = STRING_ELT(value, 0);
        if (string != R_NilValue) {
            const char *decoded = Rf_translateCharUTF8(string);
            size_t size = strlen(decoded);
            if (size) {
                char *copy = (char *) R_alloc(size + 1, sizeof(char));
                memcpy(copy, decoded, size + 1);
                name = copy;
            }
        }
    }
    UNPROTECT(2);
    return name;
}

static int normalized_parameter_match(const char *name, const char *const *parameters,
    int count, int partial, const int *used) {
    if (name == NULL) return -1;
    for (int i = 0; i < count; i++) {
        if (parameters[i] != NULL && strcmp(name, parameters[i]) == 0) return i;
    }
    if (!partial) return -1;
    size_t size = strlen(name);
    int matched = -1;
    for (int i = 0; i < count; i++) {
        if (parameters[i] == NULL) continue;
        if (strcmp(parameters[i], "...") == 0) break;
        if (used != NULL && used[i]) continue;
        if (strncmp(name, parameters[i], size) == 0) {
            if (matched >= 0) return -2;
            matched = i;
        }
    }
    return matched;
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

/* Find the closing parenthesis before allocating. Ordinary calls use bounded
 * look-ahead so incomplete source cannot copy the remainder of a document. */
static const char *pipe_call_text(SEXP content, int row, int col, size_t limit) {
    fsm_state state;
    fsm_initialize(&state);
    int depth = 0;
    int last_row = row;
    int last_col = col;
    size_t size = 0;
    int bounded = 0;
    for (int line = row; line < Rf_length(content); line++) {
        if (STRING_ELT(content, line) == NA_STRING) break;
        const char *text = Rf_translateCharUTF8(STRING_ELT(content, line));
        int start = line == row ? col : 0;
        size_t previous_size = size + (line > row);
        if (previous_size > limit) break;
        int i = start;
        int in_special = 0;
        for (; text[i]; i++) {
            if (previous_size + (size_t) (i - start) >= limit) {
                bounded = 1;
                break;
            }
            if ((i & 65535) == 0) R_CheckUserInterrupt();
            char ch = text[i];
            if (in_special) {
                if (ch == '%') in_special = 0;
                continue;
            }
            if (!state.single_quoted && !state.double_quoted && !state.backticked && !state.escaped) {
                if (ch == '#') {
                    while (text[i] && previous_size + (size_t) (i - start) < limit) i++;
                    if (text[i]) bounded = 1;
                    break;
                }
                if (ch == '%') {
                    int special = 0;
                    for (int next = i + 1; text[next] &&
                            previous_size + (size_t) (next - start) < limit; next++) {
                        if ((next & 65535) == 0) R_CheckUserInterrupt();
                        if (text[next] == '%') { special = 1; break; }
                    }
                    if (special) {
                        in_special = 1;
                        fsm_feed(&state, ch);
                        continue;
                    }
                }
                if (ch == ')' && depth == 0) break;
                if (ch == '(' || ch == '[' || ch == '{') depth++;
                else if (ch == ')' || ch == ']' || ch == '}') depth--;
            }
            fsm_feed(&state, ch);
        }
        size = previous_size + (size_t) (i - start);
        if (size > INT_MAX) Rf_error("call text is too large");
        last_row = line;
        last_col = i;
        if (bounded || text[i] == ')') break;
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

static int parameter_match(const char *text, int start, int end, SEXP parameters,
    int partial, const int *used, const char *const *normalized, const char **tag) {
    while (start < end && space(text[start])) start++;
    while (end > start && space(text[end - 1])) end--;
    if (start == end) return -1;
    /* Comments may separate an argument name and its equals sign. Keep quoted
     * names intact while ignoring whitespace and comments after the name. */
    int name_end = literal_end(text, start, end);
    if (name_end == start) {
        while (name_end < end && identifier(text[name_end])) name_end++;
    }
    if (name_end == start) return -1;
    for (int at = name_end; at < end; at++) {
        if (space(text[at])) continue;
        if (text[at] != '#') return -1;
        while (at < end && text[at] != '\n') at++;
    }
    end = name_end;
    if (normalized != NULL) {
        const char *name = signature_literal_name(text, start, end);
        if (tag != NULL) *tag = name;
        return normalized_parameter_match(name, normalized, Rf_length(parameters), partial, used);
    }
    for (int i = 0; i < Rf_length(parameters); i++) {
        if (STRING_ELT(parameters, i) != NA_STRING && name_matches(
                text, start, end, Rf_translateCharUTF8(STRING_ELT(parameters, i)), 0)) {
            return i;
        }
    }
    if (!partial) return -1;
    int matched = -1;
    for (int i = 0; i < Rf_length(parameters); i++) {
        if (STRING_ELT(parameters, i) == NA_STRING) continue;
        const char *name = Rf_translateCharUTF8(STRING_ELT(parameters, i));
        /* R only partially matches formals before the ellipsis. */
        if (strcmp(name, "...") == 0) break;
        if (used != NULL && used[i]) continue;
        if (name_matches(text, start, end, name, 1)) {
            if (matched >= 0) return -2;
            matched = i;
        }
    }
    return matched;
}

typedef struct signature_named_argument {
    int start, end, current;
    const char *name;
    struct signature_named_argument *next;
} signature_named_argument;

static int pipe_active_parameter(const char *text, int cursor, SEXP parameters,
    int pipe, int allow_null) {
    int size = (int) strlen(text);
    int nparameters = Rf_length(parameters);
    const char **normalized = NULL;
    if (allow_null) {
        normalized = (const char **) R_alloc((size_t) nparameters + 1, sizeof(char *));
        for (int i = 0; i < nparameters; i++) {
            const char *name = STRING_ELT(parameters, i) == NA_STRING ? NULL :
                Rf_translateCharUTF8(STRING_ELT(parameters, i));
            normalized[i] = name == NULL ? NULL : signature_literal_name(name, 0, (int) strlen(name));
        }
    }
    int *used = (int *) R_alloc((size_t) nparameters + 1, sizeof(int));
    memset(used, 0, ((size_t) nparameters + 1) * sizeof(int));
    int depth = 0, start = 0, name_end = -1;
    int value_tokens = 0, placeholder = 0, has_placeholder = 0;
    int positional = 0, active_name = -1, active_named = 0, active = 0;
    signature_named_argument *partial_names = NULL, *last_partial = NULL;
    int dots = -1;
    for (int i = 0; i < nparameters; i++) {
        const char *name = normalized == NULL ?
            (STRING_ELT(parameters, i) == NA_STRING ? NULL : CHAR(STRING_ELT(parameters, i))) :
            normalized[i];
        if (name != NULL && strcmp(name, "...") == 0) dots = i;
    }
    for (int i = 0; i <= size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        char ch = text[i];
        if (i == size || (ch == ',' && depth == 0)) {
            const char *tag = NULL;
            int matched = name_end < 0 ? -1 : parameter_match(
                text, start, name_end, parameters, 0, NULL, normalized, &tag);
            int current = start <= cursor && cursor <= i;
            if (matched >= 0) used[matched] = 1;
            if (allow_null && name_end >= 0 && matched < 0) {
                signature_named_argument *argument = (signature_named_argument *)
                    R_alloc(1, sizeof(signature_named_argument));
                argument->start = start;
                argument->end = name_end;
                argument->current = current && name_end < cursor;
                argument->name = tag;
                argument->next = NULL;
                if (last_partial != NULL) last_partial->next = argument;
                else partial_names = argument;
                last_partial = argument;
            }
            if (current && name_end >= 0 && name_end < cursor) {
                active_name = matched;
                active_named = 1;
            }
            if (i < cursor) {
                active++;
                if (name_end < 0) positional++;
            }
            if (pipe && placeholder && value_tokens == 1 && (pipe == 2 || name_end >= 0)) {
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
        if (end == i) end = special_end(text, i, size);
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
    if (allow_null && partial_names != NULL) {
        int *exact_used = (int *) R_alloc((size_t) nparameters + 1, sizeof(int));
        memcpy(exact_used, used, ((size_t) nparameters + 1) * sizeof(int));
        int *partial_counts = (int *) R_alloc((size_t) nparameters + 1, sizeof(int));
        memset(partial_counts, 0, ((size_t) nparameters + 1) * sizeof(int));
        /* Exact matches, including names after the cursor, reserve their
         * formals before R attempts any partial argument matching. */
        for (signature_named_argument *argument = partial_names;
                argument != NULL; argument = argument->next) {
            if (argument->name == NULL) continue;
            size_t name_size = strlen(argument->name);
            for (int i = 0; i < nparameters; i++) {
                if (normalized[i] == NULL) continue;
                if (strcmp(normalized[i], "...") == 0) break;
                if (!exact_used[i] && strncmp(argument->name, normalized[i], name_size) == 0) {
                    partial_counts[i]++;
                }
            }
        }
        /* Keep the exact-only snapshot fixed: consuming a partial match could
         * incorrectly disambiguate another argument. R rejects multiple
         * partial candidates for a formal, including ambiguous candidates. */
        for (signature_named_argument *argument = partial_names;
                argument != NULL; argument = argument->next) {
            int matched = normalized_parameter_match(argument->name, normalized,
                nparameters, 1, exact_used);
            if (matched >= 0) {
                used[matched] = 1;
                if (partial_counts[matched] > 1) matched = -2;
            }
            if (argument->current) active_name = matched;
        }
    }
    if (active_name >= 0) return active_name;
    if (allow_null && active_named) return active_name == -2 ? -1 : dots;
    positional += pipe && !has_placeholder;
    for (int i = 0; i < nparameters; i++) {
        const char *name = normalized == NULL ?
            (STRING_ELT(parameters, i) == NA_STRING ? NULL : CHAR(STRING_ELT(parameters, i))) :
            normalized[i];
        if (name != NULL && strcmp(name, "...") == 0) return i;
        if (!used[i] && positional-- == 0) return i;
    }
    return allow_null ? -1 : active + !has_placeholder;
}

SEXP active_parameter_c(SEXP call_text, SEXP parameters, SEXP content,
    SEXP start_row, SEXP start_col, SEXP allow_null) {
    const char *text = single_string(call_text, "call text");
    if (TYPEOF(parameters) != STRSXP) Rf_error("parameters must be character");
    size_t size_long = strlen(text);
    if (size_long > INT_MAX) Rf_error("call text is too large");
    int size = (int) size_long;
    int row = Rf_asInteger(start_row);
    int col = Rf_asInteger(start_col);
    int nullable = Rf_asLogical(allow_null) == TRUE;
    if (TYPEOF(content) == STRSXP && row >= 0 && row < Rf_length(content) &&
            col >= 0 && STRING_ELT(content, row) != NA_STRING) {
        const char *line = Rf_translateCharUTF8(STRING_ELT(content, row));
        int byte = 0;
        for (int point = 0; line[byte] && point < col; point++) {
            byte++;
            while (((unsigned char) line[byte] & 0xc0) == 0x80) byte++;
        }
        int pipe = line[byte] == '(' ? call_pipe(content, row, byte) : 0;
        if (line[byte] == '(' && (pipe || nullable)) {
            /* Keep the complete cursor prefix, then inspect at most 64 KiB
             * more for named arguments that can reserve positional formals. */
            size_t limit = pipe ? (size_t) -1 : (size_t) size + 65536;
            int active = pipe_active_parameter(
                pipe_call_text(content, row, byte + 1, limit), size, parameters, pipe, nullable);
            return active < 0 ? R_NilValue : Rf_ScalarInteger(active);
        }
    }
    int depth = 0;
    int count = 0;
    int left = -1;
    int right = -1;
    int name_end = -1;
    for (int i = 0; i < size; i++) {
        if ((i & 65535) == 0) R_CheckUserInterrupt();
        int end = literal_end(text, i, size);
        if (end == i) end = special_end(text, i, size);
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
                    Rf_translateCharUTF8(STRING_ELT(parameters, i)), 0)) {
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
