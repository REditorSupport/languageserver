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

typedef struct {
    int prefix_length;
    int closing_brackets;
    int skip_empty_line;
    int row;
    int col;
    char bracket;
} bracket_scan_cache;

static void free_bracket_scan_cache(SEXP cache) {
    bracket_scan_cache *data = R_ExternalPtrAddr(cache);
    if (data != NULL) {
        R_Free(data);
        R_ClearExternalPtr(cache);
    }
}

static void initialize_bracket_scan_cache(SEXP cache) {
    bracket_scan_cache *data = R_Calloc(1, bracket_scan_cache);
    data->prefix_length = -1;
    R_SetExternalPtrAddr(cache, data);
    R_RegisterCFinalizerEx(cache, free_bracket_scan_cache, TRUE);
}

SEXP new_bracket_scan_cache_c(void) {
    SEXP cache = PROTECT(R_MakeExternalPtr(NULL,
        Rf_install("languageserver.bracket_scan_cache"), R_NilValue));
    initialize_bracket_scan_cache(cache);
    UNPROTECT(1);
    return cache;
}

/* R strings are immutable. Comparing their identities lets an edit on the
 * cursor line reuse a preceding-line scan without trusting stale parse data.
 * Keep the original vector protected so cached string identities stay live. */
static int bracket_scan_cache_matches(SEXP object, SEXP content,
    int prefix_length, int closing_brackets, int skip_empty_line) {
    bracket_scan_cache *cache = R_ExternalPtrAddr(object);
    if (cache->prefix_length != prefix_length ||
            cache->closing_brackets != closing_brackets ||
            cache->skip_empty_line != skip_empty_line) return 0;
    SEXP previous = R_ExternalPtrProtected(object);
    if (previous == content) return 1;
    for (int i = 0; i < prefix_length; ++i) {
        if (STRING_ELT(previous, i) != STRING_ELT(content, i)) return 0;
    }
    return 1;
}

static SEXP find_unbalanced_bracket_impl(SEXP content, SEXP _row, SEXP _col,
    SEXP _skip_el, SEXP cache_object) {
    int ncontent = Rf_length(content);
    int row = Rf_asInteger(_row);
    int col = Rf_asInteger(_col);
    int skip = Rf_asInteger(_skip_el);

    int i, j;
    int k = 0;
    int n;
    const char* c;
    unsigned char cj;
    fsm_state state;
    stack pos;
    stack codept_pos;
    int nbracket = 0;
    int ncurrentunbalanced = 0;
    int nunbalanced = 0;
    char brac[2] = " \x00";
    int cache_prefix = 0;
    int prefix_closing_brackets = 0;
    bracket_scan_cache *cache = cache_object == R_NilValue ? NULL :
        R_ExternalPtrAddr(cache_object);

    for (i = row; i >= 0 && i < ncontent; i--) {
        if (cache != NULL && i == row - 1) {
            if (bracket_scan_cache_matches(cache_object, content,
                    row, nunbalanced, skip)) {
                i = cache->row;
                k = cache->col;
                brac[0] = cache->bracket;
                /* Future requests for this exact content need no line comparisons. */
                R_SetExternalPtrProtected(cache_object, content);
                break;
            }
            cache_prefix = 1;
            prefix_closing_brackets = nunbalanced;
        }
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
        ncurrentunbalanced = 0;
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
                        ncurrentunbalanced += 1;
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
        nunbalanced += ncurrentunbalanced;
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
    if (cache_prefix) {
        cache->prefix_length = row;
        cache->closing_brackets = prefix_closing_brackets;
        cache->skip_empty_line = skip;
        cache->row = i;
        cache->col = k;
        cache->bracket = brac[0];
        R_SetExternalPtrProtected(cache_object, content);
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

SEXP find_unbalanced_bracket(SEXP content, SEXP row, SEXP col,
    SEXP skip_empty_line) {
    return find_unbalanced_bracket_impl(content, row, col, skip_empty_line, R_NilValue);
}

SEXP find_unbalanced_bracket_cached_c(SEXP content, SEXP row, SEXP col,
    SEXP skip_empty_line, SEXP object) {
    if (TYPEOF(object) != EXTPTRSXP || R_ExternalPtrTag(object) !=
            Rf_install("languageserver.bracket_scan_cache")) {
        Rf_error("invalid bracket scan cache");
    }
    /* External pointers lose their address when an R document is serialized. */
    if (R_ExternalPtrAddr(object) == NULL) initialize_bracket_scan_cache(object);
    return find_unbalanced_bracket_impl(content, row, col, skip_empty_line, object);
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
