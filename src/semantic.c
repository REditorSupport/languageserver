#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

/*
 * Encode semantic tokens in LSP format using relative position deltas.
 *
 * Args:
 *   lines: integer vector of 0-based line numbers (sorted)
 *   cols: integer vector of 0-based column numbers (sorted by line, then col)
 *   lengths: integer vector of token lengths (in code points)
 *   types: integer vector of token types
 *   modifiers: integer vector of token modifiers (bitfield)
 *
 * Returns:
 *   Integer vector representing encoded semantic tokens:
 *   [deltaLine, deltaCol, length, tokenType, tokenModifiers, ...]
 *
 * The LSP semantic tokens format encodes positions as deltas (differences) to
 * reduce data size. On a new line, the column resets to absolute position.
 */
SEXP encode_semantic_tokens_c(SEXP lines, SEXP cols, SEXP lengths,
                              SEXP types, SEXP modifiers) {
    R_xlen_t n = XLENGTH(lines);
    
    if (n == 0) {
        return Rf_allocVector(INTSXP, 0);
    }

    // Ensure inputs are integers
    if (!Rf_isInteger(lines) || !Rf_isInteger(cols) ||
        !Rf_isInteger(lengths) || !Rf_isInteger(types) ||
        !Rf_isInteger(modifiers)) {
        Rf_error("All inputs must be integer vectors");
    }
    if (XLENGTH(cols) != n || XLENGTH(lengths) != n ||
            XLENGTH(types) != n || XLENGTH(modifiers) != n ||
            n > R_XLEN_T_MAX / 5) {
        Rf_error("Invalid semantic token vector lengths");
    }

    // Allocate output: exactly 5*n elements
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 5 * n));
    int* out_ptr = INTEGER(out);
    int* lines_ptr = INTEGER(lines);
    int* cols_ptr = INTEGER(cols);
    int* lengths_ptr = INTEGER(lengths);
    int* types_ptr = INTEGER(types);
    int* mods_ptr = INTEGER(modifiers);

    R_xlen_t out_idx = 0;
    int prev_line = 0;
    int prev_col = 0;

    for (R_xlen_t i = 0; i < n; i++) {
        int line = lines_ptr[i];
        int col = cols_ptr[i];
        int length = lengths_ptr[i];
        int type = types_ptr[i];
        int mods = mods_ptr[i];

        // Compute line delta
        int line_delta = line - prev_line;

        // Compute column delta
        // If same line, delta from previous column; else reset to absolute column
        int col_delta = (line_delta == 0) ? (col - prev_col) : col;

        // Store encoded values
        out_ptr[out_idx++] = line_delta;
        out_ptr[out_idx++] = col_delta;
        out_ptr[out_idx++] = length;
        out_ptr[out_idx++] = type;
        out_ptr[out_idx++] = mods;

        prev_line = line;
        prev_col = col;
    }

    UNPROTECT(1);
    return out;
}

/* Lower/upper bounds avoid allocating a document-sized logical vector for
 * every viewport request. The parser stores these indexes in source order. */
static R_xlen_t line_bound(const int *lines, R_xlen_t n, int line, int upper) {
    R_xlen_t first = 0;
    while (first < n) {
        R_xlen_t mid = first + (n - first) / 2;
        if (lines[mid] < line || (upper && lines[mid] == line)) {
            first = mid + 1;
        } else {
            n = mid;
        }
    }
    return first;
}

SEXP range_line_bounds_c(SEXP lines, SEXP start, SEXP end) {
    if (TYPEOF(lines) != INTSXP || XLENGTH(lines) >= INT_MAX ||
            XLENGTH(start) != 1 || XLENGTH(end) != 1) {
        Rf_error("Invalid range line index");
    }
    int start_line = Rf_asInteger(start), end_line = Rf_asInteger(end);
    if (start_line == NA_INTEGER || end_line == NA_INTEGER) {
        Rf_error("Range lines must not be missing");
    }
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(result)[0] = (int) line_bound(INTEGER(lines), XLENGTH(lines), start_line, 0) + 1;
    INTEGER(result)[1] = (int) line_bound(INTEGER(lines), XLENGTH(lines), end_line, 1);
    UNPROTECT(1);
    return result;
}

SEXP semantic_token_range_c(SEXP data, SEXP start, SEXP end) {
    if (TYPEOF(data) != VECSXP || XLENGTH(data) != 6 ||
            TYPEOF(start) != INTSXP || XLENGTH(start) != 2 ||
            TYPEOF(end) != INTSXP || XLENGTH(end) != 2) {
        Rf_error("Invalid semantic token range arguments");
    }
    R_xlen_t n = XLENGTH(VECTOR_ELT(data, 0));
    const int *columns[5];
    for (int i = 0; i < 5; i++) {
        SEXP value = VECTOR_ELT(data, i);
        if (TYPEOF(value) != INTSXP || XLENGTH(value) != n) {
            Rf_error("Invalid semantic token columns");
        }
        columns[i] = INTEGER(value);
    }
    int start_line = INTEGER(start)[0], start_col = INTEGER(start)[1];
    int end_line = INTEGER(end)[0], end_col = INTEGER(end)[1];
    if (start_line == NA_INTEGER || start_col == NA_INTEGER ||
            end_line == NA_INTEGER || end_col == NA_INTEGER) {
        Rf_error("Semantic token range positions must not be missing");
    }
    R_xlen_t first = line_bound(columns[0], n, start_line, 0);
    R_xlen_t last = line_bound(columns[0], n, end_line, 1);
    /* Same-line semantic tokens do not overlap. Retain the complete token
     * when the requested start falls within it, as required by the existing
     * range provider. */
    while (first < last && columns[0][first] == start_line &&
            (double) columns[1][first] + columns[2][first] <= start_col) first++;
    while (last > first && columns[0][last - 1] == end_line &&
            columns[1][last - 1] >= end_col) last--;
    if (end_line < start_line || (end_line == start_line && end_col <= start_col)) {
        last = first;
    }
    R_xlen_t size = last > first ? last - first : 0;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 6));
    Rf_setAttrib(result, R_NamesSymbol, Rf_getAttrib(data, R_NamesSymbol));
    for (int i = 0; i < 5; i++) {
        SEXP value = PROTECT(Rf_allocVector(INTSXP, size));
        if (size) memcpy(INTEGER(value), columns[i] + first, (size_t) size * sizeof(int));
        SET_VECTOR_ELT(result, i, value);
        UNPROTECT(1);
    }
    SET_VECTOR_ELT(result, 5, encode_semantic_tokens_c(
        VECTOR_ELT(result, 0), VECTOR_ELT(result, 1), VECTOR_ELT(result, 2),
        VECTOR_ELT(result, 3), VECTOR_ELT(result, 4)));
    UNPROTECT(1);
    return result;
}

SEXP semantic_token_delta_c(SEXP previous, SEXP current) {
    if (TYPEOF(previous) != INTSXP || TYPEOF(current) != INTSXP ||
            XLENGTH(previous) % 5 || XLENGTH(current) % 5 ||
            XLENGTH(previous) > INT_MAX || XLENGTH(current) > INT_MAX) {
        Rf_error("Semantic token deltas require complete integer tokens");
    }
    R_xlen_t previous_n = XLENGTH(previous), current_n = XLENGTH(current);
    R_xlen_t shared = previous_n < current_n ? previous_n : current_n;
    R_xlen_t prefix = 0, suffix = 0;
    const int *before = INTEGER(previous), *after = INTEGER(current);
    while (prefix < shared && !memcmp(before + prefix, after + prefix, 5 * sizeof(int))) {
        prefix += 5;
    }
    if (prefix == previous_n && prefix == current_n) return Rf_allocVector(VECSXP, 0);
    while (suffix < shared - prefix &&
            !memcmp(before + previous_n - suffix - 5,
                after + current_n - suffix - 5, 5 * sizeof(int))) suffix += 5;

    R_xlen_t replacement_n = current_n - prefix - suffix;
    int fields = replacement_n ? 3 : 2;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 1));
    SEXP edit = PROTECT(Rf_allocVector(VECSXP, fields));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, fields));
    SET_STRING_ELT(names, 0, Rf_mkChar("start"));
    SET_STRING_ELT(names, 1, Rf_mkChar("deleteCount"));
    SET_VECTOR_ELT(edit, 0, Rf_ScalarInteger((int) prefix));
    SET_VECTOR_ELT(edit, 1, Rf_ScalarInteger((int) (previous_n - prefix - suffix)));
    if (replacement_n) {
        SET_STRING_ELT(names, 2, Rf_mkChar("data"));
        SEXP replacement = PROTECT(Rf_allocVector(INTSXP, replacement_n));
        memcpy(INTEGER(replacement), after + prefix, (size_t) replacement_n * sizeof(int));
        SET_VECTOR_ELT(edit, 2, replacement);
        UNPROTECT(1);
    }
    Rf_setAttrib(edit, R_NamesSymbol, names);
    SET_VECTOR_ELT(result, 0, edit);
    UNPROTECT(3);
    return result;
}
