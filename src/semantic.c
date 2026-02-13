#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <ctype.h>

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
    int n = Rf_length(lines);
    
    if (n == 0) {
        return Rf_allocVector(INTSXP, 0);
    }

    // Ensure inputs are integers
    if (!Rf_isInteger(lines) || !Rf_isInteger(cols) ||
        !Rf_isInteger(lengths) || !Rf_isInteger(types) ||
        !Rf_isInteger(modifiers)) {
        Rf_error("All inputs must be integer vectors");
    }

    // Allocate output: exactly 5*n elements
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 5 * n));
    int* out_ptr = INTEGER(out);
    int* lines_ptr = INTEGER(lines);
    int* cols_ptr = INTEGER(cols);
    int* lengths_ptr = INTEGER(lengths);
    int* types_ptr = INTEGER(types);
    int* mods_ptr = INTEGER(modifiers);

    int out_idx = 0;
    int prev_line = 0;
    int prev_col = 0;

    for (int i = 0; i < n; i++) {
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
