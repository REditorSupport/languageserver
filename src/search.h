#ifndef SEARCH_H__
#define SEARCH_H__

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP find_unbalanced_bracket(SEXP document, SEXP _row, SEXP _col, SEXP _skip_el);
SEXP new_bracket_scan_cache_c(void);
SEXP find_unbalanced_bracket_cached_c(SEXP document, SEXP row, SEXP col,
    SEXP skip_empty_line, SEXP cache);

SEXP enclosed_by_quotes(SEXP s, SEXP _col);

SEXP detect_comments(SEXP content, SEXP _row);

#endif /* end of include guard: SEARCH_H__ */
