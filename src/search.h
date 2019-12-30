#ifndef SEARCH_H__
#define SEARCH_H__

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP find_unbalanced_bracket(SEXP document, SEXP _row, SEXP _col, SEXP _skip_el);

SEXP enclosed_by_quotes(SEXP s, SEXP _col);

SEXP detect_comments(SEXP content, SEXP _row);

SEXP detect_indention(SEXP content, SEXP _start_row, SEXP _end_row);

SEXP remove_indention(SEXP content, SEXP _start_row, SEXP _end_row, SEXP _indention);

#endif /* end of include guard: SEARCH_H__ */
