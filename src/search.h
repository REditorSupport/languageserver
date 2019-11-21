#ifndef SEARCH_H__
#define SEARCH_H__

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP backward_search(SEXP document, SEXP _row, SEXP _col, SEXP _char, SEXP _skip_el);

SEXP enclosed_by_quotes(SEXP s, SEXP _col);

#endif /* end of include guard: SEARCH_H__ */
