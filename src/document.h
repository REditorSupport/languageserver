#ifndef _DOCUMENT_H_
#define _DOCUMENT_H_ 1

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP content_backward_search(SEXP document, SEXP _row, SEXP _col, SEXP _char, SEXP _skip_el);

#endif
