#ifndef _PAIRLIST_H_
#define _PAIRLIST_H_ 1

#include <R.h>
#include <Rdefines.h>

SEXP pairlist_car(SEXP x);

SEXP pairlist_cdr(SEXP x);

SEXP pairlist_last(SEXP x);

SEXP pairlist_append(SEXP x, SEXP value);

SEXP pairlist_update(SEXP x, SEXP value);

#endif
