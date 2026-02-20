#ifndef MATCH_H__
#define MATCH_H__

#include <R.h>
#include <Rinternals.h>

/* Case-insensitive literal substring match (ASCII case-folding). */
SEXP match_with_c(SEXP x, SEXP token);

/* Case-insensitive subsequence match (ASCII case-folding). */
SEXP fuzzy_find_c(SEXP x, SEXP pattern);

#endif /* end of include guard: MATCH_H__ */
