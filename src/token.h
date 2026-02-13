#ifndef TOKEN_H__
#define TOKEN_H__

#include <R.h>
#include <Rinternals.h>

/* Scan a line for tokens around a column index (ASCII fast-path). */
SEXP scan_token_c(SEXP line, SEXP col, SEXP forward);

#endif /* end of include guard: TOKEN_H__ */
