#ifndef MEMBER_H__
#define MEMBER_H__

#include <R.h>
#include <Rinternals.h>

SEXP member_syntax_names_c(SEXP expr);
SEXP member_hint_paths_c(SEXP expr, SEXP rows, SEXP cols, SEXP offset);
SEXP member_function_key_c(SEXP fn);

#endif
