#ifndef SEMANTIC_H__
#define SEMANTIC_H__

#include <R.h>
#include <Rinternals.h>

/* Encode semantic tokens in LSP format using relative position deltas */
SEXP encode_semantic_tokens_c(SEXP lines, SEXP cols, SEXP lengths,
                              SEXP types, SEXP modifiers);

#endif /* end of include guard: SEMANTIC_H__ */
