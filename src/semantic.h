#ifndef SEMANTIC_H__
#define SEMANTIC_H__

#include <R.h>
#include <Rinternals.h>

/* Encode semantic tokens in LSP format using relative position deltas */
SEXP encode_semantic_tokens_c(SEXP lines, SEXP cols, SEXP lengths,
                              SEXP types, SEXP modifiers);
SEXP semantic_token_delta_c(SEXP previous, SEXP current);
SEXP semantic_token_range_c(SEXP data, SEXP start, SEXP end);
SEXP range_line_bounds_c(SEXP lines, SEXP start, SEXP end);
SEXP function_assignment_ids_c(SEXP data);
SEXP range_provider_index_c(SEXP data);

#endif /* end of include guard: SEMANTIC_H__ */
