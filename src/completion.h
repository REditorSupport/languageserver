#ifndef COMPLETION_H__
#define COMPLETION_H__

#include <R.h>
#include <Rinternals.h>

SEXP member_binding_c(SEXP env, SEXP name, SEXP materialize);

/* Extract row indexes needed to build document completion indexes. */
SEXP completion_parse_index_c(
    SEXP id,
    SEXP parent,
    SEXP token,
    SEXP line1,
    SEXP col1,
    SEXP line2,
    SEXP col2
);

/* Select the best completion candidate indexes with bounded memory. */
SEXP completion_select_c(
    SEXP labels,
    SEXP sort_text,
    SEXP token,
    SEXP limit
);

#endif /* end of include guard: COMPLETION_H__ */
