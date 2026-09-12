#ifndef NAVIGATION_H__
#define NAVIGATION_H__

#include <R.h>
#include <Rinternals.h>

SEXP navigation_find_token_c(
    SEXP line1, SEXP col1, SEXP line2, SEXP col2,
    SEXP is_string, SEXP point
);

SEXP reference_resolve_local_c(
    SEXP occurrence_group, SEXP occurrence_line, SEXP occurrence_col,
    SEXP definition_group, SEXP line1, SEXP col1, SEXP line2, SEXP col2
);

#endif
