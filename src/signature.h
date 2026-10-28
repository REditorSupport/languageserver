#ifndef LANGUAGESERVER_SIGNATURE_H
#define LANGUAGESERVER_SIGNATURE_H

#include <R.h>
#include <Rinternals.h>

SEXP signature_info_c(SEXP signature);
SEXP active_parameter_c(SEXP text, SEXP parameters, SEXP content,
    SEXP row, SEXP col, SEXP allow_null);
SEXP new_signature_call_cache_c(void);
SEXP signature_call_bracket_c(SEXP content, SEXP row, SEXP col,
    SEXP first_row, SEXP cache);

#endif
