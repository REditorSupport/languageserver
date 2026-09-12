#ifndef LANGUAGESERVER_SIGNATURE_H
#define LANGUAGESERVER_SIGNATURE_H

#include <R.h>
#include <Rinternals.h>

SEXP signature_info_c(SEXP signature);
SEXP active_parameter_c(SEXP text, SEXP parameters);

#endif
