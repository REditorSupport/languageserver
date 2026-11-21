#ifndef LANGUAGESERVER_INDEX_H
#define LANGUAGESERVER_INDEX_H

#include <Rinternals.h>

SEXP source_calls_c(SEXP expressions);
SEXP member_assignment_calls_c(SEXP expressions);

#endif
