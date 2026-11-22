#ifndef LANGUAGESERVER_INDEX_H
#define LANGUAGESERVER_INDEX_H

#include <Rinternals.h>

SEXP source_calls_c(SEXP expressions);
SEXP member_assignment_calls_c(SEXP expressions);
SEXP member_accessor_registrations_c(SEXP expressions);
SEXP top_level_statements_c(SEXP expressions, SEXP gaps);

#endif
