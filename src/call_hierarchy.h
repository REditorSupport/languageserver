#ifndef LANGUAGESERVER_CALL_HIERARCHY_H
#define LANGUAGESERVER_CALL_HIERARCHY_H

#include <R.h>
#include <Rinternals.h>

SEXP call_hierarchy_containers_c(SEXP occurrences, SEXP definitions);

#endif
