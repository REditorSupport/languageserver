#ifndef LANGUAGESERVER_JSON_H
#define LANGUAGESERVER_JSON_H

#include <R.h>
#include <Rinternals.h>

/* Return NULL when the value needs jsonlite's general-purpose encoder. */
SEXP response_json_c(SEXP value, SEXP max_bytes);

#endif
