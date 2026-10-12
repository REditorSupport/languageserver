#ifndef LANGUAGESERVER_STDIN_H_
#define LANGUAGESERVER_STDIN_H_
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
SEXP stdin_read_raw(SEXP count);
SEXP stdin_wait(SEXP timeout);
#endif
