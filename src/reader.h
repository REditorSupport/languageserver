#ifndef _READER_H_
#define _READER_H_ 1

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP stdin_read_char(SEXP _n);

SEXP stdin_read_line(void);

#endif
