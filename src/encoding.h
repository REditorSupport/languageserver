#ifndef ENCODING_H__
#define ENCODING_H__

#include <R.h>
#include <Rinternals.h>

/* Convert code points to UTF-16 code units */
SEXP code_point_to_unit_c(SEXP line, SEXP points);

/* Convert UTF-16 code units to code points */
SEXP code_point_from_unit_c(SEXP line, SEXP units);

#endif /* end of include guard: ENCODING_H__ */
