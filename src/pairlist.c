#include "pairlist.h"

// return the current item of a pairlist
SEXP pairlist_car(SEXP x) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  return CAR(x);
}


// return the next item of a pairlist
SEXP pairlist_cdr(SEXP x) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  return CDR(x);
}


// return the last item of a pairlist
SEXP pairlist_last(SEXP x) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");

  SEXP next = x;
  while (next != R_NilValue) {
    x = next;
    next = CDR(next);
  }
  return x;
}


// append an item at the end of the pairlist
SEXP pairlist_append(SEXP x, SEXP value) {
  if (x == R_NilValue)
    return Rf_cons(value, R_NilValue);
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");

  // traverse to end
  x = pairlist_last(x);

  SETCDR(x, Rf_cons(value, R_NilValue));
  return CDR(x);
}

// update the current item
SEXP pairlist_setcar(SEXP x, SEXP value) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  SETCAR(x, value);
  return x;
}

// link current item to a different item
SEXP pairlist_setcdr(SEXP x, SEXP y) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  if (!Rf_isList(y))
    Rf_error("y must be a pairlist");
  SETCDR(x, y);
  return x;
}
