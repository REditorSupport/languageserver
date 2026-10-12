#ifndef MESSAGE_BUFFER_H_
#define MESSAGE_BUFFER_H_
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
SEXP new_message_buffer_c(void);
SEXP message_buffer_append_c(SEXP pointer, SEXP chunk);
SEXP message_buffer_fetch_c(SEXP pointer);
SEXP message_buffer_pending_c(SEXP pointer);
#endif
