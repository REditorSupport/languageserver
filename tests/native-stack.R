# Run in a fresh R session so languageserver's native library loads first.
loadNamespace("languageserver")
s <- collections::stack()
s$push(1L)
stopifnot(identical(s$pop(), 1L))
stopifnot(identical(languageserver:::missing_closing_delimiters("f({["), "]})"))
