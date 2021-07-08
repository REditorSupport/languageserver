.onLoad <- function(...) {
  unscoped_functions <- getOption("languageserver.unscoped_functions")
  parse_config$unscoped_functions[names(unscoped_functions)] <- unscoped_functions

  library_functions <- getOption("languageserver.library_functions")
  parse_config$library_functions[names(library_functions)] <- library_functions
}
