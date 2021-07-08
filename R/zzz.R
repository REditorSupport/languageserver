.onLoad <- function(...) {
  extra_unscoped_functions <- getOption("languageserver.extra_unscoped_functions")
  parse_config$unscoped_functions[names(extra_unscoped_functions)] <- extra_unscoped_functions

  extra_library_functions <- getOption("languageserver.extra_library_functions")
  parse_config$library_functions[names(extra_library_functions)] <- extra_library_functions
}
