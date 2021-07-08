.onLoad <- function(...) {
  parse_config$unscoped_functions <- union(
    parse_config$unscoped_functions,
    getOption("languageserver.extra_unscoped_functions")
  )

  parse_config$library_functions <- union(
    parse_config$library_functions,
    getOption("languageserver.extra_library_functions")
  )
}
