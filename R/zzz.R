.onLoad <- function(...) {
  user_parser_hooks <- getOption("languageserver.parser_hooks")
  parser_hooks[names(user_parser_hooks)] <<- user_parser_hooks
  invisible()
}
