#' The response to a textDocument/selectionRange Request
#'
#' @keywords internal
selection_range_reply <- function(id, uri, workspace, document, positions) {
  result <- NULL

  parse_data <- workspace$get_parse_data(uri)
  if (is.null(parse_data) ||
    (!is.null(parse_data$version) && parse_data$version != document$version)) {
    return(Response$new(id))
  }

  xdoc <- parse_data$xml_doc
  if (!is.null(xdoc)) {
    result <- lapply(positions, function(point) {
      row <- point$line + 1
      col <- point$character + 1
      token <- xdoc_find_token(xdoc, row, col)
      nodes <- xml_find_all(token, "self::*[@line1] | ancestor::*[@line1]")
      ranges <- lapply(nodes, function(token) {
        range(
          start = document$to_lsp_position(
            row = as.integer(xml_attr(token, "line1")) - 1,
            col = as.integer(xml_attr(token, "col1")) - 1),
          end = document$to_lsp_position(
            row = as.integer(xml_attr(token, "line2")) - 1,
            col = as.integer(xml_attr(token, "col2")))
        )
      })
      selection_range <- NULL
      for (item in ranges) {
        selection_range <- list(
          range = item,
          parent = selection_range
        )
      }
      selection_range
    })
  }

  Response$new(id, result = result)
}
