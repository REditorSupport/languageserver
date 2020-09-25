prepare_rename_reply <- function(id, uri, workspace, document, point, newName) {
  defn <- definition_reply(NULL, uri, workspace, document, point)

  if (length(defn$result)) {
    xdoc <- workspace$get_parse_data(uri)$xml_doc
    row <- point$row + 1
    col <- point$col + 1
    token <- xdoc_find_token(xdoc, row, col)
    Response$new(
      id,
      result = range(
        start = document$to_lsp_position(
          row = as.integer(xml_attr(token, "line1")) - 1,
          col = as.integer(xml_attr(token, "col1")) - 1),
        end = document$to_lsp_position(
          row = as.integer(xml_attr(token, "line2")) - 1,
          col = as.integer(xml_attr(token, "col2")))
      )
    )
  } else {
    Response$new(
      id,
      result = NULL,
      error = list(message = "Cannot rename the symbol")
    )
  }
}

#' @keywords internal
rename_reply <- function(id, uri, workspace, document, point, newName) {
  refs <- references_reply(NULL, uri, workspace, document, point)
  result <- list()

  for (ref in refs$result) {
    result[[ref$uri]] <- c(result[[ref$uri]], list(text_edit(
      range = ref$range,
      new_text = newName
    )))
  }

  logger$info("rename_reply: ", result)

  if (length(result)) {
    Response$new(
      id,
      result = list(
        changes = result
      )
    )
  } else {
    Response$new(
      id
    )
  }
}
