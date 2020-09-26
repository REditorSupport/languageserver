prepare_rename_reply <- function(id, uri, workspace, document, point) {
  token <- document$detect_token(point)
  defn <- definition_reply(NULL, uri, workspace, document, point)

  logger$info("prepare_rename_reply: ", list(
    token = token,
    defn = defn$result
  ))

  if (length(defn$result)) {
    Response$new(
      id,
      result = range(
        start = document$to_lsp_position(
          row = token$range$start$row,
          col = token$range$start$col),
        end = document$to_lsp_position(
          row = token$range$end$row,
          col = token$range$end$col)
      )
    )
  } else {
    ResponseErrorMessage$new(
      id,
      errortype = "RequestCancelled",
      message = "Cannot rename the symbol"
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
