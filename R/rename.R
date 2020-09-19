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
