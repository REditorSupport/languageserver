#' @keywords internal
prepare_call_hierarchy_reply <- function(id, uri, workspace, document, point) {

  token <- document$detect_token(point)
  token_quote <- xml_single_quote(token$token)

  result <- NULL

  logger$info("prepare_call_hierarchy_reply: ", list(
    uri = uri,
    token = token
  ))

  logger$info("prepare_call_hierarchy_reply: ", result)

  Response$new(
    id,
    result = result
  )
}
