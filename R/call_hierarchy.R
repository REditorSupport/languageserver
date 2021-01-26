#' @keywords internal
prepare_call_hierarchy_reply <- function(id, uri, workspace, document, point) {

  token <- document$detect_token(point)
  defn <- definition_reply(NULL, uri, workspace, document, point)
  token_quote <- xml_single_quote(token$token)

  logger$info("prepare_call_hierarchy_reply: ", list(
    uri = uri,
    token = token,
    defn = defn$result
  ))

  result <- list()

  if (length(defn$result)) {
    result <- list(
      list(
        name = token$token,
        kind =  SymbolKind$Function,
        uri = defn$uri,
        range = defn$range,
        selectionRange = defn$range
      )
    )
  }

  logger$info("prepare_call_hierarchy_reply: ", result)

  Response$new(
    id,
    result = result
  )
}
