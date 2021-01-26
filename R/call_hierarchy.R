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
        uri = defn$result$uri,
        range = defn$result$range,
        selectionRange = defn$result$range,
        data = list(
          definition = defn$result
        )
      )
    )
  }

  logger$info("prepare_call_hierarchy_reply: ", result)

  Response$new(
    id,
    result = result
  )
}

call_hierarchy_incoming_calls_reply <- function(id, workspace, item) {
  logger$info("call_hierarchy_incoming_calls_reply: ", item)

  token_quote <- xml_single_quote(item$name)
  result <- list()

  for (doc_uri in workspace$documents$keys()) {
    doc <- workspace$documents$get(doc_uri)
    xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
    if (is.null(xdoc)) next

    symbols <- xml_find_all(xdoc,
      glue("//SYMBOL_FUNCTION_CALL[text() = '{token_quote}']", token_quote)
    )

    line1 <- as.integer(xml_attr(symbols, "line1"))
    col1 <- as.integer(xml_attr(symbols, "col1"))
    line2 <- as.integer(xml_attr(symbols, "line2"))
    col2 <- as.integer(xml_attr(symbols, "col2"))

    for (i in seq_len(length(symbols))) {
      symbol_point <- list(row = line1[[i]] - 1, col = col1[[i]])
      symbol_defn <- definition_reply(NULL, doc_uri, workspace, doc, symbol_point)
      if (identical(symbol_defn$result, item$data$definition)) {
        result <- c(result, list(list(
          uri = doc_uri,
          range = range(
            start = doc$to_lsp_position(
              row = line1[[i]] - 1,
              col = col1[[i]] - 1
            ),
            end = doc$to_lsp_position(
              row = line2[[i]] - 1,
              col = col2[[i]]
            )
          )
        )))
      }
    }
  }

  Response$new(id)
}

call_hierarchy_outgoing_calls_reply <- function(id, workspace, item) {
  logger$info("call_hierarchy_outgoing_calls_reply: ", item)
  Response$new(id)
}
