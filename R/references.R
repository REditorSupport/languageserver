references_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and text() = '{token_quote}']"

#' @keywords internal
references_reply <- function(id, uri, workspace, document, point) {

  token <- document$detect_token(point)
  defn <- definition_reply(NULL, uri, workspace, document, point)
  token_quote <- xml_single_quote(token$token)

  logger$info("references_reply: ", list(
    uri = uri,
    token = token,
    defn = defn$result
  ))

  result <- list()

  if (length(defn$result)) {
    for (doc_uri in workspace$documents$keys()) {
      doc <- workspace$documents$get(doc_uri)
      xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
      if (!is.null(xdoc)) {
        symbols <- xml_find_all(xdoc, glue(references_xpath, token_quote = token_quote))
        line1 <- as.integer(xml_attr(symbols, "line1"))
        col1 <- as.integer(xml_attr(symbols, "col1"))
        line2 <- as.integer(xml_attr(symbols, "line2"))
        col2 <- as.integer(xml_attr(symbols, "col2"))
        for (i in seq_len(length(symbols))) {
          symbol_point <- list(row = line1[[i]] - 1, col = col1[[i]])
          symbol_defn <- definition_reply(NULL, doc_uri, workspace, doc, symbol_point)
          if (identical(symbol_defn$result, defn$result)) {
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
    }
  }

  logger$info("references_reply: ", result)

  Response$new(
    id,
    result = result
  )
}
