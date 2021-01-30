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

  in_calls <- collections::dict()

  for (doc_uri in workspace$documents$keys()) {
    doc <- workspace$documents$get(doc_uri)
    xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
    if (is.null(xdoc)) next

    defns <- workspace$get_definitions_for_uri(doc_uri)

    for (defn in defns) {
      if (doc_uri == item$uri && equal_range(defn$range, item$data$definition$range)) {
        next
      }

      start_point <- doc$from_lsp_position(defn$range$start)
      end_point <- doc$from_lsp_position(defn$range$end)
      line1 <- start_point$row + 1
      col1 <- start_point$col + 1
      line2 <- end_point$row + 1
      col2 <- end_point$col

      symbols <- xml_find_all(xdoc,
        glue("//SYMBOL_FUNCTION_CALL[
          ((@line1 = {line1} and @col1 >= {col1}) or @line1 > {line1}) and
          ((@line2 = {line2} and @col2 <= {col2}) or @line2 < {line2}) and
          text() = '{token_quote}']",
          line1 = line1, col1 = col1, line2 = line2, col2 = col2,
          token_quote = token_quote
        )
      )

      if (length(symbols) == 0) {
        next
      }

      defn$uri <- doc_uri

      symbol_names <- xml_name(symbols)
      symbol_text <- xml_text(symbols)
      symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
      symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
      symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
      symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

      for (i in seq_along(symbols)) {
        symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
        symbol_defn <- definition_reply(NULL, doc_uri, workspace, doc, symbol_point)$result

        if (!equal_definition(symbol_defn, item$data$definition)) {
          next
        }

        if (!in_calls$has(defn)) {
          in_calls$set(defn, list(
            from = list(
              name = defn$name,
              kind = get_document_symbol_kind(defn$type),
              uri = doc_uri,
              range = defn$range,
              selectionRange = defn$range,
              data = list(
                definition = list(
                  uri = doc_uri,
                  range = defn$range
                )
              )
            ),
            fromRanges = list()
          ))
        }

        defn_item <- in_calls$get(defn)
        defn_item$fromRanges <- c(
          defn_item$fromRanges,
          list(
            range(
              start = doc$to_lsp_position(
                row = symbol_line1[[i]] - 1,
                col = symbol_col1[[i]] - 1
              ),
              end = doc$to_lsp_position(
                row = symbol_line2[[i]] - 1,
                col = symbol_col2[[i]]
              )
            )
          )
        )

        in_calls$set(defn, defn_item)
      }
    }
  }

  result <- in_calls$values()
  logger$info("call_hierarchy_incoming_calls_reply: ", result)

  Response$new(id, result = result)
}

call_hierarchy_outgoing_calls_reply <- function(id, workspace, item) {
  logger$info("call_hierarchy_outgoing_calls_reply: ", item)

  doc <- workspace$documents$get(item$uri)
  xdoc <- workspace$get_parse_data(item$uri)$xml_doc

  if (is.null(xdoc)) {
    return(Response$new(id))
  }

  result <- list()
  start_point <- doc$from_lsp_position(item$range$start)
  end_point <- doc$from_lsp_position(item$range$end)
  line1 <- start_point$row + 1
  col1 <- start_point$col + 1
  line2 <- end_point$row + 1
  col2 <- end_point$col

  symbols <- xml_find_all(xdoc,
    glue("//SYMBOL_FUNCTION_CALL[
        ((@line1 = {line1} and @col1 >= {col1}) or @line1 > {line1}) and
        ((@line2 = {line2} and @col2 <= {col2}) or @line2 < {line2})]",
      line1 = line1, col1 = col1, line2 = line2, col2 = col2
    )
  )

  out_calls <- collections::dict()

  symbol_names <- xml_name(symbols)
  symbol_text <- xml_text(symbols)
  symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
  symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
  symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
  symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

  for (i in seq_along(symbols)) {
    symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
    symbol_defn <- definition_reply(NULL, item$uri, workspace, doc, symbol_point)$result

    if (is.null(symbol_defn) || equal_definition(symbol_defn, item$data$definition)) {
      next
    }

    if (!out_calls$has(symbol_defn)) {
      out_calls$set(symbol_defn, list(
        to = list(
          name = symbol_text[[i]],
          kind = SymbolKind$Function,
          uri = symbol_defn$uri,
          range = symbol_defn$range,
          selectionRange = symbol_defn$range,
          data = list(
            definition = symbol_defn
          )
        ),
        fromRanges = list()
      ))
    }

    defn_item <- out_calls$get(symbol_defn)

    defn_item$fromRanges <- c(
      defn_item$fromRanges,
      list(
        range(
          start = doc$to_lsp_position(
            row = symbol_line1[[i]] - 1,
            col = symbol_col1[[i]] - 1
          ),
          end = doc$to_lsp_position(
            row = symbol_line2[[i]] - 1,
            col = symbol_col2[[i]]
          )
        )
      )
    )

    out_calls$set(symbol_defn, defn_item)
  }

  result <- out_calls$values()

  logger$info("call_hierarchy_outgoing_calls_reply: ", result)

  Response$new(id, result = result)
}
