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

  in_calls <- new.env()

  for (doc_uri in workspace$documents$keys()) {
    doc <- workspace$documents$get(doc_uri)
    xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
    if (is.null(xdoc)) next

    defns <- workspace$get_definitions_for_uri(doc_uri)

    for (defn in defns) {
      start_point <- doc$from_lsp_position(defn$range$start)
      end_point <- doc$from_lsp_position(defn$range$end)
      line1 <- start_point$row + 1
      col1 <- start_point$col + 1
      line2 <- end_point$row + 1
      col2 <- end_point$col

      symbols <- xml_find_all(xdoc,
        glue("//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL) and
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

      defn_hash <- digest::digest(defn)

      symbol_names <- xml_name(symbols)
      symbol_text <- xml_text(symbols)
      symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
      symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
      symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
      symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

      for (i in seq_along(symbols)) {
        symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
        symbol_defn <- definition_reply(NULL, doc_uri, workspace, doc, symbol_point)$result
        if (!identical(symbol_defn, item$data$definition)) {
          next
        }

        if (is.null(in_calls[[defn_hash]])) {
          in_calls[[defn_hash]] <- list(
            to = list(
              name = defn$name,
              kind = SymbolKind$Function,
              uri = doc_uri,
              range = defn$range,
              selectionRange = defn$range,
              data = list(
                definition = defn
              )
            ),
            fromRangess = list()
          )
        }

        in_calls[[defn_hash]]$fromRanges <- c(
          in_calls[[defn_hash]]$fromRanges,
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
      }
    }
  }

  result <- unname(as.list.environment(in_calls))

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
    glue("//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL) and
        ((@line1 = {line1} and @col1 >= {col1}) or @line1 > {line1}) and
        ((@line2 = {line2} and @col2 <= {col2}) or @line2 < {line2})]",
      line1 = line1, col1 = col1, line2 = line2, col2 = col2
    )
  )

  logger$info("call_hierarchy_outgoing_calls_reply", list(
    start_point = start_point,
    end_point = end_point,
    symbols = xml_text(symbols)
  ))

  out_calls <- new.env()

  symbol_names <- xml_name(symbols)
  symbol_text <- xml_text(symbols)
  symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
  symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
  symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
  symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

  for (i in seq_along(symbols)) {
    symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
    symbol_defn <- definition_reply(NULL, item$uri, workspace, doc, symbol_point)$result
    if (is.null(symbol_defn) || identical(symbol_defn, item$data$definition)) {
      next
    }

    defn_hash <- digest::digest(symbol_defn)
    if (is.null(out_calls[[defn_hash]])) {
      out_calls[[defn_hash]] <- list(
        to = list(
          name = symbol_text[[i]],
          kind =  SymbolKind$Function,
          uri = symbol_defn$uri,
          range = symbol_defn$range,
          selectionRange = symbol_defn$range,
          data = list(
            definition = symbol_defn
          )
        ),
        fromRangess = list()
      )
    }

    out_calls[[defn_hash]]$fromRanges <- c(
      out_calls[[defn_hash]]$fromRanges,
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
  }

  result <- unname(as.list.environment(out_calls))

  logger$info("call_hierarchy_outgoing_calls_reply: ", result)

  Response$new(id, result = result)
}
