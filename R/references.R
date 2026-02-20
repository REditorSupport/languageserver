references_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and text() = '{token_quote}']"

#' @noRd
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
        doc_uris <- workspace$documents$keys()
        doc_results <- lapply(doc_uris, function(doc_uri) {
            doc <- workspace$documents$get(doc_uri)
            xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
            if (is.null(xdoc)) {
                return(list())
            }

            symbols <- xml_find_all(xdoc, glue(references_xpath, token_quote = token_quote))
            if (length(symbols) == 0) {
                return(list())
            }

            line1 <- as.integer(xml_attr(symbols, "line1"))
            col1 <- as.integer(xml_attr(symbols, "col1"))
            line2 <- as.integer(xml_attr(symbols, "line2"))
            col2 <- as.integer(xml_attr(symbols, "col2"))

            matches <- vector("list", length(symbols))
            idx <- 0L
            for (i in seq_along(symbols)) {
                symbol_point <- list(row = line1[[i]] - 1, col = col1[[i]])
                symbol_defn <- definition_reply(NULL, doc_uri, workspace, doc, symbol_point)
                if (identical(symbol_defn$result, defn$result)) {
                    idx <- idx + 1L
                    matches[[idx]] <- list(
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
                    )
                }
            }

            if (idx == 0L) {
                return(list())
            }
            if (idx < length(matches)) {
                matches <- matches[seq_len(idx)]
            }
            matches
        })

        if (length(doc_results)) {
            result <- do.call(c, doc_results)
        }
    }

    logger$info("references_reply: ", result)

    Response$new(
        id,
        result = result
    )
}
