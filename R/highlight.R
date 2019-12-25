DocumentHighlightKind <- list(
    Text = 1,
    Read = 2,
    Write = 3
)

document_highlight_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and not(preceding-sibling::OP-DOLLAR) and text() = '{token_quote}']"

#' The response to a textDocument/documentHighlight Request
#'
#' @keywords internal
document_highlight_reply <- function(id, uri, workspace, document, point) {
    result <- NULL
    xdoc <- workspace$get_xml_doc(uri)
    if (!is.null(xdoc)) {
        row <- point$row + 1
        col <- point$col + 1
        token <- xdoc_find_token(xdoc, row, col)
        if (length(token)) {
            token_name <- xml_name(token)
            if (token_name == "COMMENT") {
                # ignore comments
            } else {
                token_text <- xml_text(token)
                token_quote <- xml_single_quote(token_text)
                logger$info("highlight: ", token_name, token_text)

                tokens <- NULL
                if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) {
                    preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                    if (length(preceding_dollar) == 0) {
                        xpath <- glue(document_highlight_xpath, token_quote = token_quote)
                        tokens <- xml_find_all(xdoc, xpath)
                    }
                } else if (token_name %in% c("SYMBOL_SUB", "SLOT")) {
                    # ignore
                } else {
                    # highlight tokens with same name and text
                    xpath <- glue("//{token_name}[text()='{token_quote}']",
                        token_name = token_name, token_quote = token_quote)
                    tokens <- xml_find_all(xdoc, xpath)
                }

                if (length(tokens)) {
                    result <- lapply(tokens, function(token) {
                        list(
                            range = range(
                                start = document$to_lsp_position(
                                    row = as.integer(xml_attr(token, "line1")) - 1,
                                    col = as.integer(xml_attr(token, "col1")) - 1),
                                end = document$to_lsp_position(
                                    row = as.integer(xml_attr(token, "line2")) - 1,
                                    col = as.integer(xml_attr(token, "col2")))
                            ),
                            kind = DocumentHighlightKind$Text
                        )
                    })
                }
            }
        }
    }

    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
