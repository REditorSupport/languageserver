DocumentHighlightKind <- list(
    Text = 1,
    Read = 2,
    Write = 3
)

document_highlight_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and not(preceding-sibling::OP-DOLLAR) and text() = '{token_quote}']"

#' The response to a textDocument/documentHighlight Request
#'
#' @noRd
document_highlight_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(Response$new(id))
    }

    result <- NULL
    parse_data <- workspace$get_parse_data(uri)
    index <- parse_data$reference_index
    detected <- document$detect_token(point)
    if (!is.null(index) && nzchar(detected$token)) {
        token_point <- list(
            row = detected$range$start$row,
            col = detected$range$start$col
        )
        definition_key <- reference_key_at(index, token_point, detected$token)
        if (!is.null(definition_key)) {
            selected <- reference_indices(index, detected$token, definition_key)
            result <- lapply(selected, function(i) {
                list(
                    range = range(
                        position(index$line[[i]], index$col[[i]]),
                        position(index$end_line[[i]], index$end_col[[i]])
                    ),
                    kind = DocumentHighlightKind$Text
                )
            })
            return(Response$new(id, result = result))
        }
    }

    syntax <- parse_data$syntax_data
    if (!is.null(syntax)) {
        row <- navigation_token(parse_data, point$row + 1L, point$col + 1L)
        if (!row) return(Response$new(id))
        type <- syntax$token[[row]]
        if (type %in% c("SYMBOL_PACKAGE", "FUNCTION", "NUM_CONST", "STR_CONST")) {
            rows <- syntax$terminals[syntax$token[syntax$terminals] == type]
            # Match XML text semantics, including raw strings with control
            # characters, without building an XML tree for a cursor movement.
            text <- gsub("[\001-\b\v-\037]", "", enc2utf8(syntax$text[c(row, rows)]), useBytes = TRUE)
            selected <- rows[text[-1L] == text[[1L]]]
            result <- lapply(selected, function(i) {
                list(range = navigation_range(document, syntax, i), kind = DocumentHighlightKind$Text)
            })
            return(Response$new(id, result = result))
        }
        if (!type %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) return(Response$new(id))
        siblings <- syntax_children(syntax, syntax$parent_row[[row]])
        preceding <- siblings[syntax$order_rank[siblings] < syntax$order_rank[[row]]]
        if (any(syntax$token[preceding] %in% c("'$'", "OP-DOLLAR"))) return(Response$new(id))
        rows <- syntax$terminals[syntax$token[syntax$terminals] %in%
                c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")]
        rows <- rows[syntax$text[rows] == syntax$text[[row]]]
        dollars <- which(syntax$token %in% c("'$'", "OP-DOLLAR"))
        dollar <- match(syntax$parent_row[rows], syntax$parent_row[dollars])
        rows <- rows[is.na(dollar) | syntax$order_rank[rows] < syntax$order_rank[dollars[dollar]]]
        if (!length(rows)) return(Response$new(id))
        return(Response$new(id, result = lapply(rows, function(i) {
            list(range = navigation_range(document, syntax, i), kind = DocumentHighlightKind$Text)
        })))
    }

    xdoc <- parse_data_xml(parse_data)
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
                } else if (token_name %in% c(
                        "SYMBOL_PACKAGE", "FUNCTION", "NUM_CONST", "STR_CONST")) {
                    # Compare text directly: XML entities do not escape quotes
                    # inside XPath string literals (for example, 'a string').
                    tokens <- xml_find_all(xdoc, paste0("//", token_name))
                    tokens <- tokens[xml_text(tokens) == token_text]
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
