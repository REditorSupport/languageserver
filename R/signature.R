signature_xpath <- paste(
    "*[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION]",
    "*[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION]",
    sep = "|")

#' the response to a textDocument/signatureHelp Request
#'
#' If the symbol at the current position is a function, return its arguments
#' (as with [base::args()]).
#'
#' @keywords internal
signature_reply <- function(id, uri, workspace, document, point) {

    if (!check_scope(uri, document, point)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    result <- document$detect_call(point)

    SignatureInformation <- list()
    activeSignature <- -1
    sig <- NULL

    if (nzchar(result$token)) {
        xdoc <- workspace$get_parse_data(uri)$xml_doc
        if (result$accessor == "" && !is.null(xdoc)) {
            row <- point$row + 1
            col <- point$col + 1
            enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                row, col, top = TRUE)
            xpath <- glue(signature_xpath, row = row,
                token_quote = xml_single_quote(result$token))
            all_defs <- xml_find_all(enclosing_scopes, xpath)
            if (length(all_defs)) {
                last_def <- all_defs[[length(all_defs)]]
                func_line1 <- as.integer(xml_attr(last_def, "line1"))
                func_col1 <- as.integer(xml_attr(last_def, "col1"))
                func_line2 <- as.integer(xml_attr(last_def, "line2"))
                func_col2 <- as.integer(xml_attr(last_def, "col2"))
                func_text <- get_range_text(document$content,
                    line1 = func_line1,
                    col1 = func_col1,
                    line2 = func_line2,
                    col2 = func_col2
                )
                func_expr <- parse(text = func_text, keep.source = FALSE)
                sig <- get_signature(result$token, func_expr[[1]])
                documentation <- NULL

                doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                if (doc_line1 < func_line1) {
                    comment <- document$content[doc_line1:(func_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                    if (is.character(doc)) {
                        doc_string <- doc
                    } else if (is.list(doc)) {
                        if (is.null(doc$markdown)) {
                            doc_string <- doc$description
                        } else {
                            doc_string <- doc$markdown
                        }
                    }
                    documentation <- list(kind = "markdown", value = doc_string)
                }

                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation
                ))
                activeSignature <- 0
            }
        }

        if (is.null(sig)) {
            sig <- workspace$get_signature(result$token, result$package,
                exported_only = result$accessor != ":::")
            logger$info("sig: ", sig)
            if (!is.null(sig)) {
                doc <- workspace$get_documentation(result$token, result$package, isf = TRUE)
                doc_string <- ""
                if (is.character(doc)) {
                    doc_string <- doc
                } else if (is.list(doc)) {
                    doc_string <- doc$description
                }
                documentation <- list(kind = "markdown", value = doc_string)

                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation
                ))
                activeSignature <- 0
            }
        }
    }

    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = activeSignature
        )
    )
}
