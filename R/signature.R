signature_xpath <- paste(
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION|OP-LAMBDA]",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]/expr[FUNCTION|OP-LAMBDA]",
    sep = "|")

#' Format local function arguments without reparsing the function body
#' @noRd
local_function_signature <- function(document, node, symbol) {
    closing <- xml_find_first(node, "OP-RIGHT-PAREN")
    text <- get_range_text(document$content,
        line1 = as.integer(xml_attr(node, "line1")),
        col1 = as.integer(xml_attr(node, "col1")),
        line2 = as.integer(xml_attr(closing, "line2")),
        col2 = as.integer(xml_attr(closing, "col2")))
    # Only the formals are used by get_signature(). A large body contributes
    # no signature information and need not be allocated or parsed again.
    expr <- parse(text = c(text, "NULL"), keep.source = FALSE)
    get_signature(symbol, expr[[1L]])
}

#' Parse a signature once for both parameter names and LSP label ranges
#' @noRd
signature_info <- local({
    previous_signature <- NULL
    previous_info <- NULL
    function(signature) {
        if (!identical(signature, previous_signature)) {
            previous_info <<- .Call(
                "signature_info_c", PACKAGE = "languageserver", signature)
            previous_signature <<- signature
        }
        previous_info
    }
})

#' Extract parameter names from a function signature
#' @noRd
extract_parameter_names <- function(signature) {
    signature_info(signature)$names
}

#' Parse UTF-16 parameter label ranges from a function signature
#' @noRd
parse_signature_parameters <- function(signature) {
    signature_info(signature)$parameters
}

#' Detect the active positional or named argument without parsing user code
#' @noRd
detect_active_parameter <- function(content, start_row, start_col,
    end_row, end_col, signature = NULL) {
    if (start_row < 0L || start_row >= length(content) || end_row < start_row) {
        return(0L)
    }
    last_row <- min(end_row, length(content) - 1L)
    lines <- content[seq.int(start_row + 1L, last_row + 1L)]
    missing_line <- match(TRUE, is.na(lines), nomatch = 0L)
    if (missing_line) lines <- utils::head(lines, missing_line - 1L)
    if (!length(lines)) return(0L)

    # Cursor columns are code points here (the document converts LSP units).
    # Restrict the final line before removing the opening bracket on the first.
    if (start_row + length(lines) - 1L == end_row) {
        lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, end_col)
    }
    lines[[1L]] <- substring(lines[[1L]], start_col + 2L)
    parameters <- if (is.null(signature)) character() else signature_info(signature)$names
    .Call("active_parameter_c", PACKAGE = "languageserver",
        paste0(lines, collapse = "\n"), parameters)
}

#' the response to a textDocument/signatureHelp Request
#'
#' If the symbol at the current position is a function, return its arguments
#' (as with [base::args()]).
#'
#' @noRd
signature_reply <- function(id, uri, workspace, document, point) {

    if (!check_scope(uri, document, point)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    result <- document$detect_call(point)

    SignatureInformation <- list()
    activeSignature <- NULL
    activeParameter <- NULL
    sig <- NULL

    if (nzchar(result$token)) {
        xdoc <- workspace$get_parse_data(uri)$xml_doc
        if (result$accessor == "" && !is.null(xdoc)) {
            row <- point$row + 1
            col <- point$col + 1
            xpath <- glue(signature_xpath, row = row,
                token_quote = xml_single_quote(result$token))
            all_defs <- xdoc_find_definitions(xdoc, row, col, result$token, xpath)
            if (length(all_defs)) {
                last_def <- all_defs[[length(all_defs)]]
                func_line1 <- as.integer(xml_attr(last_def, "line1"))
                sig <- local_function_signature(document, last_def, result$token)
                documentation <- ""

                doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                if (doc_line1 < func_line1) {
                    comment <- document$content[doc_line1:(func_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                    doc_string <- NULL

                    if (is.character(doc)) {
                        doc_string <- doc
                    } else if (is.list(doc)) {
                        if (is.null(doc$markdown)) {
                            doc_string <- doc$description
                        } else {
                            doc_string <- doc$markdown
                        }
                    }

                    if (is.null(doc_string)) {
                        doc_string <- ""
                    }

                    documentation <- list(kind = "markdown", value = doc_string)
                }

                parameters <- parse_signature_parameters(sig)
                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation,
                    parameters = parameters
                ))
                activeSignature <- 0
            }
        }

        if (is.null(sig)) {
            sig <- call_with_optional_uri(
                workspace$get_signature,
                result$token, result$package,
                exported_only = result$accessor != ":::", uri = uri)
            logger$info("sig: ", sig)
            if (!is.null(sig)) {
                doc <- call_with_optional_uri(
                    workspace$get_documentation,
                    result$token, result$package, isf = TRUE, uri = uri)
                doc_string <- NULL

                if (is.character(doc)) {
                    doc_string <- doc
                } else if (is.list(doc)) {
                    doc_string <- doc$description
                }

                if (is.null(doc_string)) {
                    doc_string <- ""
                }

                documentation <- list(kind = "markdown", value = doc_string)

                parameters <- parse_signature_parameters(sig)
                SignatureInformation <- list(list(
                    label = sig,
                    documentation = documentation,
                    parameters = parameters
                ))
                activeSignature <- 0
            }
        }
    }

    # Calculate activeParameter if we have a valid signature
    if (!is.null(activeSignature) && nzchar(result$token)) {
        logger$info("Calculating activeParameter for token: ", result$token)
        fub_result <- find_unbalanced_bracket(document$content, point$row, point$col - 1)
        loc <- fub_result[[1]]
        bracket <- fub_result[[2]]
        logger$info("Bracket location: row=", loc[1], ", col=", loc[2], ", bracket='", bracket, "'")
        
        if (loc[1] >= 0 && loc[2] >= 0 && bracket == "(") {
            activeParameter <- detect_active_parameter(
                document$content,
                loc[1],  # start_row (0-based)
                loc[2],  # start_col (0-based)
                point$row,  # end_row (0-based)
                point$col,   # end_col (0-based)
                sig          # signature for named argument detection
            )
            logger$info("activeParameter set to: ", activeParameter)
        } else {
            logger$info("Invalid bracket location or not a parenthesis")
        }
    }

    response_result <- list(signatures = SignatureInformation)
    response_result$activeSignature <- activeSignature
    response_result$activeParameter <- activeParameter
    
    logger$info("signature_reply result: activeSignature=", activeSignature,
                ", activeParameter=", activeParameter)

    Response$new(id, result = response_result)
}
