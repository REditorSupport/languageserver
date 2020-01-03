hover_xpath <- paste(
    "FUNCTION[following-sibling::SYMBOL_FORMALS[text() = '{token_quote}' and @line1 <= {row}]]/parent::expr",
    "expr[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]",
    "expr[RIGHT_ASSIGN/following-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]",
    "equal_assign[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]]",
    "forcond/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]",
    sep = "|")

#' The response to a textDocument/hover Request
#'
#' When hovering on a symbol, if it is a function, return its help text
#' if it exists in the current [Workspace].
#' @keywords internal
hover_reply <- function(id, uri, workspace, document, point) {
    if (!check_scope(uri, document, point)) {
        return(Response$new(id))
    }

    token_result <- document$detect_token(point)
    range <- token_result$range

    if (is.null(token_result$package)) {
        signs <- workspace$guess_namespace(token_result$token)
    } else {
        signs <- token_result$package
    }

    sig <- workspace$get_signature(token_result$token, signs,
        exported_only = token_result$accessor != ":::")
    contents <- NULL

    if (!is.null(sig)) {
        logger$info("sig: ", sig)
        sig <- trimws(gsub("function\\s*", token_result$token, sig))
    }

    resolved <- FALSE
    xdoc <- workspace$get_xml_doc(uri)

    if (token_result$accessor == "" && !is.null(xdoc)) {
        row <- point$row + 1
        col <- point$col + 1
        token <- xdoc_find_token(xdoc, row, col)
        if (length(token)) {
            token_name <- xml_name(token)
            token_text <- xml_text(token)
            logger$info(token_name, token_text)
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                # symbol
                preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                if (length(preceding_dollar) == 0 && (is.null(signs) || signs != WORKSPACE || is.null(sig))) {
                    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                        row, col, top = TRUE)
                    token_quote <- xml_single_quote(token_text)
                    xpath <- glue(hover_xpath, row = row, token_quote = token_quote)
                    all_defs <- xml_find_all(enclosing_scopes, xpath)
                    if (length(all_defs)) {
                        last_def <- all_defs[[length(all_defs)]]
                        def_funct <- xml_find_first(last_def, "FUNCTION")
                        if (length(def_funct)) {
                            def_funct_end <- xml_find_first(last_def,
                                glue("SYMBOL_FORMALS[text() = '{token_quote}']", token_quote = token_quote))
                            def_line1 <- as.integer(xml_attr(def_funct, "line1"))
                            def_line2 <- as.integer(xml_attr(def_funct_end, "line2"))
                            def_lines <- seq.int(def_line1, def_line2)
                        } else {
                            def_lines <- as.integer(xml_attr(last_def, "line1"))
                        }
                        contents <- sprintf("```r\n%s\n```",
                            trimws(paste0(document$line(def_lines), collapse = "\n")))
                        resolved <- TRUE
                    }
                }
            } else if (token_name == "SYMBOL_SUB") {
                # function parameter
                funct <- xml_text(xml_find_first(token,
                    "preceding-sibling::expr/SYMBOL_FUNCTION_CALL/text()"))
                if (!is.na(funct)) {
                    package <- xml_text(xml_find_first(token,
                        "preceding-sibling::expr/SYMBOL_PACKAGE/text()"))
                    if (is.na(package)) {
                        package <- NULL
                    }
                    doc <- workspace$get_documentation(funct, package, isf = TRUE)
                    doc_string <- doc$arguments[[token_text]]
                    if (is.null(doc_string)) {
                        doc_string <- doc$arguments$...
                        token_text <- "..."
                    }
                    if (!is.null(doc_string)) {
                        sig <- workspace$get_signature(funct, package)
                        if (is.null(sig)) {
                            contents <- doc_string
                        } else {
                            sig <- trimws(gsub("function\\s*", funct,
                                stringr::str_trunc(sig, 300)))
                            contents <- c(
                                sprintf("```r\n%s\n```", sig),
                                sprintf("`%s`: %s", token_text, doc_string))
                        }
                    }
                    resolved <- TRUE
                }
            } else if (token_name == "SYMBOL_FORMALS") {
                # function formals
                # contents <- "function parameter"
                resolved <- TRUE
            } else if (token_name == "SLOT") {
                # S4 slot
                resolved <- TRUE
            } else if (token_name == "NUM_CONST") {
                # logical, integer, double
                # contents <- "number"
                # resolved <- TRUE
            } else if (token_name == "STR_CONST") {
                # string literal
                # contents <- "string"
                resolved <- TRUE
            } else if (token_name == "COMMENT") {
                # comment
                resolved <- TRUE
            }
        }
    }

    if (!resolved) {
        contents <- workspace$get_help(token_result$token, token_result$package)
        if (is.null(contents) && !is.null(sig)) {
            contents <- sprintf("```r\n%s\n```", sig)
        }
    }

    if (is.null(contents)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = list(
                contents = contents,
                range = range(
                    start = document$to_lsp_position(row = range$start$row, col = range$start$col),
                    end = document$to_lsp_position(row = range$end$row, col = range$end$col)
                )
            )
        )
    }
}
