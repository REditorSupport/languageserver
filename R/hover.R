hover_xpath <- paste(
    "FUNCTION[following-sibling::SYMBOL_FORMALS[text() = '{token_quote}' and @line1 <= {row}]]/parent::expr",
    "*[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and LEFT_ASSIGN/following-sibling::expr[@start > {start} or @end < {end}]]",
    "*[RIGHT_ASSIGN/following-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and RIGHT_ASSIGN/preceding-sibling::expr[@start > {start} or @end < {end}]]",
    "*[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and EQ_ASSIGN/following-sibling::expr[@start > {start} or @end < {end}]]",
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

    contents <- NULL
    resolved <- FALSE

    version <- workspace$get_parse_data(uri)$version
    logger$info("hover:", list(uri = uri, version = version))

    xdoc <- workspace$get_parse_data(uri)$xml_doc

    if (token_result$accessor == "" && !is.null(xdoc)) {
        row <- point$row + 1
        col <- point$col + 1
        token <- xdoc_find_token(xdoc, row, col)
        if (length(token)) {
            token_name <- xml_name(token)
            token_text <- xml_text(token)
            token_start <- as.integer(xml_attr(token, "start"))
            token_end <- as.integer(xml_attr(token, "end"))
            logger$info(token_name, token_text)
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                # symbol
                preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                if (length(preceding_dollar) == 0) {
                    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                        row, col, top = TRUE)
                    xpath <- glue(hover_xpath,
                        row = row, start = token_start, end = token_end,
                        token_quote = xml_single_quote(token_text))
                    all_defs <- xml_find_all(enclosing_scopes, xpath)
                    if (length(all_defs)) {
                        last_def <- all_defs[[length(all_defs)]]
                        def_func <- xml_find_first(last_def,
                            "self::expr[LEFT_ASSIGN | RIGHT_ASSIGN | EQ_ASSIGN]/expr[FUNCTION]")
                        if (length(def_func)) {
                            func_line1 <- as.integer(xml_attr(def_func, "line1"))
                            func_col1 <- as.integer(xml_attr(def_func, "col1"))
                            func_line2 <- as.integer(xml_attr(def_func, "line2"))
                            func_col2 <- as.integer(xml_attr(def_func, "col2"))
                            func_text <- get_range_text(document$content,
                                line1 = func_line1,
                                col1 = func_col1,
                                line2 = func_line2,
                                col2 = func_col2
                            )
                            func_expr <- parse(text = func_text, keep.source = FALSE)
                            def_text <- get_signature(token_text, func_expr[[1]])
                            def_line1 <- func_line1
                        } else {
                            def_line1 <- as.integer(xml_attr(last_def, "line1"))
                            def_line2 <- def_line1
                            def_text <- trimws(
                                paste0(document$content[def_line1:def_line2],
                                    collapse = "\n")
                            )
                        }
                        doc_string <- NULL
                        doc_line1 <- detect_comments(document$content, def_line1 - 1) + 1
                        if (doc_line1 < def_line1) {
                            comment <- document$content[doc_line1:(def_line1 - 1)]
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
                        }
                        contents <- c(sprintf("```r\n%s\n```", def_text), doc_string)
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
                        enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                            row, col, top = TRUE)
                        xpath <- glue(signature_xpath, row = row,
                            token_quote = xml_single_quote(funct))
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
                            sig <- get_signature(funct, func_expr[[1]])
                            doc_string <- NULL

                            doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                            if (doc_line1 < func_line1) {
                                comment <- document$content[doc_line1:(func_line1 - 1)]
                                doc <- convert_comment_to_documentation(comment)
                                if (is.list(doc)) {
                                    doc_string <- doc$arguments[[token_text]]
                                    if (!is.null(doc_string)) {
                                        contents <- c(
                                            sprintf("```r\n%s\n```", sig),
                                            sprintf("`%s` - %s", token_text, doc_string))
                                    }
                                }
                            }

                            resolved <- TRUE
                        }
                    }

                    if (!resolved) {
                        doc <- workspace$get_documentation(funct, package, isf = TRUE)
                        doc_string <- NULL
                        if (is.list(doc)) {
                            doc_string <- doc$arguments[[token_text]]
                            if (is.null(doc_string)) {
                                doc_string <- doc$arguments$...
                                token_text <- "..."
                            }
                        }
                        if (!is.null(doc_string)) {
                            sig <- workspace$get_signature(funct, package)
                            if (is.null(sig)) {
                                contents <- doc_string
                            } else {
                                sig <- str_trunc(sig, 300)
                                contents <- c(
                                    sprintf("```r\n%s\n```", sig),
                                    sprintf("`%s` - %s", token_text, doc_string))
                            }
                        }
                        resolved <- TRUE
                    }
                }
            } else if (token_name == "SYMBOL_FORMALS") {
                # function formals
                # contents <- "function parameter"
                resolved <- TRUE
            } else if (token_name == "SYMBOL_PACKAGE") {
                # package
                if (length(find.package(token_text, quiet = TRUE))) {
                    desc <- utils::packageDescription(token_text, fields = c("Title", "Description"))
                    description <- gsub("\\s*\n\\s*", " ", desc$Description)
                    contents <- sprintf("**%s**\n\n%s", desc$Title, description)
                } else {
                    contents <- sprintf("Package `%s` is not installed.", token_text)
                }
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
        if (is.null(contents)) {
            def_text <- NULL

            doc <- workspace$get_documentation(token_result$token, token_result$package)
            signs <- if (is.null(token_result$package)) {
                workspace$guess_namespace(token_result$token)
            } else {
                token_result$package
            }
            sig <- workspace$get_signature(token_result$token, signs,
                exported_only = token_result$accessor != ":::")

            if (is.null(sig)) {
                def <- workspace$get_definition(token_result$token, token_result$package,
                    exported_only = token_result$accessor != ":::")
                if (!is.null(def)) {
                    def_doc <- workspace$documents$get(def$uri)
                    def_line1 <- def$range$start$line + 1
                    def_text <- def_doc$line(def_line1)
                }
            } else {
                def_text <- sig
            }

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
            contents <- c(
                if (!is.null(def_text)) sprintf("```r\n%s\n```", def_text),
                doc_string
            )
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
