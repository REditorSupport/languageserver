#' The response to a textDocument/hover Request
#'
#' When hovering on a symbol, if it is a function, return its help text
#' if it exists in the current [Workspace].
#' @keywords internal
hover_reply <- function(id, uri, workspace, document, position) {
    if (!check_scope(uri, document, position)) {
        return(Response$new(id))
    }

    token_result <- document$detect_token(position)

    contents <- workspace$get_help(token_result$token, token_result$package)

    if (is.null(contents)) {
        # try function signature
        sig <- workspace$get_signature(token_result$token, token_result$package)
        logger$info("sig: ", sig)
        if (is.null(sig)) {
            xdoc <- workspace$get_xml_doc(uri)
            if (!is.null(xdoc)) {
                resolved <- FALSE

                if (!resolved) {
                    token <- find_symbol(xdoc, position$line + 1, position$character + 1)
                    if (length(token) == 1L) {
                        token_text <- xml2::xml_text(token)
                        enclosing_scopes <- find_enclosing_scopes2(xdoc, position$line + 1, position$character + 1)
                        def_lines <- xml2::xml_find_all(enclosing_scopes, sprintf("
                                            expr[FUNCTION and SYMBOL_FORMALS[text() = '%s']]/@line1 |
                                            expr[LEFT_ASSIGN/preceding-sibling::expr/SYMBOL[text() = '%s']]/@line1 |
                                            expr[RIGHT_ASSIGN/following-sibling::expr/SYMBOL[text() = '%s']]/@line1 |
                                            equal_assign[expr[1]/SYMBOL[text() = '%s']]/@line1",
                            token_text, token_text, token_text, token_text))
                        if (length(def_lines)) {
                            last_def_line <- as.integer(xml2::xml_text(def_lines[[length(def_lines)]]))
                            contents <- sprintf("```r\n%s\n```", trimws(document$line(last_def_line)))
                            resolved <- TRUE
                        }
                    }
                }
                
                if (!resolved) {
                    token <- find_symbol_sub(xdoc, position$line + 1, position$character + 1)
                    if (length(token) == 1) {
                        package <- xml2::xml_text(xml2::xml_find_all(token, "preceding-sibling::expr/SYMBOL_PACKAGE/text()"))
                        funct <- xml2::xml_text(xml2::xml_find_all(token, "preceding-sibling::expr/SYMBOL_FUNCTION_CALL/text()"))
                        if (length(package) == 0) {
                            package <- NULL
                        }
                        doc <- workspace$get_documentation(funct, package)
                        doc_string <- doc$arguments[[xml2::xml_text(token)]]
                        if (!is.null(doc_string)) {
                            contents <- doc_string
                            resolved <- TRUE
                        }
                    }
                }
                
            }
        } else {
            sig <- trimws(gsub("function\\s*", token_result$token, sig))
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
                range = token_result$range
            )
        )
    }
}

find_enclosing_scopes2 <- function(x, line, col) {
    xpath <- sprintf("/exprlist | //expr[(@line1 < %d or (@line1 = %d and @col1 <= %d)) and
        (@line2 > %d or (@line2 = %d and @col2 >= %d))]",
        line, line, col, line, line, col)
    xml2::xml_find_all(x, xpath)
}

find_symbol <- function(x, line, col) {
    xpath <- sprintf("//expr[(@line1 = %d and @col1 <= %d) and
          (@line2 = %d and @col2 >= %d)]/SYMBOL",
        line, col, line, col)
    xml2::xml_find_all(x, xpath)
}

find_symbol_sub <- function(x, line, col) {
    xpath <- sprintf("//expr/SYMBOL_SUB[(@line1 = %d and @col1 <= %d) and
          (@line2 = %d and @col2 >= %d)]",
        line, col, line, col)
    xml2::xml_find_all(x, xpath)
}
