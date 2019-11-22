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

    contents <- NULL
    resolved <- FALSE
    xdoc <- workspace$get_xml_doc(uri)

    if (!is.null(xdoc)) {
        # symbol
        if (!resolved) {
            token <- xdoc_find_symbol(xdoc,
                position$line + 1, position$character + 1)
            if (length(token) == 1L) {
                token <- xml_text(token)
                enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                    position$line + 1, position$character + 1, top = TRUE)
                xpath <- glue("
                            expr[FUNCTION and SYMBOL_FORMALS[text() = '{token}']]/@line1 |
                            expr[LEFT_ASSIGN/preceding-sibling::expr/SYMBOL[text() = '{token}']]/@line1 |
                            expr[RIGHT_ASSIGN/following-sibling::expr/SYMBOL[text() = '{token}']]/@line1 |
                            equal_assign[expr[1]/SYMBOL[text() = '{token}']]/@line1 |
                            forcond/SYMBOL[text() = '{token}']/@line1")
                def_lines <- xml_find_all(enclosing_scopes, xpath)
                if (length(def_lines)) {
                    last_def_line <- as.integer(xml_text(def_lines[[length(def_lines)]]))
                    contents <- sprintf("```r\n%s\n```", trimws(document$line(last_def_line)))
                    resolved <- TRUE
                }
            }
        }
        
        # function parameter
        if (is.null(contents)) {
            token <- xdoc_find_symbol_sub(xdoc,
                position$line + 1, position$character + 1)
            if (length(token) == 1) {
                package <- xml_text(xml_find_all(token,
                    "preceding-sibling::expr/SYMBOL_PACKAGE/text()"))
                funct <- xml_text(xml_find_all(token,
                    "preceding-sibling::expr/SYMBOL_FUNCTION_CALL/text()"))
                if (length(package) == 0) {
                    package <- NULL
                }
                argument <- xml_text(token)
                doc <- workspace$get_documentation(funct, package)
                doc_string <- doc$arguments[[argument]]
                if (!is.null(doc_string)) {
                    sig <- workspace$get_signature(funct, package)
                    if (is.null(sig)) {
                        contents <- doc_string
                    } else {
                        sig <- trimws(gsub("function\\s*", funct, sig))
                        contents <- sprintf("```r\n%s\n```\n`%s`: %s", sig, argument, doc_string)
                    }
                }
                resolved <- TRUE
            }
        }
    }

    if (!resolved) {
        contents <- workspace$get_help(token_result$token, token_result$package)
        
        if (is.null(contents)) {
            # try function signature
            sig <- workspace$get_signature(token_result$token, token_result$package)
            logger$info("sig: ", sig)
            if (!is.null(sig)) {
                sig <- trimws(gsub("function\\s*", token_result$token, sig))
                contents <- sprintf("```r\n%s\n```", sig)
            }
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
