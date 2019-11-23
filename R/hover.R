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
    range <- token_result$range

    ns <- workspace$guess_namespace(token_result$token, isf = TRUE)
    logger$info("ns: ", ns)

    sig <- workspace$get_signature(token_result$token, ns)
    contents <- NULL
    
    if (!is.null(sig)) {
        logger$info("sig: ", sig)
        sig <- trimws(gsub("function\\s*", token_result$token, sig))
    }

    resolved <- FALSE
    xdoc <- workspace$get_xml_doc(uri)

    if (!is.null(xdoc)) {
        token <- xdoc_find_token(xdoc, position$line + 1, position$character + 1)
        logger$info(token)
        if (length(token)) {
            range <- range(
                start = position(
                    line = as.integer(xml_attr(token, "line1")) - 1,
                    character = as.integer(xml_attr(token, "col1")) - 1),
                end = position(
                    line = as.integer(xml_attr(token, "line2")) - 1, 
                    character = as.integer(xml_attr(token, "col2")) - 1)
            )
            token_name <- xml_name(token)
            token_text <- xml_text(token)
            logger$info(token_name, range)
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                # symbol
                preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                logger$info("preceding_dollar", length(preceding_dollar))
                if (length(preceding_dollar) == 0) {
                    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                        position$line + 1, position$character + 1, top = TRUE)
                    xpath <- glue("expr[FUNCTION and SYMBOL_FORMALS[text() = '{token_text}']]/@line1 |
                                                   expr[LEFT_ASSIGN/preceding-sibling::expr/SYMBOL[text() = '{token_text}']]/@line1 |
                                                   expr[RIGHT_ASSIGN/following-sibling::expr/SYMBOL[text() = '{token_text}']]/@line1 |
                                                   equal_assign[expr[1]/SYMBOL[text() = '{token_text}']]/@line1 |
                                                   forcond/SYMBOL[text() = '{token_text}']/@line1")
                    def_lines <- xml_find_all(enclosing_scopes, xpath)
                    if (length(def_lines)) {
                        last_def_line <- as.integer(xml_text(def_lines[[length(def_lines)]]))
                        contents <- sprintf("```r\n%s\n```", trimws(document$line(last_def_line)))
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
                    doc <- workspace$get_documentation(funct, package)
                    doc_string <- doc$arguments[[token_text]]
                    if (!is.null(doc_string)) {
                        sig <- workspace$get_signature(funct, package)
                        if (is.null(sig)) {
                            contents <- doc_string
                        } else {
                            sig <- trimws(gsub("function\\s*", funct, sig))
                            contents <- sprintf("```r\n%s\n```\n`%s`: %s", sig, token_text, doc_string)
                        }
                    }
                    resolved <- TRUE
                }
            } else if (token_name == "SYMBOL_FORMALS") {
                # function formals
                contents <- "function parameter"
                resolved <- TRUE
            } else if (token_name == "NUM_CONST") {
                # logical, integer, double
                contents <- "number"
                resolved <- TRUE
            } else if (token_name == "STR_CONST") {
                # string literal
                contents <- "string"
                resolved <- TRUE
            }
        }
    }

    if (!resolved) {
        contents <- workspace$get_help(token_result$token, ns)
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
                range = range
            )
        )
    }
}
