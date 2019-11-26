#' Get the location of a specified function definition
#'
#' If the function is not found in a file but is found in a loaded package,
#' writes the function definition to a temporary file and returns that
#' as the location.

#' @keywords internal
definition_reply <- function(id, uri, workspace, document, position) {

    token_result <- document$detect_token(position)

    pkg <- token_result$package
    if (is.null(pkg)) {
        pkg <- workspace$guess_namespace(token_result$token)
    }

    result <- workspace$get_definition(token_result$token, token_result$package)
    logger$info("definition", result)

    xdoc <- workspace$get_xml_doc(uri)
    if (!is.null(xdoc)) {
        line <- position$line + 1
        col <- position$character + 1
        token <- xdoc_find_token(xdoc, line, col)
        logger$info("definition: ", token)
        if (length(token)) {
            token_name <- xml_name(token)
            token_text <- xml_text(token)
            token_range <- range(
                start = position(
                    line = as.integer(xml_attr(token, "line1")) - 1,
                    character = as.integer(xml_attr(token, "col1")) - 1),
                end = position(
                    line = as.integer(xml_attr(token, "line2")) - 1,
                    character = as.integer(xml_attr(token, "col2")))
            )
            logger$info("definition: ", token_name, token_text, token_range)
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                # symbol
                preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                if (length(preceding_dollar) == 0 && (is.null(pkg) || pkg != WORKSPACE || is.null(result))) {
                    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                        line, col, top = TRUE)
                    xpath <- glue(paste(
                        "expr[FUNCTION]/SYMBOL_FORMALS[text() = '{token_text}' and @line1 <= {line}] |",
                        "expr/LEFT_ASSIGN/preceding-sibling::expr/SYMBOL[text() = '{token_text}' and @line1 <= {line}] |",
                        "expr/RIGHT_ASSIGN/following-sibling::expr/SYMBOL[text() = '{token_text}' and @line1 <= {line}] |",
                        "equal_assign/expr[1]/SYMBOL[text() = '{token_text}' and @line1 <= {line}] |",
                        "forcond/SYMBOL[text() = '{token_text}' and @line1 <= {line}]"))
                    all_defs <- xml_find_all(enclosing_scopes, xpath)
                    if (length(all_defs)) {
                        last_def <- all_defs[[length(all_defs)]]
                        result <- list(
                            uri = uri,
                            range = range(
                                start = position(
                                    line = as.integer(xml_attr(last_def, "line1")) - 1, 
                                    character = as.integer(xml_attr(last_def, "col1")) - 1),
                                end = position(
                                    line = as.integer(xml_attr(last_def, "line2")) - 1,
                                    character = as.integer(xml_attr(last_def, "col2")))
                            )
                        )
                        resolved <- TRUE
                    }
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
