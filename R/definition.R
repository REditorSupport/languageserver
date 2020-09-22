definition_xpath <- paste(
    "FUNCTION/following-sibling::SYMBOL_FORMALS[text() = '{token_quote}' and @line1 <= {row}]",
    "*[LEFT_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and LEFT_ASSIGN/following-sibling::expr[@start > {start} or @end < {end}]]",
    "*[RIGHT_ASSIGN/following-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and RIGHT_ASSIGN/preceding-sibling::expr[@start > {start} or @end < {end}]]",
    "*[EQ_ASSIGN/preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and EQ_ASSIGN/following-sibling::expr[@start > {start} or @end < {end}]]",
    "forcond/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]",
    sep = "|")

#' Get the location of a specified function definition
#'
#' If the function is not found in a file but is found in a loaded package,
#' writes the function definition to a temporary file and returns that
#' as the location.
#' @keywords internal
definition_reply <- function(id, uri, workspace, document, point) {

    token_result <- document$detect_token(point)
    resolved <- FALSE
    result <- NULL

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
            logger$info("definition: ", token_name, token_text)
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
                # symbol
                preceding_dollar <- xml_find_first(token, "preceding-sibling::OP-DOLLAR")
                if (length(preceding_dollar) == 0) {
                    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                        row, col, top = TRUE)
                    xpath <- glue(definition_xpath,
                        row = row, start = token_start, end = token_end,
                        token_quote = xml_single_quote(token_text))
                    all_defs <- xml_find_all(enclosing_scopes, xpath)
                    if (length(all_defs)) {
                        last_def <- all_defs[[length(all_defs)]]
                        result <- list(
                            uri = uri,
                            range = range(
                                start = document$to_lsp_position(
                                    row = as.integer(xml_attr(last_def, "line1")) - 1,
                                    col = as.integer(xml_attr(last_def, "col1")) - 1),
                                end = document$to_lsp_position(
                                    row = as.integer(xml_attr(last_def, "line2")) - 1,
                                    col = as.integer(xml_attr(last_def, "col2")))
                            )
                        )
                        logger$info("definition: ", result)
                        resolved <- TRUE
                    }
                }
            } else {
                resolved <- TRUE
            }
        }
    }

    if (!resolved) {
        result <- workspace$get_definition(token_result$token, token_result$package,
            exported_only = token_result$accessor != ":::")
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
