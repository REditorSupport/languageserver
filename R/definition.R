definition_xpath <- paste(
    "*[self::FUNCTION or self::OP-LAMBDA]/following-sibling::SYMBOL_FORMALS[text() = '{token_quote}' and @line1 <= {row}]",
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
#' @noRd
definition_reply <- function(id, uri, workspace, document, point, rootPath) {

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
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) {
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
            } else if (token_name == "STR_CONST") {
                str_line1 <- as.integer(xml_attr(token, "line1"))
                str_line2 <- as.integer(xml_attr(token, "line2"))
                if (str_line1 == str_line2) {
                    str_col1 <- as.integer(xml_attr(token, "col1"))
                    str_col2 <- as.integer(xml_attr(token, "col2"))
                    str_expr <- substr(document$content[str_line1], str_col1, str_col2)
                    str_text <- tryCatch(as.character(parse(text = str_expr, keep.source = FALSE)),
                        error = function(e) NULL)
                    if (is.character(str_text)) {
                        path <- fs::path_abs(str_text, rootPath)
                        if (file.exists(path) && !dir.exists(path) && is_text_file(path)) {
                            result <- list(
                                uri = path_to_uri(path),
                                range = range(
                                    start = position(0, 0),
                                    end = position(0, 0)
                                )
                            )
                        }
                    }
                }
                resolved <- TRUE
            } else {
                resolved <- TRUE
            }
        }
    }

    if (!resolved && check_scope(uri, document, point)) {
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
