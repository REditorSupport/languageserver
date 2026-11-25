definition_xpath <- paste(
    "(FUNCTION | OP-LAMBDA)/following-sibling::SYMBOL_FORMALS[text() = '{token_quote}' and @line1 <= {row}]",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)[(LEFT_ASSIGN | EQ_ASSIGN)[preceding-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and following-sibling::expr[@start > {start} or @end < {end}]]]",
    "(* | descendant-or-self::expr | descendant-or-self::expr_or_assign_or_help)[RIGHT_ASSIGN[following-sibling::expr[count(*)=1]/SYMBOL[text() = '{token_quote}' and @line1 <= {row}] and preceding-sibling::expr[@start > {start} or @end < {end}]]]",
    "forcond/SYMBOL[text() = '{token_quote}' and @line1 <= {row}]",
    sep = "|")

#' Get the location of a specified function definition
#'
#' If the function is not found in a file but is found in a loaded package,
#' returns a generated read-only document when supported by the client,
#' otherwise writes the function definition to a temporary file.
#' @noRd
definition_reply <- function(id, uri, workspace, document, point,
    rootPath = get_root_path_for_uri(uri, workspace$root),
    context_uri = uri, text_document_content = NULL, embedded = FALSE) {
    context <- if (!embedded) glue_cursor_context(document, point, workspace)
    if (!is.null(context)) return(glue_definition_reply(id, uri, workspace, context, rootPath, text_document_content))

    if (!check_r_region(document, point)) {
        return(Response$new(id))
    }

    if (isTRUE(lsp_settings$get("member_completion"))) {
        member <- member_definition_location(uri, workspace, document, point)
        if (!is.null(member)) {
            if (!is.null(member$body)) {
                member$result <- package_body_location(member$package, member$key, deparse(member$body), text_document_content)
            }
            return(Response$new(id, result = member$result))
        }
        parsed <- document$parse_data
        row <- member_navigation_token(parsed, point)
        if (row && parsed$syntax_data$token[[row]] %in% c("SYMBOL_SUB", "STR_CONST")) {
            tag <- member_navigation_at(workspace, document, point)
            if (!is.null(tag$definition) && tag$kind %in% c("tag", "property_alias")) {
                return(Response$new(id, result = tag$definition))
            }
        }
        if (is.null(parsed$syntax_data)) {
            formal <- xml_argument_definition(document, parsed, point)
            if (!is.null(formal)) return(Response$new(id, result = formal))
        }
    }

    s4 <- s4_definition_at(uri, workspace, document, point)
    if (length(s4)) return(Response$new(id, result = s4))

    symbol_definition_reply(id, uri, workspace, document, point, rootPath, context_uri, text_document_content,
        project_helpers = TRUE)
}

# Older parse snapshots have XML instead of the compact navigation index.
xml_argument_definition <- function(document, parsed, point) {
    xdoc <- parse_data_xml(parsed)
    if (is.null(xdoc)) return(NULL)
    row <- point$row + 1L
    col <- point$col + 1L
    token <- xdoc_find_token(xdoc, row, col)
    if (!identical(xml_name(token), "SYMBOL_SUB")) return(NULL)
    name <- xml_find_chr_(token, "string(../expr[1][count(*)=1]/SYMBOL_FUNCTION_CALL)")
    if (!nzchar(name)) return(NULL)
    xpath <- glue(signature_xpath, row = row, token_quote = xml_single_quote(name))
    fns <- xdoc_find_definitions(xdoc, row, col, name, xpath)
    if (!length(fns)) return(NULL)
    args <- xml_find_all_(fns[[length(fns)]], "SYMBOL_FORMALS")
    i <- match(xml_text(token), xml_text(args))
    if (!is.na(i)) location(document$uri, get_element_range(document, args[[i]]))
}

s4_definition_operators <- c("SPECIAL", "'+'", "'-'", "'*'", "'/'", "'^'", "':'", "'!'",
    "AND", "OR", "LT", "LE", "GT", "GE", "EQ", "NE", "'['", "LBB")

# An S4 call may have several source definitions. Only Go to Definition takes
# this path: providers that identify a lexical binding need a single location.
s4_definition_at <- function(uri, workspace, document, point) {
    parsed <- workspace$get_parse_data(uri)
    syntax <- parsed$syntax_data
    if (is.null(syntax)) return(NULL)
    row <- point$row + 1L
    col <- point$col + 1L
    # On x[f(i)], selecting the bracket and selecting the first letter of f
    # refer to different calls even when both have S4 methods.
    token <- navigation_token(parsed, row, col, c("SYMBOL", "SYMBOL_FUNCTION_CALL", s4_definition_operators))
    if (!token) return(NULL)
    kind <- syntax$token[[token]]
    if (kind == "STR_CONST") {
        name <- s4_definition_literal(parsed, token)
        if (is.null(name)) return(NULL)
    } else {
        name <- syntax$text[[token]]
        if (kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
            # Qualified names, slots and member calls belong to their receiver.
            parent <- syntax$parent_row[[token]]
            if (length(syntax_children(syntax, parent)) != 1L) return(NULL)
            name <- reference_symbol_names(name)
        } else if (!kind %in% s4_definition_operators) {
            return(NULL)
        } else if (kind == "'^'") {
            name <- "^" # R also spells this operator **.
        }
        if (kind %in% c("SYMBOL_FUNCTION_CALL", "'['", "LBB") && s4_definition_replacement(syntax, token)) {
            name <- paste0(name, "<-")
        }
    }

    direct <- workspace_s4_definitions(workspace, uri, name)
    targets <- direct
    for (group in s4_definition_groups(name)) {
        targets <- c(targets, workspace_s4_definitions(workspace, uri, group, methods_only = TRUE))
    }
    if (!length(targets)) return(NULL)
    if (kind != "STR_CONST") {
        binding <- s4_masking_definition(workspace, document, point, token, name, direct)
        if (!is.null(binding)) return(binding)
    }
    if (length(targets) == 1L) targets[[1L]] else targets
}

# Reference, rename and call-hierarchy indexes track lexical symbols separately
# from member declarations. Keep their existing identity resolution.
symbol_definition_reply <- function(id, uri, workspace, document, point,
    rootPath = get_root_path_for_uri(uri, workspace$root),
    context_uri = uri, text_document_content = NULL, project_helpers = FALSE) {
    if (!check_r_region(document, point)) return(Response$new(id))
    parse_data <- workspace$get_parse_data(uri)
    token_result <- reference_token_at(document, parse_data, point)
    resolved <- FALSE
    result <- NULL
    token_name <- NULL

    syntax <- parse_data$syntax_data
    if (token_result$accessor == "" && !is.null(syntax)) {
        row <- point$row + 1L
        col <- point$col + 1L
        token <- navigation_token(parse_data, row, col)
        if (token) {
            token_name <- syntax$token[[token]]
            token_text <- syntax$text[[token]]
            if (token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) {
                if (!navigation_after_dollar(syntax, token)) {
                    found <- navigation_definitions(parse_data, row, col, token_text,
                        token = token, last = TRUE)
                    if (length(found)) {
                        result <- list(uri = uri, range = navigation_range(document, syntax, found[[1L]]))
                        resolved <- TRUE
                    }
                }
            } else if (token_name == "STR_CONST") {
                first <- syntax$line1[[token]]
                if (first == syntax$line2[[token]]) {
                    text <- substr(document$content[[first]], syntax$col1[[token]], syntax$col2[[token]])
                    text <- tryCatch(as.character(str2lang(text)), error = function(e) NULL)
                    if (is.character(text) && with_wd(rootPath,
                            file.exists(text) && !dir.exists(text) && is_text_file(text))) {
                        result <- list(uri = path_to_uri(fs::path_abs(text, rootPath)),
                            range = range(position(0, 0), position(0, 0)))
                    }
                }
                resolved <- TRUE
            } else {
                resolved <- TRUE
            }
        }
    }

    xdoc <- if (is.null(syntax)) parse_data_xml(parse_data)
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
                if (xml_find_lgl_(token, "not(preceding-sibling::OP-DOLLAR)")) {
                    xpath <- glue(definition_xpath,
                        row = row, start = token_start, end = token_end,
                        token_quote = xml_single_quote(token_text))
                    all_defs <- xdoc_find_definitions(
                        xdoc, row, col, token_text, xpath)
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
                    str_text <- tryCatch(as.character(str2lang(str_expr)),
                        error = function(e) NULL)
                    if (is.character(str_text)) {
                        if (with_wd(rootPath, file.exists(str_text) && !dir.exists(str_text) && is_text_file(str_text))) {
                            path <- fs::path_abs(str_text, rootPath)
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
        if (project_helpers && length(workspace$index$helper_rules) &&
                identical(token_result$accessor, "") && length(token_name) &&
                token_name %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
            bare <- if (!is.null(syntax)) length(syntax_children(syntax, syntax$parent_row[[token]])) == 1L else
                xml_find_lgl_(token, "count(../*) = 1")
            if (bare) {
                reply <- project_helper_definition(id, workspace, context_uri, token_result$token)
                if (!is.null(reply)) return(reply)
            }
        }
        arguments <- list(token_result$token, token_result$package,
            exported_only = token_result$accessor != ":::", uri = context_uri)
        parameters <- names(formals(workspace$get_definition))
        if (!is.null(text_document_content) &&
                ("content_scheme" %in% parameters || "..." %in% parameters)) {
            arguments$content_scheme <- text_document_content$scheme
        }
        result <- do.call(call_with_optional_uri,
            c(list(workspace$get_definition), arguments))
        text <- attr(result, "text_document_content")
        if (!is.null(text)) {
            if (!text_document_content$set(result$uri, text)) {
                # Bodies exceeding the cache budget still have a usable
                # definition, using the same fallback as legacy clients.
                arguments$content_scheme <- NULL
                result <- do.call(call_with_optional_uri,
                    c(list(workspace$get_definition), arguments))
            } else {
                attr(result, "text_document_content") <- NULL
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
