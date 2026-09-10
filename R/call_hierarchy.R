#' @noRd
indexed_position_in_range <- function(line, col, item_range) {
    after_start <- line > item_range$start$line ||
        line == item_range$start$line && col >= item_range$start$character
    before_end <- line < item_range$end$line ||
        line == item_range$end$line && col <= item_range$end$character
    after_start && before_end
}

indexed_call_range <- function(index, i) {
    range(
        position(index$line[[i]], index$col[[i]]),
        position(index$end_line[[i]], index$end_col[[i]])
    )
}

#' Select the narrowest function containing each complete call range
#' @noRd
indexed_call_containers <- function(index, selected, definitions) {
    definition_ranges <- vapply(definitions, function(definition) {
        as.integer(c(
            definition$range$start$line, definition$range$start$character,
            definition$range$end$line, definition$range$end$character
        ))
    }, integer(4L))
    .Call(
        "call_hierarchy_containers_c",
        cbind(index$line[selected], index$col[selected],
            index$end_line[selected], index$end_col[selected]),
        t(definition_ranges), PACKAGE = "languageserver"
    )
}

#' Select calls whose complete ranges fall inside the requested function
#' @noRd
indexed_calls_in_range <- function(index, item_range) {
    selected <- which(index$token == "SYMBOL_FUNCTION_CALL")
    start <- item_range$start
    end <- item_range$end
    lines <- index$line[selected]
    cols <- index$col[selected]
    end_lines <- index$end_line[selected]
    end_cols <- index$end_col[selected]
    selected[
        (lines > start$line | lines == start$line & cols >= start$character) &
            (lines < end$line | lines == end$line & cols <= end$character) &
            (end_lines > start$line |
                    end_lines == start$line & end_cols >= start$character) &
            (end_lines < end$line |
                    end_lines == end$line & end_cols <= end$character)
    ]
}

#' Create a call-hierarchy item for calls made outside a function
#' @noRd
call_hierarchy_file_item <- function(workspace, uri, selection_range,
    context_uri = uri) {
    document <- workspace$documents$get(uri, NULL)
    if (is.null(document)) return(NULL)
    last_row <- max(document$nline - 1L, 0L)
    last_col <- nchar(document$line0(last_row), type = "chars")
    file_range <- range(
        position(0L, 0L),
        document$to_lsp_position(last_row, last_col)
    )
    name <- basename(path_from_uri(uri))
    if (!length(name) || !nzchar(name)) name <- uri
    list(
        name = name,
        detail = "top level",
        kind = SymbolKind$File,
        uri = uri,
        range = file_range,
        selectionRange = selection_range,
        data = list(contextUri = context_uri, topLevel = TRUE)
    )
}

indexed_incoming_calls <- function(workspace, item) {
    target_key <- item$data$definitionKey
    if (is.null(target_key)) target_key <- paste0("global:", item$name)
    in_calls <- collections::dict()
    context_uri <- item$data$contextUri
    if (is.null(context_uri)) context_uri <- item$uri
    doc_uris <- workspace_reference_document_uris(
        workspace, item$uri, context_uri)

    for (doc_uri in doc_uris) {
        parse_data <- workspace$get_parse_data(doc_uri)
        index <- parse_data$reference_index
        if (is.null(index)) return(NULL)

        selected <- reference_indices(index, item$name, target_key)
        selected <- selected[index$token[selected] == "SYMBOL_FUNCTION_CALL"]
        if (!length(selected)) next

        definitions <- workspace$get_definitions_for_uri(doc_uri)
        definitions <- definitions[vapply(definitions, function(definition) {
            identical(definition$type, "function")
        }, logical(1L))]

        containers <- indexed_call_containers(index, selected, definitions)
        used <- unique(containers[containers > 0L])
        if (doc_uri == item$uri) {
            recursive <- used[vapply(definitions[used], function(definition) {
                equal_range(definition$range, item$data$definition$range)
            }, logical(1L))]
            keep <- !containers %in% recursive
            containers <- containers[keep]
            selected <- selected[keep]
            if (!length(selected)) next
            used <- unique(containers[containers > 0L])
        }
        keys <- rep(paste(doc_uri, "top-level", sep = ":"), length(selected))
        definition_keys <- vapply(used, function(container) {
            definition <- definitions[[container]]
            paste(doc_uri, definition$range$start$line,
                definition$range$start$character, sep = ":")
        }, character(1L))
        contained <- containers > 0L
        keys[contained] <- definition_keys[match(containers[contained], used)]
        # Build each caller's ranges in one allocation. Repeatedly appending
        # to a dictionary entry copied all earlier calls from that caller.
        groups <- split(seq_along(selected), factor(keys, levels = unique(keys)))
        for (group in groups) {
            container <- containers[[group[[1L]]]]
            indices <- selected[group]
            call_ranges <- lapply(indices, function(i) indexed_call_range(index, i))
            if (container == 0L) {
                key <- paste(doc_uri, "top-level", sep = ":")
                in_calls$set(key, list(
                    from = call_hierarchy_file_item(
                        workspace, doc_uri, call_ranges[[1L]], context_uri),
                    fromRanges = call_ranges
                ))
                next
            }

            definition <- definitions[[container]]
            key <- paste(
                doc_uri,
                definition$range$start$line,
                definition$range$start$character,
                sep = ":"
            )
            in_calls$set(key, list(
                from = list(
                    name = definition$name,
                    kind = get_document_symbol_kind(definition$type),
                    uri = doc_uri,
                    range = definition$range,
                    selectionRange = definition$range,
                    data = list(
                        definition = list(uri = doc_uri, range = definition$range),
                        contextUri = context_uri
                    )
                ),
                fromRanges = call_ranges
            ))
        }
    }

    in_calls$values()
}

indexed_outgoing_calls <- function(workspace, item) {
    doc <- workspace$documents$get(item$uri)
    parse_data <- workspace$get_parse_data(item$uri)
    index <- parse_data$reference_index
    if (is.null(index)) return(NULL)

    selected <- indexed_calls_in_range(index, item$range)
    if (!length(selected)) return(list())

    groups <- split(selected, paste(
        index$name[selected],
        index$definition_key[selected],
        index$call_package[selected],
        sep = "\r"
    ))
    result <- list()
    context_uri <- item$data$contextUri
    if (is.null(context_uri)) context_uri <- item$uri
    for (indices in groups) {
        i <- indices[[1L]]
        point <- list(
            row = index$line[[i]],
            col = index$code_point_col[[i]]
        )
        symbol_definition <- definition_reply(
            NULL, item$uri, workspace, doc, point,
            context_uri = context_uri)$result
        if (is.null(symbol_definition) ||
                equal_definition(symbol_definition, item$data$definition)) {
            next
        }

        namespace <- attr(symbol_definition, "namespace")
        detail <- if (!is.null(namespace)) sprintf("{%s}", namespace)
        result[[length(result) + 1L]] <- list(
            to = list(
                name = index$name[[i]],
                kind = SymbolKind$Function,
                uri = symbol_definition$uri,
                detail = detail,
                range = symbol_definition$range,
                selectionRange = symbol_definition$range,
                data = list(
                    definition = symbol_definition,
                    contextUri = context_uri
                )
            ),
            fromRanges = lapply(indices, function(j) indexed_call_range(index, j))
        )
    }
    result
}

#' @noRd
prepare_call_hierarchy_reply <- function(id, uri, workspace, document, point) {

    if (!check_r_region(document, point)) {
        return(Response$new(id, result = NULL))
    }

    token <- document$detect_token(point)
    defn <- definition_reply(NULL, uri, workspace, document, point)$result
    token_quote <- xml_single_quote(token$token)

    logger$info("prepare_call_hierarchy_reply: ", list(
        uri = uri,
        token = token,
        defn = defn
    ))

    result <- NULL

    if (length(defn)) {
        index <- workspace$get_parse_data(uri)$reference_index
        definition_key <- reference_key_at(index, list(
            row = token$range$start$row,
            col = token$range$start$col
        ), token$token)
        result <- list(
            list(
                name = token$token,
                kind = SymbolKind$Function,
                uri = defn$uri,
                range = defn$range,
                selectionRange = defn$range,
                data = list(
                    definition = defn,
                    definitionKey = definition_key,
                    contextUri = uri
                )
            )
        )
    }

    logger$info("prepare_call_hierarchy_reply: ", result)

    Response$new(
        id,
        result = result
    )
}

call_hierarchy_incoming_calls_reply <- function(id, workspace, item) {
    logger$info("call_hierarchy_incoming_calls_reply: ", item)

    indexed <- indexed_incoming_calls(workspace, item)
    if (!is.null(indexed)) {
        return(Response$new(id, result = indexed))
    }

    token_quote <- xml_single_quote(item$name)
    result <- list()

    in_calls <- collections::dict()

    context_uri <- item$data$contextUri
    if (is.null(context_uri)) context_uri <- item$uri
    doc_uris <- workspace_reference_document_uris(
        workspace, item$uri, context_uri)
    for (doc_uri in doc_uris) {
        doc <- workspace$documents$get(doc_uri)
        xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
        if (is.null(xdoc)) next

        defns <- workspace$get_definitions_for_uri(doc_uri)

        for (defn in defns) {
            if (doc_uri == item$uri && equal_range(defn$range, item$data$definition$range)) {
                next
            }

            start_point <- doc$from_lsp_position(defn$range$start)
            end_point <- doc$from_lsp_position(defn$range$end)
            line1 <- start_point$row + 1
            col1 <- start_point$col + 1
            line2 <- end_point$row + 1
            col2 <- end_point$col

            symbols <- xml_find_all(xdoc,
                glue("//SYMBOL_FUNCTION_CALL[
          ((@line1 = {line1} and @col1 >= {col1}) or @line1 > {line1}) and
          ((@line2 = {line2} and @col2 <= {col2}) or @line2 < {line2}) and
          text() = '{token_quote}']",
                    line1 = line1, col1 = col1, line2 = line2, col2 = col2,
                    token_quote = token_quote
                )
            )

            if (length(symbols) == 0) {
                next
            }

            defn$uri <- doc_uri

            symbol_names <- xml_name(symbols)
            symbol_text <- xml_text(symbols)
            symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
            symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
            symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
            symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

            for (i in seq_along(symbols)) {
                symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
                symbol_defn <- definition_reply(
                    NULL, doc_uri, workspace, doc, symbol_point,
                    context_uri = context_uri)$result

                if (!equal_definition(symbol_defn, item$data$definition)) {
                    next
                }

                if (!in_calls$has(defn)) {
                    in_calls$set(defn, list(
                        from = list(
                            name = defn$name,
                            kind = get_document_symbol_kind(defn$type),
                            uri = doc_uri,
                            range = defn$range,
                            selectionRange = defn$range,
                            data = list(
                                definition = list(
                                    uri = doc_uri,
                                    range = defn$range
                                ),
                                contextUri = context_uri
                            )
                        ),
                        fromRanges = list()
                    ))
                }

                defn_item <- in_calls$get(defn)
                defn_item$fromRanges <- c(
                    defn_item$fromRanges,
                    list(
                        range(
                            start = doc$to_lsp_position(
                                row = symbol_line1[[i]] - 1,
                                col = symbol_col1[[i]] - 1
                            ),
                            end = doc$to_lsp_position(
                                row = symbol_line2[[i]] - 1,
                                col = symbol_col2[[i]]
                            )
                        )
                    )
                )

                in_calls$set(defn, defn_item)
            }
        }

        symbols <- xml_find_all(xdoc,
            glue("//SYMBOL_FUNCTION_CALL[text() = '{token_quote}']",
                token_quote = token_quote))
        if (!length(symbols)) next
        symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
        symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
        symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
        symbol_col2 <- as.integer(xml_attr(symbols, "col2"))
        for (i in seq_along(symbols)) {
            call_range <- range(
                start = doc$to_lsp_position(
                    row = symbol_line1[[i]] - 1L,
                    col = symbol_col1[[i]] - 1L
                ),
                end = doc$to_lsp_position(
                    row = symbol_line2[[i]] - 1L,
                    col = symbol_col2[[i]]
                )
            )
            contained <- any(vapply(defns, function(defn) {
                indexed_position_in_range(
                    call_range$start$line,
                    call_range$start$character,
                    defn$range
                ) && indexed_position_in_range(
                    call_range$end$line,
                    call_range$end$character,
                    defn$range
                )
            }, logical(1L)))
            if (contained) next
            symbol_point <- list(
                row = symbol_line1[[i]] - 1L,
                col = symbol_col1[[i]]
            )
            symbol_defn <- definition_reply(
                NULL, doc_uri, workspace, doc, symbol_point,
                context_uri = context_uri)$result
            if (!equal_definition(symbol_defn, item$data$definition)) next

            key <- paste(doc_uri, "top-level", sep = ":")
            if (!in_calls$has(key)) {
                in_calls$set(key, list(
                    from = call_hierarchy_file_item(
                        workspace, doc_uri, call_range, context_uri),
                    fromRanges = list()
                ))
            }
            entry <- in_calls$get(key)
            entry$fromRanges[[length(entry$fromRanges) + 1L]] <- call_range
            in_calls$set(key, entry)
        }
    }

    result <- in_calls$values()
    logger$info("call_hierarchy_incoming_calls_reply: ", result)

    Response$new(id, result = result)
}

call_hierarchy_outgoing_calls_reply <- function(id, workspace, item) {
    logger$info("call_hierarchy_outgoing_calls_reply: ", item)

    indexed <- indexed_outgoing_calls(workspace, item)
    if (!is.null(indexed)) {
        return(Response$new(id, result = indexed))
    }

    doc <- workspace$documents$get(item$uri)
    xdoc <- workspace$get_parse_data(item$uri)$xml_doc

    if (is.null(xdoc)) {
        return(Response$new(id))
    }

    result <- list()
    context_uri <- item$data$contextUri
    if (is.null(context_uri)) context_uri <- item$uri
    start_point <- doc$from_lsp_position(item$range$start)
    end_point <- doc$from_lsp_position(item$range$end)
    line1 <- start_point$row + 1
    col1 <- start_point$col + 1
    line2 <- end_point$row + 1
    col2 <- end_point$col

    symbols <- xml_find_all(xdoc,
        glue("//SYMBOL_FUNCTION_CALL[
        ((@line1 = {line1} and @col1 >= {col1}) or @line1 > {line1}) and
        ((@line2 = {line2} and @col2 <= {col2}) or @line2 < {line2})]",
            line1 = line1, col1 = col1, line2 = line2, col2 = col2
        )
    )

    out_calls <- collections::dict()

    symbol_names <- xml_name(symbols)
    symbol_text <- xml_text(symbols)
    symbol_line1 <- as.integer(xml_attr(symbols, "line1"))
    symbol_col1 <- as.integer(xml_attr(symbols, "col1"))
    symbol_line2 <- as.integer(xml_attr(symbols, "line2"))
    symbol_col2 <- as.integer(xml_attr(symbols, "col2"))

    for (i in seq_along(symbols)) {
        symbol_point <- list(row = symbol_line1[[i]] - 1, col = symbol_col1[[i]])
        symbol_defn <- definition_reply(
            NULL, item$uri, workspace, doc, symbol_point,
            context_uri = context_uri)$result

        if (is.null(symbol_defn) || equal_definition(symbol_defn, item$data$definition)) {
            next
        }

        if (!out_calls$has(symbol_defn)) {
            namespace <- attr(symbol_defn, "namespace")
            detail <- if (!is.null(namespace)) sprintf("{%s}", namespace)
            out_calls$set(symbol_defn, list(
                to = list(
                    name = symbol_text[[i]],
                    kind = SymbolKind$Function,
                    uri = symbol_defn$uri,
                    detail = detail,
                    range = symbol_defn$range,
                    selectionRange = symbol_defn$range,
                    data = list(
                        definition = symbol_defn,
                        contextUri = context_uri
                    )
                ),
                fromRanges = list()
            ))
        }

        defn_item <- out_calls$get(symbol_defn)

        defn_item$fromRanges <- c(
            defn_item$fromRanges,
            list(
                range(
                    start = doc$to_lsp_position(
                        row = symbol_line1[[i]] - 1,
                        col = symbol_col1[[i]] - 1
                    ),
                    end = doc$to_lsp_position(
                        row = symbol_line2[[i]] - 1,
                        col = symbol_col2[[i]]
                    )
                )
            )
        )

        out_calls$set(symbol_defn, defn_item)
    }

    result <- out_calls$values()

    logger$info("call_hierarchy_outgoing_calls_reply: ", result)

    Response$new(id, result = result)
}
