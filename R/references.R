references_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and text() = '{token_quote}']"

#' Build the symbol occurrence index used by references and call providers
#'
#' The completion parser already records the lexical range of local
#' definitions. Reusing those ranges lets us associate occurrences with a
#' definition once in the background instead of rerunning XPath definition
#' resolution for every occurrence in an interactive request.
#' @noRd
reference_parse_data <- function(data, content, completion_data, uri,
    global_definitions) {
    empty <- list(
        name = character(),
        token = character(),
        line = integer(),
        col = integer(),
        end_line = integer(),
        end_col = integer(),
        code_point_col = integer(),
        code_point_end_col = integer(),
        definition_key = character(),
        is_definition = logical(),
        definition_kind = character(),
        qualified_call = logical(),
        call_package = character()
    )
    if (is.null(data) || !nrow(data)) return(empty)

    rows <- which(data$terminal & data$token %in% c(
        "SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"))
    if (!length(rows)) return(empty)

    # Member names following `$` are properties, not lexical symbols.
    row_lines <- content[data$line1[rows]]
    member <- data$col1[rows] > 1L &
        stringi::stri_sub(
            row_lines,
            from = pmax(data$col1[rows] - 1L, 1L),
            to = pmax(data$col1[rows] - 1L, 1L)
        ) == "$"
    rows <- rows[!member]
    if (!length(rows)) return(empty)

    combine_records <- function(records, kinds) {
        list(
            name = unlist(lapply(records, `[[`, "name"), use.names = FALSE),
            line = unlist(lapply(records, `[[`, "line"), use.names = FALSE),
            definition_line1 = unlist(lapply(
                records, `[[`, "definition_line1"), use.names = FALSE),
            definition_col1 = unlist(lapply(
                records, `[[`, "definition_col1"), use.names = FALSE),
            definition_line2 = unlist(lapply(
                records, `[[`, "definition_line2"), use.names = FALSE),
            definition_col2 = unlist(lapply(
                records, `[[`, "definition_col2"), use.names = FALSE),
            line1 = unlist(lapply(records, `[[`, "line1"), use.names = FALSE),
            col1 = unlist(lapply(records, `[[`, "col1"), use.names = FALSE),
            line2 = unlist(lapply(records, `[[`, "line2"), use.names = FALSE),
            col2 = unlist(lapply(records, `[[`, "col2"), use.names = FALSE),
            kind = rep(kinds, lengths(lapply(records, `[[`, "name")))
        )
    }
    definitions <- combine_records(
        list(
            completion_data$symbols,
            completion_data$functions,
            completion_data$formals
        ),
        c("variable", "function", "formal")
    )

    global_lines <- vapply(global_definitions, function(definition) {
        as.integer(definition$range$start$line + 1L)
    }, integer(1L))
    is_global <- definitions$name %in% names(global_lines) &
        definitions$line == unname(global_lines[definitions$name])
    local <- which(!is_global)

    names <- data$text[rows]
    definition_keys <- paste0("global:", names)
    definition_kinds <- rep("global", length(rows))
    local_names <- unique(definitions$name[local])
    for (name in intersect(unique(names), local_names)) {
        occurrence_indices <- which(names == name)
        definition_indices <- local[definitions$name[local] == name]
        if (!length(definition_indices)) next

        start_positions <- definitions$line1[definition_indices] * 1000000 +
            definitions$col1[definition_indices]
        end_positions <- definitions$line2[definition_indices] * 1000000 +
            definitions$col2[definition_indices]
        spans <- end_positions - start_positions
        definition_order <- order(start_positions, -spans)
        definition_indices <- definition_indices[definition_order]
        start_positions <- start_positions[definition_order]
        end_positions <- end_positions[definition_order]

        occurrence_rows <- rows[occurrence_indices]
        occurrence_positions <- data$line1[occurrence_rows] * 1000000 +
            data$col1[occurrence_rows]
        nearest <- findInterval(occurrence_positions, start_positions)

        for (k in seq_along(occurrence_indices)) {
            candidate_position <- nearest[[k]]
            # Local definition ranges are normally nested or disjoint. Walk
            # back only when the nearest preceding definition has already
            # ended, which keeps this linear for ordinary documents.
            while (candidate_position > 0L &&
                    end_positions[[candidate_position]] <
                        occurrence_positions[[k]]) {
                candidate_position <- candidate_position - 1L
            }
            if (candidate_position == 0L) next
            candidate <- definition_indices[[candidate_position]]
            occurrence_index <- occurrence_indices[[k]]
            definition_keys[[occurrence_index]] <- paste(
                "local", uri, name,
                definitions$line[[candidate]],
                definitions$line1[[candidate]],
                definitions$col1[[candidate]],
                definitions$line2[[candidate]],
                definitions$col2[[candidate]],
                sep = ":"
            )
            definition_kinds[[occurrence_index]] <-
                definitions$kind[[candidate]]
        }
    }

    definition_positions <- paste(
        definitions$name,
        definitions$definition_line1,
        definitions$definition_col1,
        definitions$definition_line2,
        definitions$definition_col2,
        sep = ":"
    )
    occurrence_positions <- paste(
        names,
        data$line1[rows],
        data$col1[rows],
        data$line2[rows],
        data$col2[rows],
        sep = ":"
    )
    is_definition <- occurrence_positions %in% definition_positions

    cols <- as.integer(data$col1[rows] - 1L)
    end_cols <- as.integer(data$col2[rows])
    non_ascii_lines <- nchar(content, type = "bytes") !=
        nchar(content, type = "chars")
    convert <- which(non_ascii_lines[data$line1[rows]])
    for (i in convert) {
        row_index <- rows[[i]]
        converted <- code_point_to_unit(
            content[[data$line1[[row_index]]]],
            c(data$col1[[row_index]] - 1L, data$col2[[row_index]])
        )
        cols[[i]] <- converted[[1L]]
        end_cols[[i]] <- converted[[2L]]
    }

    qualified_call <- data$token[rows] == "SYMBOL_FUNCTION_CALL" &
        data$col1[rows] > 2L &
        stringi::stri_sub(
            content[data$line1[rows]],
            from = pmax(data$col1[rows] - 2L, 1L),
            to = pmax(data$col1[rows] - 1L, 1L)
        ) == "::"
    call_package <- rep("", length(rows))
    for (i in which(qualified_call)) {
        row_index <- rows[[i]]
        token <- scan_token(
            content[[data$line1[[row_index]]]],
            data$col1[[row_index]] - 1L,
            forward = TRUE
        )
        package <- token$package
        if (!is.null(package) && nzchar(package)) {
            call_package[[i]] <- package
            definition_keys[[i]] <- paste(
                "package", package, names[[i]], sep = ":")
        }
    }

    list(
        name = names,
        token = data$token[rows],
        line = as.integer(data$line1[rows] - 1L),
        col = cols,
        end_line = as.integer(data$line2[rows] - 1L),
        end_col = end_cols,
        code_point_col = as.integer(data$col1[rows] - 1L),
        code_point_end_col = as.integer(data$col2[rows]),
        definition_key = definition_keys,
        is_definition = is_definition,
        definition_kind = definition_kinds,
        qualified_call = qualified_call,
        call_package = call_package
    )
}

#' Find the indexed definition key at an internal document position
#' @noRd
reference_key_at <- function(index, point, name) {
    if (is.null(index) || !length(index$name)) return(NULL)
    candidates <- which(
        index$name == name &
            index$line == point$row &
            index$code_point_col <= point$col &
            index$code_point_end_col >= point$col
    )
    if (!length(candidates)) return(NULL)
    index$definition_key[[candidates[[1L]]]]
}

#' Create reference locations from an indexed selection
#' @noRd
indexed_reference_locations <- function(index, uri, selected) {
    lapply(selected, function(i) {
        location(
            uri,
            range(
                position(index$line[[i]], index$col[[i]]),
                position(index$end_line[[i]], index$end_col[[i]])
            )
        )
    })
}

#' @noRd
references_reply <- function(id, uri, workspace, document, point) {

    if (!check_r_region(document, point)) {
        return(Response$new(id, result = list()))
    }

    token <- document$detect_token(point)
    defn <- definition_reply(NULL, uri, workspace, document, point)
    token_quote <- xml_single_quote(token$token)

    logger$info("references_reply: ", list(
        uri = uri,
        token = token,
        defn = defn$result
    ))

    result <- list()

    if (length(defn$result)) {
        parse_data <- workspace$get_parse_data(uri)
        token_point <- list(
            row = token$range$start$row,
            col = token$range$start$col
        )
        definition_key <- reference_key_at(
            parse_data$reference_index, token_point, token$token)
        if (!is.null(definition_key)) {
            doc_uris <- workspace_reference_document_uris(
                workspace, defn$result$uri, uri)
            for (doc_uri in doc_uris) {
                indexed <- workspace$get_parse_data(doc_uri)$reference_index
                if (is.null(indexed)) next
                selected <- which(
                    indexed$name == token$token &
                        indexed$definition_key == definition_key
                )
                if (length(selected)) {
                    result <- c(
                        result,
                        indexed_reference_locations(indexed, doc_uri, selected)
                    )
                }
            }
            return(Response$new(id, result = result))
        }

        doc_uris <- workspace_reference_document_uris(
            workspace, defn$result$uri, uri)
        doc_results <- lapply(doc_uris, function(doc_uri) {
            doc <- workspace$documents$get(doc_uri)
            xdoc <- workspace$get_parse_data(doc_uri)$xml_doc
            if (is.null(xdoc)) {
                return(list())
            }

            symbols <- xml_find_all(xdoc, glue(references_xpath, token_quote = token_quote))
            if (length(symbols) == 0) {
                return(list())
            }

            line1 <- as.integer(xml_attr(symbols, "line1"))
            col1 <- as.integer(xml_attr(symbols, "col1"))
            line2 <- as.integer(xml_attr(symbols, "line2"))
            col2 <- as.integer(xml_attr(symbols, "col2"))

            matches <- vector("list", length(symbols))
            idx <- 0L
            for (i in seq_along(symbols)) {
                symbol_point <- list(row = line1[[i]] - 1, col = col1[[i]])
                symbol_defn <- definition_reply(
                    NULL, doc_uri, workspace, doc, symbol_point,
                    context_uri = uri)
                if (identical(symbol_defn$result, defn$result)) {
                    idx <- idx + 1L
                    matches[[idx]] <- list(
                        uri = doc_uri,
                        range = range(
                            start = doc$to_lsp_position(
                                row = line1[[i]] - 1,
                                col = col1[[i]] - 1
                            ),
                            end = doc$to_lsp_position(
                                row = line2[[i]] - 1,
                                col = col2[[i]]
                            )
                        )
                    )
                }
            }

            if (idx == 0L) {
                return(list())
            }
            if (idx < length(matches)) {
                matches <- matches[seq_len(idx)]
            }
            matches
        })

        if (length(doc_results)) {
            result <- do.call(c, doc_results)
        }
    }

    logger$info("references_reply: ", result)

    Response$new(
        id,
        result = result
    )
}
