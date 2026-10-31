references_xpath <- "//*[(self::SYMBOL or self::SYMBOL_FUNCTION_CALL or self::SYMBOL_FORMALS) and text() = '{token_quote}']"

#' Decode quoted symbol spellings without evaluating document code
#' @noRd
reference_symbol_names <- function(names) {
    quoted <- which(!is.na(names) & startsWith(names, "`"))
    if (!length(quoted)) return(names)
    spellings <- unique(names[quoted])
    decoded <- vapply(spellings, function(name) {
        symbol <- tryCatch(str2lang(name), error = function(e) NULL)
        if (is.symbol(symbol)) as.character(symbol) else name
    }, character(1L), USE.NAMES = FALSE)
    names[quoted] <- decoded[match(names[quoted], spellings)]
    names
}

#' Preserve the complete range and name of a backtick-quoted identifier
#' @noRd
reference_token_at <- function(document, parse_data, point) {
    detected <- document$detect_token(point)
    syntax <- parse_data$syntax_data
    if (!is.null(syntax)) {
        row <- navigation_token(parse_data, point$row + 1L, point$col + 1L)
        if (!row || !syntax$token[[row]] %in%
                c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) return(detected)
        text <- syntax$text[[row]]
        if (!startsWith(text, "`")) return(detected)
        first <- list(row = syntax$line1[[row]] - 1L, col = syntax$col1[[row]] - 1L)
        last <- list(row = syntax$line2[[row]] - 1L, col = syntax$col2[[row]])
    } else {
        xdoc <- parse_data_xml(parse_data)
        if (is.null(xdoc)) return(detected)
        token <- xdoc_find_token(xdoc, point$row + 1L, point$col + 1L)
        if (!xml_name(token) %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) return(detected)
        text <- xml_text(token)
        if (!startsWith(text, "`")) return(detected)
        first <- list(row = as.integer(xml_attr(token, "line1")) - 1L,
            col = as.integer(xml_attr(token, "col1")) - 1L)
        last <- list(row = as.integer(xml_attr(token, "line2")) - 1L,
            col = as.integer(xml_attr(token, "col2")))
    }
    detected$token <- reference_symbol_names(text)
    detected$range <- list(start = first, end = last)
    detected
}

#' Classify bare magrittr stages as calls without changing source positions
#' @noRd
reference_call_tokens <- function(data, syntax = NULL) {
    tokens <- data$token
    pipes <- which(tokens == "SPECIAL" & data$text %in% c("%>%", "%<>%", "%T>%"))
    if (!length(pipes)) return(tokens)
    if (is.null(syntax)) syntax <- syntax_parse_data(data)
    for (pipe in pipes) {
        siblings <- syntax_children(syntax, syntax$parent_row[[pipe]])
        siblings <- siblings[tokens[siblings] != "COMMENT"]
        at <- match(pipe, siblings)
        if (is.na(at) || at == length(siblings)) next
        rhs <- siblings[[at + 1L]]
        if (tokens[[rhs]] != "expr") next
        children <- syntax_children(syntax, rhs)
        children <- children[tokens[children] != "COMMENT"]
        bare <- length(children) == 1L && tokens[children] == "SYMBOL"
        qualified <- length(children) == 3L && tokens[[children[[1L]]]] == "SYMBOL_PACKAGE" &&
            tokens[[children[[2L]]]] %in% c("NS_GET", "NS_GET_INT") && tokens[[children[[3L]]]] == "SYMBOL"
        if (bare || qualified) {
            name <- utils::tail(children, 1L)
            if (data$text[[name]] != ".") tokens[[name]] <- "SYMBOL_FUNCTION_CALL"
        }
    }
    tokens
}

#' Build the symbol occurrence index used by references and call providers
#'
#' The completion parser already records the lexical range of local
#' definitions. Reusing those ranges lets us associate occurrences with a
#' definition once in the background instead of rerunning XPath definition
#' resolution for every occurrence in an interactive request.
#' @noRd
reference_parse_data <- function(data, content, completion_data, uri,
    global_definitions, syntax = NULL) {
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

    call_tokens <- reference_call_tokens(data, syntax)

    rows <- which(data$terminal & data$token %in% c(
        "SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"))
    if (!length(rows)) return(empty)

    # The property is a direct sibling of `$`; the receiver is inside its
    # own expression. Syntax preserves this relationship across whitespace
    # and line breaks.
    member <- data$parent[rows] %in% data$parent[data$token == "'$'"]
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
    definitions$name <- reference_symbol_names(definitions$name)

    global_lines <- vapply(global_definitions, function(definition) {
        as.integer(definition$range$start$line + 1L)
    }, integer(1L))
    is_global <- definitions$name %in% names(global_lines) &
        definitions$line == unname(global_lines[definitions$name])
    local <- which(!is_global)

    names <- reference_symbol_names(data$text[rows])
    definition_keys <- paste0("global:", names)
    definition_kinds <- rep("global", length(rows))
    if (length(local)) {
        local_names <- unique(definitions$name[local])
        selected <- .Call(
            "reference_resolve_local_c",
            PACKAGE = "languageserver",
            match(names, local_names),
            data$line1[rows], data$col1[rows],
            match(definitions$name[local], local_names),
            definitions$line1[local], definitions$col1[local],
            definitions$line2[local], definitions$col2[local]
        )
        local_keys <- paste(
            "local", uri, definitions$name[local],
            definitions$line[local],
            definitions$line1[local], definitions$col1[local],
            definitions$line2[local], definitions$col2[local],
            sep = ":"
        )
        resolved <- which(selected > 0L)
        definition_keys[resolved] <- local_keys[selected[resolved]]
        definition_kinds[resolved] <- definitions$kind[local[selected[resolved]]]
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
    units <- syntax_unit_columns(content, data$line1[rows], cols, end_cols, non_ascii_lines)
    cols <- units$start
    end_cols <- units$end
    multiline <- which(data$line1[rows] != data$line2[rows])
    if (length(multiline)) {
        ending <- data$col2[rows[multiline]]
        end_cols[multiline] <- syntax_unit_columns(content, data$line2[rows[multiline]],
            ending, ending, non_ascii_lines)$end
    }

    package_rows <- which(data$token == "SYMBOL_PACKAGE")
    package_match <- match(data$parent[rows], data$parent[package_rows])
    qualified <- which(!is.na(package_match))
    qualified_call <- !is.na(package_match) & call_tokens[rows] == "SYMBOL_FUNCTION_CALL"
    call_package <- rep("", length(rows))
    call_package[qualified] <- data$text[package_rows[package_match[qualified]]]
    definition_keys[qualified] <- paste(
        "package", call_package[qualified], names[qualified], sep = ":")

    list(
        name = names,
        token = call_tokens[rows],
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
        call_package = call_package,
        by_name = list2env(
            split(seq_along(names), names),
            hash = TRUE, parent = emptyenv()
        )
    )
}

#' Select occurrences without scanning unrelated symbols in the document
#' @noRd
reference_indices <- function(index, name, definition_key = NULL) {
    if (is.null(index) || !length(index$name)) return(integer())
    if (length(name) != 1L || is.na(name) || !nzchar(name)) return(integer())
    selected <- if (is.environment(index$by_name)) {
        get0(name, envir = index$by_name, inherits = FALSE, ifnotfound = integer())
    } else {
        which(index$name == name)
    }
    if (is.null(definition_key)) return(selected)
    selected[index$definition_key[selected] == definition_key]
}

#' Find the indexed definition key at an internal document position
#' @noRd
reference_key_at <- function(index, point, name) {
    selected <- reference_indices(index, name)
    candidates <- selected[
        index$line[selected] == point$row &
            index$code_point_col[selected] <= point$col &
            index$code_point_end_col[selected] >= point$col
    ]
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
references_reply <- function(id, uri, workspace, document, point,
    include_declaration = TRUE) {

    if (!check_r_region(document, point)) {
        return(Response$new(id, result = list()))
    }

    parse_data <- workspace$get_parse_data(uri)
    token <- reference_token_at(document, parse_data, point)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(Response$new(id, result = list()))
    }
    defn <- definition_reply(NULL, uri, workspace, document, point)

    logger$info("references_reply: ", list(
        uri = uri,
        token = token,
        defn = defn$result
    ))

    result <- list()

    if (length(defn$result)) {
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
                selected <- reference_indices(indexed, token$token, definition_key)
                if (!isTRUE(include_declaration)) {
                    selected <- selected[!indexed$is_definition[selected]]
                }
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
            xdoc <- parse_data_xml(workspace$get_parse_data(doc_uri))
            if (is.null(xdoc)) {
                return(list())
            }

            symbols <- xml_find_all(xdoc, "//SYMBOL | //SYMBOL_FUNCTION_CALL | //SYMBOL_FORMALS")
            symbols <- symbols[reference_symbol_names(xml_text(symbols)) == token$token]
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
                if (!isTRUE(include_declaration)) {
                    declaration <- xml_find_all(symbols[[i]], paste(
                        "self::SYMBOL_FORMALS",
                        "self::SYMBOL[parent::forcond]",
                        "self::SYMBOL[parent::expr[count(*)=1][following-sibling::LEFT_ASSIGN or following-sibling::EQ_ASSIGN or preceding-sibling::RIGHT_ASSIGN]]",
                        sep = " | "))
                    if (length(declaration)) next
                }
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
