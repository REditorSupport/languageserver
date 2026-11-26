references_xpath <- "//SYMBOL[text() = '{token_quote}'] | //SYMBOL_FUNCTION_CALL[text() = '{token_quote}'] | //SYMBOL_FORMALS[text() = '{token_quote}']"

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

#' Look up a masked occurrence at the editor cursor
#' @noRd
data_mask_site <- function(parsed, point) {
    mask <- parsed$data_mask
    if (!length(mask$column) && !length(mask$env)) return(NULL)
    row <- member_navigation_token(parsed, point)
    if (row %in% mask$env) return(list(row = row, env = TRUE))
    receiver <- data_mask_receiver(mask, row)
    if (!is.null(receiver)) list(row = row, env = FALSE, receiver = receiver)
}

#' Preserve the complete range and name of a backtick or .env identifier
#' @noRd
reference_token_at <- function(document, parse_data, point, mask_site = data_mask_site(parse_data, point)) {
    detected <- document$detect_token(point)
    syntax <- parse_data$syntax_data
    if (!is.null(syntax)) {
        env <- isTRUE(mask_site$env)
        row <- if (env) mask_site$row else navigation_token(parse_data, point$row + 1L, point$col + 1L,
            prefer_start = c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"))
        if (!row || !syntax$token[[row]] %in%
                c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")) return(detected)
        text <- syntax$text[[row]]
        if (!env && !startsWith(text, "`")) return(detected)
        if (env) {
            detected$accessor <- ""
            detected$package <- NULL
        }
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
    global_definitions, syntax = NULL, mask = NULL) {
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
    if (length(mask$env)) member <- member & !rows %in% mask$env
    if (length(mask$column)) member <- member | rows %in% mask$column
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

    names <- reference_symbol_names(data$text[rows])
    definition_positions <- paste(
        definitions$name, definitions$definition_line1, definitions$definition_col1,
        definitions$definition_line2, definitions$definition_col2, sep = ":"
    )
    occurrence_positions <- paste(
        names, data$line1[rows], data$col1[rows], data$line2[rows], data$col2[rows], sep = ":"
    )
    definition_at <- match(definition_positions, occurrence_positions, nomatch = 0L)
    is_definition <- logical(length(rows))
    is_definition[definition_at] <- TRUE

    global_lines <- vapply(global_definitions, function(definition) {
        as.integer(definition$range$start$line + 1L)
    }, integer(1L))
    global <- match(definitions$name, names(global_lines))
    is_global <- !is.na(global) & definitions$line == unname(global_lines[global])
    if (!is.null(syntax$scope_row)) {
        found <- which(definition_at > 0L)
        tokens <- rows[definition_at[found]]
        outer <- syntax$function_scope[tokens] == 0L
        # The workspace retains only the last definition of each name. Earlier
        # assignments and loop variables in bare braces bind the same name.
        parents <- syntax$parent_row[tokens]
        statement <- syntax$parent_row[parents]
        direct <- statement == syntax$scope_row[tokens] + 1L & outer
        known <- !is.na(global[found]) | syntax$token[parents] == "forcond"
        # Keep declarations recognized by the document parser in control flow.
        # On a shared line the name must actually fall inside that definition.
        check <- which(outer & !direct & is_global[found])
        is_global[found] <- direct & known
        for (i in check) {
            token <- tokens[[i]]
            line <- data$line1[[token]]
            def <- global_definitions[[global[[found[[i]]]]]]$range
            col <- code_point_to_unit(content[[line]], data$col1[[token]] - 1L)
            is_global[found[[i]]] <- col >= def$start$character &&
                (line - 1L < def$end$line || col < def$end$character)
        }
    }
    local <- which(!is_global)

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

#' Find the indexed occurrence at an internal document position
#' @noRd
reference_index_at <- function(index, point, name) {
    selected <- reference_indices(index, name)
    candidates <- selected[
        index$line[selected] == point$row &
            index$code_point_col[selected] <= point$col &
            index$code_point_end_col[selected] >= point$col
    ]
    if (length(candidates)) candidates[[1L]] else 0L
}

reference_key_at <- function(index, point, name) {
    row <- reference_index_at(index, point, name)
    if (row) index$definition_key[[row]]
}

# Local edits can use a whole expression whose original enclosing scope was
# parsed, even when a different expression in the file is incomplete.
partial_root_contains <- function(parsed, start, end = start) {
    if (!isTRUE(parsed$parse_error) && !length(parsed$partial_incomplete_lines)) return(TRUE)
    roots <- parsed$partial_complete_roots
    syntax <- parsed$syntax_data
    starts_before <- syntax$line1[roots] < start$row + 1L |
        (syntax$line1[roots] == start$row + 1L & syntax$col1[roots] <= start$col + 1L)
    ends_after <- syntax$line2[roots] > end$row + 1L |
        (syntax$line2[roots] == end$row + 1L & syntax$col2[roots] >= end$col)
    any(starts_before & ends_after)
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
    include_declaration = TRUE, for_rename = FALSE, new_name = NULL, glue_site = TRUE) {

    if (!check_r_region(document, point)) {
        return(Response$new(id, result = list()))
    }

    if (glue_site && !for_rename && length(document$parse_data$glue_templates)) {
        embedded <- glue_reference_site(workspace, document, point)
        if (!is.null(embedded)) return(Response$new(id, result = glue_site_locations(workspace, document,
                    embedded, include_declaration)))
    }
    mask <- data_mask_site(document$parse_data, point)
    if (!is.null(mask) && !mask$env) {
        if (for_rename) return(data_column_rename_error(id))
        return(Response$new(id, result = list()))
    }
    member <- if (is.null(mask)) member_navigation_at(workspace, document, point)
    if (!is.null(member)) {
        reply <- member_navigation_references(id, workspace, document, member, include_declaration, for_rename, new_name)
        if (!for_rename && !isTRUE(member$formal) && length(member$definition$range)) {
            uris <- member_navigation_uris(workspace, member$definition, uri)
            reply$result <- glue_member_locations(workspace, member$definition, member$name, uris, reply$result, include_declaration)
        }
        return(reply)
    }
    site <- s4_reference_site(document$parse_data, point)
    sources <- if (!is.null(site)) s4_reference_sources(workspace, site$name)
    s4 <- s4_reference_target(workspace, document, point, include_binding = TRUE, site = site, candidates = sources)
    if (!is.null(s4$binding) && for_rename && s4_reference_promotes_binding(workspace, s4)) return(s4_reference_rename_error(id))
    if (!is.null(s4) && is.null(s4$binding)) {
        if (for_rename) return(s4_reference_rename_error(id))
        return(Response$new(id, result = s4_reference_locations(workspace, s4, include_declaration)))
    }
    parse_data <- workspace$get_parse_data(uri)
    token <- reference_token_at(document, parse_data, point, mask)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(Response$new(id, result = list()))
    }
    defn <- symbol_definition_reply(NULL, uri, workspace, document, point, mask = mask)
    if (for_rename && !member_navigation_writable(workspace, defn$result)) {
        return(Response$new(id, result = list()))
    }

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
        scope_key <- if (is.null(s4$binding)) definition_key else paste0("global:", token$token)
        doc_uris <- workspace_reference_document_uris(workspace, defn$result$uri, uri, scope_key)
        if (!is.null(definition_key)) {
            check_capture <- for_rename && !is.null(new_name) && !identical(new_name, token$token)
            owner_scope <- if (check_capture) rename_target_scope(workspace$get_parse_data(defn$result$uri), token$token, scope_key)
            indexed_new <- check_capture && identical(owner_scope, 0L) && isTRUE(workspace$index$enabled) &&
                is.function(workspace$index$has_definition) && workspace$index$has_definition(new_name)
            global <- startsWith(definition_key, "global:")
            if (global && !identical(site$name, token$token)) sources <- s4_reference_sources(workspace, token$token)
            for (doc_uri in doc_uris) {
                if (for_rename) {
                    doc <- workspace$documents$get(doc_uri, NULL)
                    problem <- rename_lexical_document_error(id, doc, uri, token, scope_key)
                    if (!is.null(problem)) return(problem)
                    indexed <- doc$parse_data$reference_index
                } else {
                    indexed <- workspace$get_parse_data(doc_uri)$reference_index
                }
                if (is.null(indexed)) next
                selected <- reference_indices(indexed, token$token, definition_key)
                if (!is.null(s4$binding)) {
                    selected <- reference_indices(indexed, token$token)
                    selected <- s4_reference_lexical_indices(workspace, workspace$documents$get(doc_uri), selected, token$token, s4)
                } else if (global && !is.null(sources) && length(selected)) {
                    doc <- workspace$documents$get(doc_uri)
                    selected <- s4_reference_lexical_indices(workspace, doc, selected, token$token, candidates = sources)
                }
                if (check_capture && rename_lexical_conflict(workspace, workspace$documents$get(doc_uri),
                        selected, new_name, defn$result$uri, owner_scope, indexed_new)) {
                    return(ResponseErrorMessage$new(id, "RequestFailed", "The new name would change an existing lexical binding"))
                }
                if (!isTRUE(include_declaration)) {
                    selected <- selected[!indexed$is_definition[selected]]
                }
                locations <- indexed_reference_locations(indexed, doc_uri, selected)
                doc <- workspace$documents$get(doc_uri, NULL)
                fragments <- doc$parse_data$glue_templates
                if (length(fragments) || length(attr(fragments, "unmapped", exact = TRUE))) {
                    occurrences <- glue_reference_occurrences(workspace, doc, token$token)
                    if (for_rename) {
                        problem <- glue_rename_problem(workspace, doc, token$token, definition_key, new_name, sites = occurrences)
                        if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
                    }
                    locations <- glue_overlay_locations(workspace, doc, token$token, definition_key, locations,
                        include_declaration, occurrences)
                }
                result <- c(result, locations)
            }
            formal <- if (startsWith(definition_key, "local:")) {
                member_navigation_formal_owner(parse_data, uri, token$token, definition_key)
            }
            if (!is.null(formal)) {
                if (for_rename && !is.null(new_name) && !identical(new_name, token$token)) {
                    formal_row <- member_navigation_token(parse_data, document$from_lsp_position(formal$range$start))
                    syntax <- parse_data$syntax_data
                    siblings <- syntax_children(syntax, syntax$parent_row[[formal_row]])
                    names <- reference_symbol_names(syntax$text[siblings[syntax$token[siblings] == "SYMBOL_FORMALS"]])
                    if (new_name %in% names) return(ResponseErrorMessage$new(id, "RequestFailed",
                            "Another parameter of this function already has the new name"))
                }
                scope <- reference_formal_call_scope(parse_data, document, formal)
                uris <- if (identical(scope, "workspace")) {
                    member_navigation_uris(workspace, formal, uri)
                } else if (identical(scope, "document")) {
                    uri
                } else {
                    character()
                }
                if (for_rename) {
                    for (doc_uri in setdiff(uris, uri)) {
                        if (!rename_document_complete(workspace$documents$get(doc_uri), uri, token, "formal")) {
                            return(rename_incomplete_error(id))
                        }
                    }
                }
                if (for_rename && !is.null(new_name) && !identical(new_name, token$token) &&
                        member_navigation_formal_conflict(workspace, uris, formal, token$token, new_name)) {
                    return(ResponseErrorMessage$new(id, "RequestFailed", "A named argument would match a different parameter"))
                }
                if (for_rename) for (doc_uri in union(uris, uri)) {
                    doc <- workspace$documents$get(doc_uri, NULL)
                    if (length(doc$parse_data$data_mask$column) &&
                            member_masked_formal_conflict(doc, document, formal, token$token, new_name)) {
                        return(ResponseErrorMessage$new(id, "RequestFailed", "A named argument in a data mask may refer to this parameter"))
                    }
                    problem <- glue_rename_formal_problem(workspace, doc, formal, token$token, new_name)
                    if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
                }
                tags <- member_navigation_occurrences(workspace, formal, token$token, uris,
                    formal = TRUE, declarations = FALSE)
                if (!for_rename) tags <- lapply(tags, function(x) location(x$uri, x$range))
                result <- c(result, tags)
                for (doc_uri in setdiff(uris, doc_uris)) {
                    doc <- workspace$documents$get(doc_uri, NULL)
                    if (length(doc$parse_data$glue_templates)) result <- c(result, glue_overlay_locations(
                        workspace, doc, token$token, definition_key, list(), include_declaration))
                }
                if (for_rename) attr(result, "rename_formal") <- formal
            }
            return(Response$new(id, result = result))
        }

        if (for_rename) {
            for (doc_uri in doc_uris) {
                doc <- workspace$documents$get(doc_uri, NULL)
                problem <- rename_lexical_document_error(id, doc, uri, token, NULL, new_name)
                if (!is.null(problem)) return(problem)
            }
        }
        doc_results <- lapply(doc_uris, function(doc_uri) {
            doc <- workspace$documents$get(doc_uri)
            parsed <- workspace$get_parse_data(doc_uri)
            xdoc <- parse_data_xml(parsed)
            if (is.null(xdoc)) {
                return(list())
            }

            symbols <- xml_find_all_(xdoc, "//SYMBOL | //SYMBOL_FUNCTION_CALL | //SYMBOL_FORMALS")
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
                    is_declaration <- xml_name(symbols[[i]]) == "SYMBOL_FORMALS" || (
                        xml_name(symbols[[i]]) == "SYMBOL" && xml_find_lgl_(symbols[[i]],
                            "boolean(parent::forcond or parent::expr[count(*)=1 and (following-sibling::LEFT_ASSIGN or following-sibling::EQ_ASSIGN or preceding-sibling::RIGHT_ASSIGN)])")
                    )
                    if (is_declaration) next
                }
                symbol_point <- list(row = line1[[i]] - 1, col = col1[[i]])
                mask <- data_mask_site(parsed, symbol_point)
                if (!isTRUE(mask$env) && xml_find_lgl_(symbols[[i]], "boolean(preceding-sibling::OP-DOLLAR)")) next
                symbol_defn <- symbol_definition_reply(
                    NULL, doc_uri, workspace, doc, symbol_point,
                    context_uri = uri, mask = mask)
                if (identical(symbol_defn$result, defn$result) &&
                        is.null(s4_reference_target(workspace, doc, symbol_point))) {
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
            # XML-era indexes do not record a formal's reference key. The
            # lexical definition still identifies its exact source location.
            if (for_rename && length(result)) attr(result, "rename_formal") <- defn$result
        }
    }

    logger$info("references_reply: ", result)

    Response$new(
        id,
        result = result
    )
}

reference_source_scope <- function(workspace, uri, callers = FALSE) {
    index <- workspace$index
    if (isTRUE(index$enabled) && index$contains_path(path_from_uri(uri))) {
        root <- index$package_root_for_uri(uri)
        if (!is.null(root)) return(union(uri, index$package_source_uris(root)))
        if (callers) return(index$dependent_closure(uri))
        return(index$source_closure(uri))
    }
    if (callers) workspace_reference_document_uris(workspace, uri, uri) else workspace_document_uris(workspace, uri)
}

# Named arguments can reference an assigned function or a named function
# member. An anonymous callback has no source callee for tag matching.
reference_formal_call_scope <- function(parsed, document, definition) {
    row <- member_navigation_token(parsed, document$from_lsp_position(definition$range$start))
    if (!row) return(NULL)
    syntax <- parsed$syntax_data
    fn <- syntax$parent_row[[row]]
    declaration <- rename_function_declaration(syntax, row)
    if (!declaration || declaration == fn) {
        value <- fn
        repeat {
            parent <- syntax$parent_row[[value]]
            if (!parent) return(NULL)
            children <- syntax_children(syntax, parent)
            children <- children[syntax$token[children] != "COMMENT"]
            if (length(children) == 3L && children[[2L]] == value &&
                    identical(syntax$token[children[c(1L, 3L)]], c("'('", "')'"))) {
                value <- parent
                next
            }
            pos <- match(value, children)
            if (is.na(pos) || pos < 4L || syntax$token[[children[[pos - 1L]]]] != "EQ_SUB") return(NULL)
            head <- syntax_children(syntax, children[[1L]])
            if (!length(head) || !reference_symbol_names(syntax$text[utils::tail(head, 1L)]) %in% c("list", "list2", "data.frame")) {
                return(NULL)
            }
            # A factory can return this named field to another document.
            return("workspace")
        }
    }
    assigned <- syntax_children(syntax, declaration)
    assigned <- assigned[syntax$token[assigned] != "COMMENT"]
    if (length(assigned) != 3L || syntax$text[[assigned[[2L]]]] %in% c("<<-", "->>")) return("workspace")
    lhs <- if (syntax$token[[assigned[[2L]]]] == "RIGHT_ASSIGN") assigned[[3L]] else assigned[[1L]]
    binding <- syntax_children(syntax, lhs)
    if (length(binding) != 1L || syntax$token[[binding]] != "SYMBOL") return("workspace")
    parent <- syntax$parent_row[[fn]]
    while (parent) {
        if (syntax$function_close[[parent]] > 0L) return("document")
        parent <- syntax$parent_row[[parent]]
    }
    "workspace"
}

# A standalone string has one expression wrapper. Only double-bracket keys
# and slot/prop arguments can be member uses in that position. Quoted tags
# and direct $ or @ names are unwrapped children and need full classification.
reference_member_literal_candidate <- function(syntax, row) {
    parent <- syntax$parent_row[[row]]
    if (!parent) return(FALSE)
    if (syntax$child_start[[parent]] != syntax$child_end[[parent]]) return(TRUE)
    call <- syntax$parent_row[[parent]]
    if (!call) return(FALSE)
    start <- syntax$child_start[[call]]
    if (start >= syntax$child_end[[call]]) return(FALSE)
    first <- syntax$children[[start]]
    second <- syntax$children[[start + 1L]]
    # Let the full parser handle unusual comments between the callee and opener.
    if ("COMMENT" %in% syntax$token[c(first, second)]) return(TRUE)
    if (first == parent) return(FALSE)
    if (syntax$token[[second]] == "LBB") return(TRUE)
    if (syntax$token[[second]] != "'('" || syntax$child_end[[first]] < syntax$child_start[[first]]) return(FALSE)
    callee <- syntax$children[[syntax$child_end[[first]]]]
    if (syntax$token[[callee]] == "COMMENT") return(TRUE)
    syntax$token[[callee]] == "SYMBOL_FUNCTION_CALL" && reference_symbol_names(syntax$text[[callee]]) %in%
        c(names(member_key_formals), "new_property", "deprecated_property")
}

reference_literal_needs_workspace <- function(document, point) {
    parsed <- document$parse_data
    if (length(parsed$glue_templates) && !is.null(glue_fragment_at(parsed, point))) return(TRUE)
    row <- member_navigation_token(parsed, point)
    if (!row || parsed$syntax_data$token[[row]] != "STR_CONST") return(FALSE)
    if (!is.null(s4_definition_literal(parsed, row))) return(TRUE)
    reference_member_literal_candidate(parsed$syntax_data, row) && row %in% member_navigation_syntax(parsed)$row
}

reference_request_documents <- function(workspace, doc, point) {
    embedded <- glue_reference_site(workspace, doc, point)
    if (!is.null(embedded)) {
        if (length(embedded$key) && !startsWith(embedded$key, "global:") &&
                !isTRUE(embedded$target$formal) && !identical(embedded$target$kind, "field")) return(doc$uri)
        target <- embedded$target$uri
        return(unique(c(reference_source_scope(workspace, doc$uri),
                    if (length(target)) reference_source_scope(workspace, target, callers = TRUE))))
    }
    mask <- data_mask_site(doc$parse_data, point)
    if (!is.null(mask) && !mask$env) return(doc$uri)
    member <- if (is.null(mask)) member_navigation_at(workspace, doc, point)
    s4 <- if (is.null(member)) s4_reference_target(workspace, doc, point)
    definition <- if (!is.null(member)) {
        member$definition$uri
    } else if (!is.null(s4)) {
        s4$anchor
    } else {
        symbol_definition_reply(NULL, doc$uri, workspace, doc, point, mask = mask)$result$uri
    }
    # A member can originate in an unopened source() dependency. Parse these
    # first; the request manager asks again after their source edges settle.
    scope <- if (!is.null(member) || !is.null(s4)) reference_source_scope(workspace, doc$uri)
    callers <- if (!is.null(s4)) {
        s4_reference_files(workspace, s4)
    } else if (!is.null(definition)) {
        reference_source_scope(workspace, definition, callers = TRUE)
    }
    unique(c(scope, callers))
}
