rename_document_complete <- function(doc, uri, token, definition_key) {
    data <- doc$parse_data
    if (is.null(data) || !identical(doc$content, doc$parsed_content) ||
            (!is.null(data$version) && !identical(doc$version, data$version))) return(FALSE)
    missing <- data$partial_incomplete_lines
    if (!length(missing)) return(!isTRUE(data$parse_error) || !is.null(missing))
    if (!is.null(definition_key) && startsWith(definition_key, "local:") &&
            partial_root_contains(data, token$range$start, token$range$end)) return(TRUE)
    if (identical(doc$uri, uri) && is.null(definition_key)) return(FALSE)
    # A bare name may be referenced in unindexed code, including through
    # an escaped backtick spelling. Never return just the indexed edits.
    lines <- doc$content[missing + 1L]
    name <- token$token
    if (any(grepl("\\", lines, fixed = TRUE)) || grepl("\n", name, fixed = TRUE)) return(FALSE)
    matches <- lines[grepl(name, lines, fixed = TRUE)]
    if (!length(matches)) return(TRUE)
    if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) return(FALSE)
    # An ASCII identifier cannot be a separate token inside a longer name
    # such as the 'a' in stats::. Keep ambiguous numeric/placeholder runs:
    # e.g. R can tokenize 1Lname as an integer followed by a symbol.
    words <- unlist(regmatches(matches, gregexpr("[A-Za-z0-9._]+", matches)), use.names = FALSE)
    ambiguous <- grepl("^([0-9_]|\\.[0-9])", words)
    !any(words == name) && !any(grepl(name, words[ambiguous], fixed = TRUE))
}

# The enclosing function is the lexical scope for assignments, formals and
# loop variables. Row zero denotes the document/workspace's outer bindings.
rename_binding_scopes <- function(syntax, rows) {
    scopes <- syntax$parent_row[rows]
    repeat {
        pending <- which(scopes > 0L)
        pending <- pending[syntax$function_close[scopes[pending]] == 0L]
        if (!length(pending)) return(scopes)
        scopes[pending] <- syntax$parent_row[scopes[pending]]
    }
}

rename_target_scope <- function(parsed, name, key) {
    if (startsWith(key, "global:")) return(0L)
    index <- parsed$reference_index
    sites <- reference_indices(index, name, key)
    declaration <- sites[index$is_definition[sites]]
    if (!length(declaration)) return(NULL)
    i <- declaration[[1L]]
    row <- member_navigation_token(parsed, list(row = index$line[[i]], col = index$code_point_col[[i]]))
    if (!row) return(NULL)
    rename_binding_scopes(parsed$syntax_data, row)
}

# Detect collisions in both directions. A changed use must not resolve to an
# inner binding, and existing free uses of the new name must not become bound
# by the renamed declaration. Only the two indexed spellings are inspected.
rename_lexical_conflict <- function(workspace, doc, selected, new, owner_uri, owner_scope, indexed_new) {
    if (is.null(owner_scope) || (owner_scope && !identical(doc$uri, owner_uri))) return(FALSE)
    parsed <- doc$parse_data
    index <- parsed$reference_index
    if (indexed_new && workspace$index$has_definition(new, uri = doc$uri)) return(TRUE)
    existing <- reference_indices(index, new)
    existing <- existing[!startsWith(index$definition_key[existing], "package:")]
    uncertain <- data_mask_uncertain_rows(parsed$data_mask, new)
    captured <- data_mask_capture_rows(parsed$data_mask, new)
    if (!length(existing) && !length(uncertain) && !length(captured)) return(FALSE)
    syntax <- parsed$syntax_data
    stride <- as.double(syntax$maxcol) + 1
    at <- function(i) as.double(index$line[i] + 1L) * stride + index$code_point_col[i] + 1L
    start <- function(rows) as.double(syntax$line1[rows]) * stride + syntax$col1[rows]
    end <- function(rows) as.double(syntax$line2[rows]) * stride + syntax$col2[rows]
    if (length(captured) && any(start(captured) %in% at(selected))) return(TRUE)
    if (!length(existing) && !length(uncertain)) return(FALSE)
    uncertain <- start(uncertain)
    if (owner_scope) {
        position <- at(existing)
        existing <- existing[position >= start(owner_scope) & position <= end(owner_scope)]
        uncertain <- uncertain[uncertain >= start(owner_scope) & uncertain <= end(owner_scope)]
        if (!length(existing) && !length(uncertain)) return(FALSE)
    }

    defs <- existing[index$is_definition[existing]]
    if (!length(defs)) return(TRUE)
    rows <- vapply(defs, function(i) {
        member_navigation_token(parsed, list(row = index$line[[i]], col = index$code_point_col[[i]]))
    }, integer(1L))
    if (any(!rows)) return(TRUE)
    scopes <- rename_binding_scopes(syntax, rows)
    if (any(scopes == owner_scope)) return(TRUE)

    # The remaining declarations are separate bindings in enclosed functions.
    # Merging their intervals avoids comparing every edit with every scope.
    covered <- function(points, first, last) {
        order <- order(first)
        found <- findInterval(points, first[order])
        points <= c(-Inf, cummax(last[order]))[found + 1L]
    }
    if (any(covered(at(selected), start(scopes), end(scopes)))) return(TRUE)
    reads <- existing[!index$is_definition[existing]]
    if (!length(reads) && !length(uncertain)) return(FALSE)
    first <- at(defs)
    formals <- index$token[defs] == "SYMBOL_FORMALS"
    first[formals] <- start(scopes[formals])
    # A read in the RHS of x <- x still reaches an outer binding. A function
    # literal can refer recursively to the name being assigned.
    nav <- parsed$navigation_data
    candidates <- get0(new, envir = nav$by_name, inherits = FALSE, ifnotfound = integer())
    assignments <- candidates[match(rows, nav$name[candidates])]
    values <- which(!is.na(assignments) & nav[["function"]][assignments] == 0L)
    first[values] <- end(nav$node[assignments[values]]) + 1
    loop <- which(syntax$token[syntax$parent_row[rows]] == "forcond")
    first[loop] <- end(syntax$parent_row[rows[loop]])

    any(!covered(c(at(reads), uncertain), first, end(scopes)))
}

rename_incomplete_error <- function(id) {
    ResponseErrorMessage$new(id, "RequestFailed", "Incomplete R code may contain other uses of this symbol")
}

data_column_rename_error <- function(id) {
    ResponseErrorMessage$new(id, "RequestFailed", "Cannot safely rename a data-masked column")
}

# A data.frame field can also be selected through its constructor tag or $.
# Protect those entry points when the member edit would miss a masked read.
data_column_member_conflict <- function(workspace, owner, target, row, name, uris) {
    syntax <- owner$parse_data$syntax_data
    if (!row || !syntax$token[[row]] %in% c("SYMBOL_SUB", "STR_CONST")) return(FALSE)
    ctor <- syntax_call_arguments(syntax, syntax$parent_row[[row]])
    if (!identical(ctor$name, "data.frame")) return(FALSE)
    for (uri in uris) {
        doc <- workspace$documents$get(uri)
        parsed <- doc$parse_data
        mask <- parsed$data_mask
        if (!length(mask$column)) next
        s <- parsed$syntax_data
        candidates <- which(reference_symbol_names(s$text[mask$column]) == name)
        if (!length(candidates)) next
        if (any(mask$receiver[candidates] == 0L)) return(TRUE)
        candidates <- candidates[!duplicated(mask$receiver[candidates])]
        candidates <- candidates[order(s$order_rank[mask$receiver[candidates]], decreasing = TRUE)]
        bracket <- mask$column[candidates] %in% mask$bracket
        receivers <- mask$receiver[candidates]
        symbols <- s$children[s$child_start[receivers]]
        symbols[s$child_start[receivers] != s$child_end[receivers] | s$token[symbols] != "SYMBOL"] <- 0L
        if (any(symbols %in% mask$column)) return(TRUE)
        covered <- new.env(hash = TRUE, parent = emptyenv())
        for (j in seq_along(candidates)) {
            i <- candidates[[j]]
            site <- list(row = mask$column[[i]], receiver = mask$receiver[[i]])
            symbol <- symbols[[j]]
            if (symbol) {
                key <- paste0(as.integer(bracket[[j]]), s$text[[symbol]])
                start <- covered[[key]]
                if (length(start) && s$order_rank[[symbol]] > start) next
            }
            proof <- data_column_definition(workspace, doc, site, proof = TRUE)
            if (is.null(proof) || equal_definition(proof$target, target)) return(TRUE)
            # Proving a later read checked every mention of this receiver
            # since its construction. Earlier reads in that interval have
            # the same origin. Reassignments begin a separate proof.
            if (symbol) covered[[key]] <- s$order_rank[[proof$constructor]]
        }
    }
    FALSE
}

data_mask_uncertain_rows <- function(mask, name) {
    c(mask$indirect_row[mask$indirect == name], mask$fallback[[name]])
}

data_mask_capture_rows <- function(mask, name) {
    unlist(lapply(mask$call_capture, function(group) {
        if (name %in% group$names) group$rows
    }), use.names = FALSE)
}

rename_lexical_document_error <- function(id, doc, uri, token, key, new_name = NULL) {
    if (!rename_document_complete(doc, uri, token, key)) return(rename_incomplete_error(id))
    syntax <- doc$parse_data$syntax_data
    if (is.null(key) && !is.null(new_name) && !identical(new_name, token$token)) {
        # The XML fallback has no occurrence keys to narrow these calls.
        captured <- data_mask_capture_rows(doc$parse_data$data_mask, new_name)
        if (length(captured) && token$token %in% reference_symbol_names(syntax$text[captured])) {
            return(ResponseErrorMessage$new(id, "RequestFailed", "The new name could call a different function in a data mask"))
        }
    }
    for (row in data_mask_uncertain_rows(doc$parse_data$data_mask, token$token)) {
        # A nested .env can refer to an enclosing mask. For literal keys
        # and ..name, a separately declared lexical binding rules out a hit.
        nested <- syntax$token[[row]] == "SYMBOL" && navigation_after_dollar(syntax, row)
        binding <- if (!nested && !is.null(key)) glue_lexical_target(doc, token$token, row)
        possible <- nested || is.null(key) || identical(binding$key, key) || (is.null(binding) && startsWith(key, "global:"))
        if (!possible && syntax$token[[row]] == "SYMBOL_FUNCTION_CALL" &&
                !(isTRUE(binding$value_row > 0L) && syntax$function_close[[binding$value_row]] > 0L)) {
            # Function lookup can skip a nearer value and reach an outer function.
            scope <- rename_target_scope(doc$parse_data, token$token, key)
            possible <- !is.null(scope) && (!scope || lsp_range_contains(navigation_range(doc, syntax, scope),
                    navigation_range(doc, syntax, row)$start))
        }
        if (possible) {
            return(ResponseErrorMessage$new(id, "RequestFailed", "A data-mask lookup may refer to this binding"))
        }
    }
    # The lexical occurrence index does not include string literals on the
    # left of an assignment. Editing only the callers would break the binding.
    if ((is.null(key) || startsWith(key, "global:")) && token$token %in% doc$parse_data$quoted_bindings) {
        return(ResponseErrorMessage$new(id, "RequestFailed", "Rename cannot update this string-literal assignment"))
    }
    NULL
}

prepare_rename_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename outside an R region"
        ))
    }

    site <- glue_reference_site(workspace, document, point)
    if (!is.null(site)) {
        checked <- glue_rename_references(id, workspace, document, point, site, NULL)
        if (!is.null(checked$error)) return(checked)
        if (!length(checked$result)) return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
        return(Response$new(id, result = site$range))
    }
    mask <- data_mask_site(document$parse_data, point)
    if (!is.null(mask) && !mask$env) return(data_column_rename_error(id))
    member <- if (is.null(mask)) member_navigation_at(workspace, document, point)
    if (!is.null(member)) {
        checked <- member_navigation_references(id, workspace, document, member, TRUE, TRUE)
        if (!is.null(checked$error)) return(checked)
        if (length(checked$result)) return(Response$new(id, result = member$range))
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    s4 <- s4_reference_target(workspace, document, point, include_binding = TRUE)
    if (!is.null(s4) && (is.null(s4$binding) || s4_reference_promotes_binding(workspace, s4))) return(s4_reference_rename_error(id))
    parsed <- workspace$get_parse_data(uri)
    token <- reference_token_at(document, parsed, point, mask)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    defn <- symbol_definition_reply(NULL, uri, workspace, document, point, mask = mask)

    logger$info("prepare_rename_reply: ", list(
        token = token,
        defn = defn$result
    ))

    if (member_navigation_writable(workspace, defn$result)) {
        key <- reference_key_at(parsed$reference_index, token$range$start, token$token)
        for (doc_uri in workspace_reference_document_uris(workspace, defn$result$uri, uri, key)) {
            doc <- workspace$documents$get(doc_uri, NULL)
            problem <- rename_lexical_document_error(id, doc, uri, token, key)
            if (!is.null(problem)) return(problem)
            if (!is.null(key)) {
                problem <- glue_rename_problem(workspace, doc, token$token, key)
                if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
            }
        }
        Response$new(
            id,
            result = range(
                start = document$to_lsp_position(
                    row = token$range$start$row,
                    col = token$range$start$col),
                end = document$to_lsp_position(
                    row = token$range$end$row,
                    col = token$range$end$col)
            )
        )
    } else {
        ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename the symbol"
        )
    }
}

#' Find the statement owning an explicitly declared function parameter
#' @noRd
rename_function_declaration <- function(syntax, formal) {
    declaration <- syntax$parent_row[[formal]]
    assigned <- FALSE
    repeat {
        parent <- syntax$parent_row[[declaration]]
        if (!parent) return(if (assigned || declaration == syntax$parent_row[[formal]]) declaration else 0L)
        children <- syntax_children(syntax, parent)
        children <- children[syntax$token[children] != "COMMENT"]
        tokens <- syntax$token[children]
        assignment <- length(children) == 3L &&
            ((tokens[[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN") && children[[3L]] == declaration) ||
                    (tokens[[2L]] == "RIGHT_ASSIGN" && children[[1L]] == declaration))
        parentheses <- length(children) == 3L && identical(tokens[c(1L, 3L)], c("'('", "')'")) &&
            children[[2L]] == declaration
        if (!assignment && !parentheses) return(if (assigned) declaration else 0L)
        assigned <- assigned || assignment
        declaration <- parent
    }
}

#' Locate explicit parameter names in an attached roxygen tag
#' @noRd
rename_roxygen_names <- function(text, offset) {
    value <- substr(text, offset + 1L, nchar(text))
    quoted <- startsWith(value, "`")
    pattern <- if (quoted) "^`(?:\\\\.|[^`\\\\])*`" else "^[^[:space:]]+"
    match <- regexpr(pattern, value, perl = TRUE)
    if (match[[1L]] != 1L) return(list())
    spelling <- regmatches(value, match)
    if (quoted) {
        return(list(list(name = reference_symbol_names(spelling),
                    start = offset, end = offset + nchar(spelling))))
    }
    names <- strsplit(spelling, ",", fixed = TRUE)[[1L]]
    if (!length(names) || any(!nzchar(names)) || endsWith(spelling, ",")) return(list())
    starts <- offset + cumsum(c(0L, nchar(utils::head(names, -1L)) + 1L))
    lapply(seq_along(names), function(i) {
        list(name = names[[i]], start = starts[[i]], end = starts[[i]] + nchar(names[[i]]))
    })
}

#' Rename attached @param tags only for formal declarations selected by Rename
#' @noRd
rename_roxygen_edits <- function(document, formal, new_name) {
    parsed <- document$parse_data
    syntax <- parsed$syntax_data
    point <- document$from_lsp_position(formal$range$start)
    row <- member_navigation_token(parsed, point)
    if (!row || syntax$token[[row]] != "SYMBOL_FORMALS" ||
            !equal_range(navigation_range(document, syntax, row), formal$range)) return(list())
    name <- reference_symbol_names(syntax$text[[row]])
    quote_label <- grepl("[[:space:],`]", new_name)
    replacement <- if (quote_label) quote_completion_name(new_name) else new_name
    edits <- list()
    add_tag_edits <- function(line, offset) {
        parameters <- rename_roxygen_names(document$content[[line + 1L]], offset)
        matches <- Filter(function(parameter) identical(parameter$name, name), parameters)
        # Only a whole roxygen label can be quoted. Quoting an individual
        # name changes how the other names and the description are parsed.
        if (length(matches) && length(parameters) > 1L && quote_label) return(FALSE)
        edits <<- c(edits, lapply(matches, function(parameter) {
            text_edit(range(document$to_lsp_position(line, parameter$start),
                    document$to_lsp_position(line, parameter$end)), replacement)
        }))
        TRUE
    }
    # Verified typed tags may be separated from their declaration by blank
    # lines or comments. Each union shares a single parameter-name spelling.
    sites <- attr(parsed$member_data$type_hints, "occurrences", exact = TRUE)
    typed <- NULL
    if (length(sites$parameter_row)) {
        owner <- syntax$parent_row[[row]]
        keep <- !is.na(sites$parameter_row) & sites$function_row == syntax$line1[[owner]] - 1L &
            sites$function_col == syntax$col1[[owner]] - 1L
        typed <- unique(sites[keep, c("parameter_row", "parameter_start"), drop = FALSE])
        for (i in seq_len(nrow(typed))) {
            if (!add_tag_edits(typed$parameter_row[[i]], typed$parameter_start[[i]])) return(NULL)
        }
    }
    declaration <- rename_function_declaration(syntax, row)
    if (!declaration) return(edits)
    first <- syntax$line1[[declaration]]
    prefix <- substr(document$content[[first]], 1L, syntax$col1[[declaration]] - 1L)
    if (first <= 1L || nzchar(trimws(prefix)) || !startsWith(trimws(document$content[[first - 1L]]), "#'")) return(edits)
    # Look up only the contiguous roxygen block immediately above this
    # declaration. Text resembling a tag inside a string is not a comment.
    first_comment <- first
    while (first_comment > 1L) {
        line <- first_comment - 1L
        text <- document$content[[line]]
        marker <- regexpr("#'", text, fixed = TRUE)[[1L]]
        if (marker < 1L || nzchar(trimws(substr(text, 1L, marker - 1L)))) break
        token <- navigation_token(parsed, line, marker, prefer_start = "COMMENT")
        if (!token || syntax$token[[token]] != "COMMENT" || syntax$col1[[token]] != marker) break
        first_comment <- line
    }
    if (first_comment == first) return(edits)
    rows <- seq.int(first_comment, first - 1L)
    for (row in rows) {
        text <- document$content[[row]]
        tag <- regexpr("^[[:space:]]*#'[[:space:]]*@param(?:[[:space:]]+|$)", text, perl = TRUE)
        if (tag[[1L]] != 1L) next
        offset <- attr(tag, "match.length")
        name_row <- row
        interrupted <- FALSE
        while (!nzchar(trimws(substr(text, offset + 1L, nchar(text)))) && name_row < first - 1L) {
            name_row <- name_row + 1L
            text <- document$content[[name_row]]
            continuation <- regexpr("^[[:space:]]*#'[[:space:]]*", text)
            offset <- attr(continuation, "match.length")
            if (startsWith(substr(text, offset + 1L, nchar(text)), "@")) {
                interrupted <- TRUE
                break
            }
        }
        if (interrupted || any(typed$parameter_row == name_row - 1L & typed$parameter_start == offset)) next
        if (!add_tag_edits(name_row - 1L, offset)) return(NULL)
    }
    edits
}

#' @noRd
rename_reply <- function(id, uri, workspace, document, point, newName) {
    if (!is.character(newName) || length(newName) != 1L || is.na(newName) || !nzchar(newName)) {
        return(ResponseErrorMessage$new(id, "InvalidParams", "The new name must be a non-empty string"))
    }
    if (!check_r_region(document, point)) {
        return(Response$new(id))
    }

    site <- glue_reference_site(workspace, document, point)
    if (!is.null(site)) {
        refs <- glue_rename_references(id, workspace, document, point, site, newName)
    } else {
        refs <- references_reply(id, uri, workspace, document, point, for_rename = TRUE, new_name = newName)
    }
    if (!is.null(refs$error)) return(refs)
    formal <- attr(refs$result, "rename_formal", exact = TRUE)
    topic <- attr(refs$result, "rename_topic", exact = TRUE)
    if (!is.null(formal) || !is.null(topic)) {
        owner_uri <- if (is.null(topic)) formal$uri else topic$uri
        owner <- workspace$documents$get(owner_uri, NULL)
        extra <- roxygen_topic_rename(workspace, owner, topic$name, newName, if (is.null(topic)) formal, owner_uri)
        if (!is.null(extra$problem)) return(ResponseErrorMessage$new(id, "RequestFailed", extra$problem))
        if (length(extra$locations)) refs$result <- c(refs$result, extra$locations)
    }
    uris <- vapply(refs$result, function(ref) ref$uri, character(1L))
    groups <- split(seq_along(uris), factor(uris, levels = unique(uris)))
    replacement <- quote_completion_name(newName)
    result <- lapply(groups, function(indices) {
        lapply(refs$result[indices], function(ref) {
            quote <- attr(ref, "spelling", exact = TRUE)
            roxygen_label <- attr(ref, "roxygen_label", exact = TRUE)
            text <- if (!is.null(roxygen_label)) roxygen_label else if (isTRUE(attr(ref, "roxygen_type", exact = TRUE))) newName else
                if (length(quote) && nzchar(quote)) encodeString(newName, quote = quote) else replacement
            fragment <- attr(ref, "fragment", exact = TRUE)
            if (!is.null(fragment)) text <- glue_encode_text(fragment, text)
            text_edit(range = ref$range, new_text = text)
        })
    })
    if (!is.null(formal)) {
        doc <- workspace$documents$get(formal$uri, NULL)
        if (!is.null(doc)) {
            documentation <- rename_roxygen_edits(doc, formal, newName)
            if (is.null(documentation)) {
                return(ResponseErrorMessage$new(id, "RequestFailed", "The new name cannot be represented in this grouped roxygen tag"))
            }
            if (length(documentation)) result[[formal$uri]] <- c(result[[formal$uri]], documentation)
        }
    }

    logger$info("rename_reply: ", result)

    if (length(result)) {
        Response$new(
            id,
            result = list(
                changes = result
            )
        )
    } else {
        Response$new(
            id
        )
    }
}
