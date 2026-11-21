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
rename_lexical_conflict <- function(workspace, doc, selected, new, owner_uri, owner_scope) {
    if (is.null(owner_scope) || (owner_scope && !identical(doc$uri, owner_uri))) return(FALSE)
    parsed <- doc$parse_data
    index <- parsed$reference_index
    if (!owner_scope && isTRUE(workspace$index$enabled) && is.function(workspace$index$has_definition) &&
            workspace$index$has_definition(new, uri = doc$uri)) return(TRUE)
    existing <- reference_indices(index, new)
    existing <- existing[!startsWith(index$definition_key[existing], "package:")]
    if (!length(existing)) return(FALSE)
    syntax <- parsed$syntax_data
    stride <- as.double(syntax$maxcol) + 1
    at <- function(i) as.double(index$line[i] + 1L) * stride + index$code_point_col[i] + 1L
    start <- function(rows) as.double(syntax$line1[rows]) * stride + syntax$col1[rows]
    end <- function(rows) as.double(syntax$line2[rows]) * stride + syntax$col2[rows]
    if (owner_scope) {
        position <- at(existing)
        existing <- existing[position >= start(owner_scope) & position <= end(owner_scope)]
        if (!length(existing)) return(FALSE)
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
    if (!length(reads)) return(FALSE)
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

    any(!covered(at(reads), first, end(scopes)))
}

rename_incomplete_error <- function(id) {
    ResponseErrorMessage$new(id, "RequestFailed", "Incomplete R code may contain other uses of this symbol")
}

prepare_rename_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename outside an R region"
        ))
    }

    member <- member_navigation_at(workspace, document, point)
    if (!is.null(member)) {
        checked <- member_navigation_references(id, workspace, document, member, TRUE, TRUE)
        if (!is.null(checked$error)) return(checked)
        if (length(checked$result)) return(Response$new(id, result = member$range))
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    s4 <- s4_reference_target(workspace, document, point, include_binding = TRUE)
    if (!is.null(s4) && (is.null(s4$binding) || s4_reference_promotes_binding(workspace, s4))) return(s4_reference_rename_error(id))
    parsed <- workspace$get_parse_data(uri)
    token <- reference_token_at(document, parsed, point)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    defn <- symbol_definition_reply(NULL, uri, workspace, document, point)

    logger$info("prepare_rename_reply: ", list(
        token = token,
        defn = defn$result
    ))

    if (member_navigation_writable(workspace, defn$result)) {
        key <- reference_key_at(parsed$reference_index, token$range$start, token$token)
        for (doc_uri in workspace_reference_document_uris(workspace, defn$result$uri, uri, key)) {
            doc <- workspace$documents$get(doc_uri, NULL)
            if (!rename_document_complete(doc, uri, token, key)) return(rename_incomplete_error(id))
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
rename_roxygen_edits <- function(uri, workspace, document, references, new_name) {
    syntax <- workspace$get_parse_data(uri)$syntax_data
    formals <- which(syntax$token == "SYMBOL_FORMALS")
    if (!length(formals) || !length(references)) return(list())
    range_key <- function(value) {
        paste(value$start$line, value$start$character,
            value$end$line, value$end$character, sep = ":")
    }
    own_references <- references[vapply(references, function(ref) identical(ref$uri, uri), logical(1L))]
    reference_ranges <- vapply(own_references, function(ref) range_key(ref$range), character(1L))
    selected <- formals[vapply(formals, function(row) {
        range_key(navigation_range(document, syntax, row)) %in% reference_ranges
    }, logical(1L))]
    edits <- list()
    comment_lines <- logical(document$nline)
    comments <- which(syntax$token == "COMMENT" & startsWith(syntax$text, "#'"))
    # String contents may resemble roxygen and have a trailing real comment.
    # Require the roxygen marker to start the actual standalone comment token.
    comment_lines[syntax$line1[comments]] <- vapply(comments, function(row) {
        text <- sub("^[[:space:]]*", "", document$content[[syntax$line1[[row]]]])
        identical(gsub("\t", " ", text, fixed = TRUE), syntax$text[[row]])
    }, logical(1L))
    replacement <- if (grepl("[[:space:],]", new_name)) quote_completion_name(new_name) else new_name
    for (formal in selected) {
        declaration <- rename_function_declaration(syntax, formal)
        if (!declaration) next
        first <- syntax$line1[[declaration]]
        prefix <- substr(document$content[[first]], 1L, syntax$col1[[declaration]] - 1L)
        if (nzchar(trimws(prefix))) next
        rows <- integer()
        row <- first - 1L
        while (row > 0L && comment_lines[[row]] && grepl("^[[:space:]]*#'", document$content[[row]])) {
            rows <- c(row, rows)
            row <- row - 1L
        }
        name <- reference_symbol_names(syntax$text[[formal]])
        for (row in rows) {
            text <- document$content[[row]]
            tag <- regexpr("^[[:space:]]*#'[[:space:]]*@param(?:[[:space:]]+|$)", text, perl = TRUE)
            if (tag[[1L]] != 1L) next
            offset <- attr(tag, "match.length")
            name_row <- row
            interrupted <- FALSE
            while (!nzchar(trimws(substr(text, offset + 1L, nchar(text)))) && name_row < first - 1L) {
                name_row <- name_row + 1L
                if (!comment_lines[[name_row]]) {
                    interrupted <- TRUE
                    break
                }
                text <- document$content[[name_row]]
                continuation <- regexpr("^[[:space:]]*#'[[:space:]]*", text)
                offset <- attr(continuation, "match.length")
                if (startsWith(substr(text, offset + 1L, nchar(text)), "@")) {
                    interrupted <- TRUE
                    break
                }
            }
            if (interrupted) next
            names <- rename_roxygen_names(text, offset)
            for (parameter in names) {
                if (!identical(parameter$name, name)) next
                edits[[length(edits) + 1L]] <- text_edit(range(
                    start = document$to_lsp_position(name_row - 1L, parameter$start),
                    end = document$to_lsp_position(name_row - 1L, parameter$end)), replacement)
            }
        }
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

    refs <- references_reply(id, uri, workspace, document, point, for_rename = TRUE, new_name = newName)
    if (!is.null(refs$error)) return(refs)
    uris <- vapply(refs$result, function(ref) ref$uri, character(1L))
    groups <- split(seq_along(uris), factor(uris, levels = unique(uris)))
    replacement <- quote_completion_name(newName)
    result <- lapply(groups, function(indices) {
        lapply(refs$result[indices], function(ref) {
            quote <- attr(ref, "spelling", exact = TRUE)
            text <- if (length(quote) && nzchar(quote)) encodeString(newName, quote = quote) else replacement
            text_edit(range = ref$range, new_text = text)
        })
    })
    for (doc_uri in names(result)) {
        doc <- workspace$documents$get(doc_uri, NULL)
        if (is.null(doc)) next
        documentation <- rename_roxygen_edits(doc_uri, workspace, doc, refs$result[groups[[doc_uri]]], newName)
        if (length(documentation)) result[[doc_uri]] <- c(result[[doc_uri]], documentation)
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
