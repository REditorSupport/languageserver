prepare_rename_reply <- function(id, uri, workspace, document, point) {
    if (!check_r_region(document, point)) {
        return(ResponseErrorMessage$new(
            id,
            errortype = "RequestCancelled",
            message = "Cannot rename outside an R region"
        ))
    }

    token <- reference_token_at(document, workspace$get_parse_data(uri), point)
    if (!nzchar(token$token) || !check_scope(uri, document, point)) {
        return(ResponseErrorMessage$new(id, "RequestCancelled", "Cannot rename the symbol"))
    }
    defn <- definition_reply(NULL, uri, workspace, document, point)

    logger$info("prepare_rename_reply: ", list(
        token = token,
        defn = defn$result
    ))

    if (length(defn$result)) {
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

    refs <- references_reply(NULL, uri, workspace, document, point)
    uris <- vapply(refs$result, function(ref) ref$uri, character(1L))
    groups <- split(seq_along(uris), factor(uris, levels = unique(uris)))
    replacement <- quote_completion_name(newName)
    result <- lapply(groups, function(indices) {
        lapply(refs$result[indices], function(ref) {
            text_edit(range = ref$range, new_text = replacement)
        })
    })
    documentation <- rename_roxygen_edits(uri, workspace, document, refs$result, newName)
    if (length(documentation)) result[[uri]] <- c(result[[uri]], documentation)

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
