# Registration literals belong to the source generic, while same-spelled
# strings, lexical functions and methods on an object have other identities.
s4_reference_registrations <- function(workspace, uri, name) {
    index <- workspace$index
    indexed <- if (isTRUE(index$enabled) && is.function(index$s4_declaration_uris)) index$s4_declaration_uris(name)
    loaded <- if (is.function(workspace$s4_declaration_uris)) workspace$s4_declaration_uris(name) else workspace$documents$keys()
    sources <- as.character(unique(unlist(c(loaded, indexed), use.names = FALSE)))
    if (!length(sources)) return(list())
    # Use the same live-buffer precedence and range records as F12. Historical
    # snapshots without the declaration index contribute no registrations.
    if (length(indexed)) {
        open <- workspace_s4_open_uris(workspace, uri, workspace$documents$keys())
        aliases <- unname(open[vapply(sources, index_canonical_uri, character(1L))])
        sources[!is.na(aliases)] <- aliases[!is.na(aliases)]
    }
    result <- list()
    for (source in unique(sources)) {
        data <- workspace_s4_source_data(workspace, uri, source)
        for (record in data$s4_declarations[[name]]) {
            result[[length(result) + 1L]] <- list(uri = source, range = record$range,
                type = if (record$kind == "generic") "S4Generic" else "S4Method")
        }
    }
    result
}

s4_reference_anchor <- function(declarations, scope) {
    visible <- Filter(function(x) x$uri %in% scope, declarations)
    generics <- Filter(function(x) x$type == "S4Generic", visible)
    if (length(generics) == 1L) return(generics[[1L]]$uri)
    if (length(generics)) return(NULL)
    # Without a source generic, method registrations in one source file can
    # establish identity. Independent method-only files are ambiguous.
    owners <- unique(vapply(visible, `[[`, "", "uri"))
    if (length(owners) == 1L) owners
}

s4_reference_owner <- function(workspace, declarations, scope) {
    anchor <- s4_reference_anchor(declarations, scope)
    generic_files <- unique(vapply(Filter(function(x) x$type == "S4Generic", declarations), `[[`, "", "uri"))
    if (is.null(anchor) || !length(generic_files) || anchor %in% generic_files) return(anchor)
    # A script can source the generic and its method file separately. Link a
    # method-only file when every caller that supplies a generic agrees.
    callers <- reference_source_scope(workspace, anchor, callers = TRUE)
    linked <- character()
    for (caller in setdiff(callers, anchor)) {
        linked <- union(linked, intersect(generic_files, reference_source_scope(workspace, caller)))
        if (length(linked) > 1L) return(anchor)
    }
    if (length(linked)) linked else anchor
}

s4_reference_files <- function(workspace, target) {
    if (is.null(target$anchor)) return(character())
    files <- reference_source_scope(workspace, target$anchor, callers = TRUE)
    declarations <- unique(vapply(target$declarations, `[[`, "", "uri"))
    for (uri in setdiff(declarations, files)) {
        owner <- s4_reference_owner(workspace, target$declarations, reference_source_scope(workspace, uri))
        if (identical(owner, target$anchor)) files <- c(files, uri)
    }
    files
}

s4_reference_name <- function(parsed, row) {
    syntax <- parsed$syntax_data
    kind <- syntax$token[[row]]
    if (kind == "STR_CONST") return(s4_definition_literal(parsed, row))
    if (kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
        if (length(syntax_children(syntax, syntax$parent_row[[row]])) != 1L) return(NULL)
        name <- reference_symbol_names(syntax$text[[row]])
    } else if (kind %in% s4_definition_operators) {
        name <- if (kind == "'^'") "^" else syntax$text[[row]]
    } else {
        return(NULL)
    }
    if (kind %in% c("SYMBOL_FUNCTION_CALL", "'['", "LBB") && s4_definition_replacement(syntax, row)) {
        name <- paste0(name, "<-")
    }
    name
}

s4_reference_direct <- function(declarations, scope) {
    lapply(Filter(function(x) x$uri %in% scope, declarations), function(x) location(x$uri, x$range))
}

s4_reference_unmasked <- function(workspace, doc, row, name, direct) {
    point <- member_navigation_point(doc$parse_data$syntax_data, row)
    is.null(s4_masking_definition(workspace, doc, point, row, name, direct))
}

s4_reference_target <- function(workspace, document, point) {
    parsed <- document$parse_data
    syntax <- parsed$syntax_data
    if (is.null(syntax)) return(NULL)
    row <- navigation_token(parsed, point$row + 1L, point$col + 1L, c("SYMBOL", "SYMBOL_FUNCTION_CALL", s4_definition_operators))
    if (!row) return(NULL)
    name <- s4_reference_name(parsed, row)
    if (is.null(name)) return(NULL)
    declarations <- s4_reference_registrations(workspace, document$uri, name)
    if (!length(declarations)) return(NULL)
    scope <- reference_source_scope(workspace, document$uri)
    direct <- s4_reference_direct(declarations, scope)
    if (!length(direct)) return(NULL)
    if (syntax$token[[row]] != "STR_CONST" && !s4_reference_unmasked(workspace, document, row, name, direct)) return(NULL)
    list(name = name, declarations = declarations, anchor = s4_reference_owner(workspace, declarations, scope))
}

s4_reference_locations <- function(workspace, target, include_declaration) {
    if (is.null(target$anchor)) return(list())
    result <- list()
    for (uri in s4_reference_files(workspace, target)) {
        doc <- workspace$documents$get(uri, NULL)
        if (is.null(doc)) next
        data <- current_parse_data(uri, workspace, doc)
        if (is.null(data)) next
        scope <- reference_source_scope(workspace, uri)
        if (!identical(s4_reference_owner(workspace, target$declarations, scope), target$anchor)) next
        syntax <- data$syntax_data
        direct <- s4_reference_direct(target$declarations, scope)
        declarations <- Filter(function(x) x$uri == uri, target$declarations)
        spellings <- unique(c(target$name, sub("<-$", "", target$name)))
        # Interrogate only strings with the selected spelling. The helper
        # resolver verifies the actual formal argument, namespace and shadowing.
        strings <- which(syntax$token == "STR_CONST")
        strings <- strings[class_token_text(syntax$text[strings]) %in% spellings]
        for (row in strings) {
            if (!identical(s4_definition_literal(data, row), target$name)) next
            call <- syntax$parent_row[[syntax$parent_row[[row]]]]
            call_range <- navigation_range(doc, syntax, call)
            declared <- any(vapply(declarations, function(x) equal_range(x$range, call_range), logical(1L)))
            if (declared && !include_declaration) next
            result[[length(result) + 1L]] <- location(uri, navigation_range(doc, syntax, row))
        }
        index <- data$reference_index
        matches <- sort(unlist(lapply(spellings, function(name) reference_indices(index, name, paste0("global:", name))),
                use.names = FALSE))
        matches <- matches[index$token[matches] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") & !index$is_definition[matches]]
        rows <- vapply(matches, function(i) {
            member_navigation_token(data, list(row = index$line[[i]], col = index$code_point_col[[i]]))
        }, integer(1L))
        operators <- which(syntax$token %in% s4_definition_operators &
                syntax$text %in% spellings)
        if ("^" %in% spellings) operators <- union(operators, which(syntax$token == "'^'"))
        for (row in sort(unique(c(rows[rows > 0L], operators)))) {
            if (identical(s4_reference_name(data, row), target$name) &&
                    s4_reference_unmasked(workspace, doc, row, target$name, direct)) {
                result[[length(result) + 1L]] <- location(uri, navigation_range(doc, syntax, row))
            }
        }
    }
    result
}

# A later ordinary assignment can shadow a generic in the same script.
# The lexical index calls both global:name; preserve the earlier S4 calls
# when renaming the later binding.
s4_reference_lexical_indices <- function(workspace, doc, selected, name) {
    if (!length(selected)) return(selected)
    defs <- s4_reference_registrations(workspace, doc$uri, name)
    if (!length(defs)) return(selected)
    direct <- s4_reference_direct(defs, reference_source_scope(workspace, doc$uri))
    if (!length(direct)) return(selected)
    parsed <- doc$parse_data
    index <- parsed$reference_index
    keep <- vapply(selected, function(i) {
        if (index$is_definition[[i]]) return(TRUE)
        row <- member_navigation_token(parsed, list(row = index$line[[i]], col = index$code_point_col[[i]]))
        !row || !identical(s4_reference_name(parsed, row), name) ||
            !s4_reference_unmasked(workspace, doc, row, name, direct)
    }, logical(1L))
    selected[keep]
}

s4_reference_rename_error <- function(id) {
    ResponseErrorMessage$new(id, "RequestFailed", "Renaming an S4 generic and its registrations is unsupported")
}
