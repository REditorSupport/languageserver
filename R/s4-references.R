# Registration literals belong to the source generic, while same-spelled
# strings, lexical functions and methods on an object have other identities.
s4_reference_catalog <- function(workspace) {
    cache <- workspace$type_hierarchy_cache
    revision <- list(workspace$s3_revision, workspace$documents$size(), workspace$index, workspace$index$revision)
    if (!is.null(cache)) {
        saved <- cache$get("s4_reference_registrations", NULL)
        if (!is.null(saved) && identical(saved$revision, revision)) return(saved$names)
    }
    declarations <- Filter(function(x) {
        x$type %in% c("S4Generic", "S4Method") && x$literal_row > 0L && nzchar(x$class_name)
    }, class_workspace_declarations(workspace))
    names <- list2env(split(declarations, vapply(declarations, `[[`, "", "class_name")), parent = emptyenv())
    if (!is.null(cache)) cache$set("s4_reference_registrations", list(revision = revision, names = names))
    names
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

s4_reference_shadow <- function(workspace, uri, name, declarations, scope) {
    for (peer in setdiff(scope, uri)) {
        doc <- workspace$documents$get(peer, NULL)
        definitions <- if (!is.null(doc)) {
            doc$parse_data$definitions
        } else if (isTRUE(workspace$index$enabled)) {
            workspace$index$summaries$get(peer, NULL)$definitions
        }
        binding <- definitions[[name]]
        if (is.null(binding)) next
        if (!any(vapply(declarations, function(x) {
            x$binding && x$uri == peer && identical(x$name, name) && equal_range(x$range, binding$range)
        }, logical(1L)))) return(TRUE)
    }
    FALSE
}

s4_reference_bare <- function(doc, row, name, declarations) {
    data <- doc$parse_data
    syntax <- data$syntax_data
    if (length(syntax_children(syntax, syntax$parent_row[[row]])) != 1L) return(FALSE)
    binding <- navigation_definitions(data, syntax$line1[[row]], syntax$col1[[row]], name, token = row, last = TRUE)
    if (!length(binding)) return(TRUE)
    rng <- navigation_range(doc, syntax, binding[[1L]])
    any(vapply(declarations, function(x) {
        x$binding && x$uri == doc$uri && identical(x$name, name) && equal_range(x$range, rng)
    }, logical(1L)))
}

s4_reference_call_name <- function(syntax, row, name) {
    if (syntax$token[[row]] != "SYMBOL_FUNCTION_CALL") return(name)
    call <- syntax$parent_row[[syntax$parent_row[[row]]]]
    if (!call) return(name)
    outer <- syntax$parent_row[[call]]
    if (!outer) return(name)
    kids <- syntax_children(syntax, outer)
    kids <- kids[syntax$token[kids] != "COMMENT"]
    if (length(kids) == 3L) {
        assignment <- syntax$token[[kids[[2L]]]]
        if ((kids[[1L]] == call && assignment %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) ||
                (kids[[3L]] == call && assignment == "RIGHT_ASSIGN")) return(paste0(name, "<-"))
    }
    name
}

s4_reference_target <- function(workspace, document, point) {
    parsed <- document$parse_data
    syntax <- parsed$syntax_data
    if (is.null(syntax)) return(NULL)
    row <- member_navigation_token(parsed, point)
    if (!row) return(NULL)
    kind <- syntax$token[[row]]
    if (kind == "STR_CONST") {
        literal <- Filter(function(x) {
            x$type %in% c("S4Generic", "S4Method") && identical(x$literal_row, row)
        }, parsed$class_data$declarations)
        if (!length(literal)) return(NULL)
        name <- literal[[1L]]$class_name
    } else if (kind %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") &&
            length(syntax_children(syntax, syntax$parent_row[[row]])) == 1L) {
        bare <- reference_symbol_names(syntax$text[[row]])
        key <- reference_key_at(parsed$reference_index, point, bare)
        if (!is.null(key) && startsWith(key, "local:")) return(NULL)
        name <- s4_reference_call_name(syntax, row, bare)
    } else {
        return(NULL)
    }
    # A miss on an ordinary call never walks the document's package/source
    # scope. The name table also covers unopened files in the shallow index.
    declarations <- s4_reference_catalog(workspace)[[name]]
    if (!length(declarations)) return(NULL)
    scope <- reference_source_scope(workspace, document$uri)
    if (!any(vapply(declarations, function(x) x$uri %in% scope, logical(1L)))) return(NULL)
    if (kind != "STR_CONST" && (!s4_reference_bare(document, row, name, declarations) ||
                s4_reference_shadow(workspace, document$uri, name, declarations, scope))) return(NULL)
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
        if (isTRUE(include_declaration)) {
            for (declaration in data$class_data$declarations) {
                if (!declaration$type %in% c("S4Generic", "S4Method") || declaration$class_name != target$name ||
                        declaration$literal_row <= 0L) next
                result[[length(result) + 1L]] <- location(uri, navigation_range(doc, data$syntax_data, declaration$literal_row))
            }
        }
        index <- data$reference_index
        spellings <- unique(c(target$name, sub("<-$", "", target$name)))
        matches <- sort(unlist(lapply(spellings, function(name) reference_indices(index, name, paste0("global:", name))),
                use.names = FALSE))
        matches <- matches[index$token[matches] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") & !index$is_definition[matches]]
        if (!length(matches) || s4_reference_shadow(workspace, uri, target$name, target$declarations, scope)) next
        for (i in matches) {
            row <- member_navigation_token(data, list(row = index$line[[i]], col = index$code_point_col[[i]]))
            if (row && identical(s4_reference_call_name(data$syntax_data, row, index$name[[i]]), target$name) &&
                    s4_reference_bare(doc, row, target$name, target$declarations)) {
                result[[length(result) + 1L]] <- location(uri, navigation_range(doc, data$syntax_data, row))
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
    defs <- s4_reference_catalog(workspace)[[name]]
    if (!length(defs)) return(selected)
    scope <- reference_source_scope(workspace, doc$uri)
    if (!any(vapply(defs, function(x) x$uri %in% scope, logical(1L))) ||
            s4_reference_shadow(workspace, doc$uri, name, defs, scope)) return(selected)
    parsed <- doc$parse_data
    index <- parsed$reference_index
    keep <- vapply(selected, function(i) {
        row <- member_navigation_token(parsed, list(row = index$line[[i]], col = index$code_point_col[[i]]))
        if (!row) return(TRUE)
        if (index$is_definition[[i]]) {
            rng <- navigation_range(doc, parsed$syntax_data, row)
            declared <- vapply(defs, function(x) {
                x$binding && doc$uri == x$uri && identical(x$name, name) && equal_range(rng, x$selectionRange)
            }, logical(1L))
            return(!any(declared))
        }
        !s4_reference_bare(doc, row, name, defs)
    }, logical(1L))
    selected[keep]
}

s4_reference_rename_error <- function(id) {
    ResponseErrorMessage$new(id, "RequestFailed", "Renaming an S4 generic and its registrations is unsupported")
}
