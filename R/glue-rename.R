# Stitched expressions have no single source edit. A damaged expression may
# also hide a reference. Inspect only the affected call and requested name.
glue_unmapped_name <- function(fragment, name) {
    lines <- fragment$content
    if (is.null(lines)) return(TRUE)
    source <- srcfilecopy("<glue>", lines)
    tryCatch(parse(text = lines, srcfile = source, keep.source = TRUE), error = function(e) NULL)
    data <- utils::getParseData(source)
    if (!is.null(data)) {
        symbols <- data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS", "SYMBOL_SUB")
        if (name %in% reference_symbol_names(data$text[symbols])) return(TRUE)
    }
    !isTRUE(fragment$complete) && (any(grepl(name, lines, fixed = TRUE)) || any(grepl("\\", lines, fixed = TRUE)))
}

# An edited read may sit in a function declared inside the decoded string.
# Consult its lexical scope only if the fragment declares the proposed name.
glue_rename_inner_capture <- function(document, site, new) {
    if (is.null(site$fragment) || !new %in% site$inner_bindings) return(FALSE)
    inner <- glue_fragment_document(document, site$fragment)
    pos <- glue_inner_point(site$fragment, document$from_lsp_position(site$range$start))
    row <- member_navigation_token(inner$parse_data, pos)
    row && !is.null(glue_lexical_target(inner, new, row))
}

glue_rename_problem <- function(workspace, doc, name, key, new_name = NULL, local_call = NULL,
    sites = glue_reference_occurrences(workspace, doc, name)) {
    fragments <- doc$parse_data$glue_templates
    unmapped <- attr(fragments, "unmapped", exact = TRUE)
    if (!length(fragments) && !length(unmapped)) return(NULL)
    local <- !is.null(local_call)
    in_call <- function(site) !local || identical(site$call_row, local_call)
    could_bind <- function(site) {
        if (!in_call(site)) return(FALSE)
        if (local || !isTRUE(site$call_row > 0L)) return(TRUE)
        context <- glue_context_at(workspace, doc, site, name)
        if (isTRUE(context$shadowed)) return(FALSE)
        if (is.null(context$target$key)) return(isTRUE(context$blocked) || startsWith(key, "global:"))
        identical(context$target$key, key)
    }
    ambiguous <- "A glue expression may refer to this name and cannot be renamed precisely"
    blocked <- doc$parse_data$glue_rename_unmapped
    if (is.null(blocked)) {
        blocked <- c(unmapped, Filter(function(fragment) !isTRUE(fragment$complete), fragments), list())
        doc$parse_data$glue_rename_unmapped <- blocked
    }
    for (fragment in blocked) {
        if (glue_unmapped_name(fragment, name) && could_bind(fragment)) return(ambiguous)
    }
    for (site in sites) {
        if (!in_call(site) || isTRUE(site$argument_tag)) next
        if (is.null(site$key) && could_bind(site)) return(ambiguous)
        if (!identical(site$key, key)) next
        if (!isTRUE(site$target$writable)) return(ambiguous)
        if (is.null(new_name) || identical(new_name, name)) next
        if (!is.null(site$fragment) && grepl("^\\.\\.(\\.|[0-9]+)$", new_name)) {
            return("The new name has special argument lookup inside a glue expression")
        }
        if (glue_rename_inner_capture(doc, site, new_name)) {
            return("The new name is already bound in this embedded R scope")
        }
        context <- glue_context_at(workspace, doc, site, new_name)
        # The existing lexical collision check covers ordinary R. This also
        # inspects names visible only at the embedded use (e.g. a formal).
        if (new_name %in% context$names || (!local && !is.null(context$target$range))) {
            return("The new name would resolve to another binding in a glue expression")
        }
    }
    if (is.null(new_name) || identical(new_name, name)) return(NULL)
    # A binding that hid the old spelling need not hide the new one. For
    # fields, identify the data input independently of the visible old name.
    # For lexical names, check the declaration's scope even when a named
    # glue argument masked its old spelling.
    field <- startsWith(key, "glue:field:")
    lexical_scope <- NULL
    for (site in glue_reference_occurrences(workspace, doc, new_name)) {
        if (!in_call(site) || isTRUE(site$argument_tag) || identical(site$target$kind, "inner")) next
        if (local) {
            # A new named argument cannot affect promises created before it.
            if (startsWith(key, "glue:named:") && is.finite(site$before)) {
                layers <- glue_call_bindings(workspace, doc, site)
                i <- match(key, vapply(layers$named, "[[", "", "key"))
                if (!is.na(i) && site$before <= layers$tags[[i]]) next
            }
            return("Renaming would capture an existing use of the new name in a glue expression")
        }
        if (!is.null(site$key) && !identical(site$target$kind, "lexical")) next
        if (field) {
            layers <- glue_call_bindings(workspace, doc, site)
            current <- layers$fields[[name]]
            if (!identical(current$key, key)) {
                if (!is.null(current) || !isTRUE(layers$open) ||
                        !isTRUE(endsWith(as.character(layers$call$name), "_data"))) next
            }
        } else if (startsWith(key, "local:")) {
            if (is.null(lexical_scope)) lexical_scope <- rename_target_scope(doc$parse_data, name, key)
            if (is.null(lexical_scope)) next
            if (lexical_scope) {
                selection <- navigation_range(doc, doc$parse_data$syntax_data, lexical_scope)
                point <- site$range$start
                if (is.null(point) && length(site$fragment$start)) {
                    point <- doc$to_lsp_position(site$fragment$start$row, site$fragment$start$col)
                }
                if (!is.null(point) && !lsp_range_contains(selection, point)) next
            }
        }
        return("Renaming would capture an existing use of the new name in a glue expression")
    }
    NULL
}

# Redispatch lexical symbols at their real declarations so workspace, formal,
# capture and documentation checks follow the same path from either cursor.
glue_rename_references <- function(id, workspace, document, point, site, new_name) {
    site <- reference_source_glue_site(workspace, document, site)
    target <- site$target
    if (!isTRUE(target$writable) || is.null(site$key)) {
        return(ResponseErrorMessage$new(id, "RequestFailed", "Cannot establish a writable glue binding"))
    }
    owner <- workspace$documents$get(target$uri, NULL)
    if (is.null(owner)) return(rename_incomplete_error(id))
    if (identical(target$kind, "lexical")) {
        index <- owner$parse_data$reference_index
        declarations <- reference_indices(index, site$name, site$key)
        declarations <- declarations[index$is_definition[declarations]]
        if (!length(declarations)) return(ResponseErrorMessage$new(id, "RequestFailed", "Cannot locate the declaration for rename"))
        row <- declarations[[length(declarations)]]
        declaration <- list(row = index$line[[row]], col = index$code_point_col[[row]] + 1L)
        return(references_reply(id, owner$uri, workspace, owner, declaration, for_rename = TRUE, new_name = new_name))
    }
    if (!rename_document_complete(owner, document$uri, list(token = site$name,
                range = list(start = point, end = point)), site$key)) return(rename_incomplete_error(id))
    if (!is.null(new_name) && identical(target$kind, "named")) {
        controls <- c("...", ".x", ".sep", ".envir", ".open", ".close", ".na", ".null", ".comment", ".literal", ".transformer", ".trim")
        if (new_name %in% controls) return(ResponseErrorMessage$new(id, "RequestFailed", "The new name is a glue control argument"))
    }
    if (identical(target$kind, "field")) {
        if (isTRUE(target$shared)) {
            pos <- owner$from_lsp_position(target$range$start)
            member <- member_navigation_at(workspace, owner, pos)
            if (is.null(member$definition$range) || !equal_definition(member$definition, target)) {
                return(ResponseErrorMessage$new(id, "RequestFailed", "Cannot establish the data field declaration"))
            }
            return(member_navigation_references(id, workspace, document, member, TRUE, TRUE, new_name))
        }
        problem <- glue_rename_field_problem(owner, target$range, new_name)
        if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
    }
    occurrences <- glue_reference_occurrences(workspace, owner, site$name)
    problem <- glue_rename_problem(workspace, owner, site$name, site$key, new_name, site$call_row, occurrences)
    if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
    Response$new(id, result = glue_overlay_locations(workspace, owner, site$name, site$key, list(), occurrences = occurrences))
}

# data.frame() interprets control arguments and repairs non-syntactic names.
glue_rename_field_problem <- function(owner, selection, new) {
    if (is.null(new)) return(NULL)
    parsed <- owner$parse_data
    syntax <- parsed$syntax_data
    row <- member_navigation_token(parsed, owner$from_lsp_position(selection$start))
    if (!row) return(NULL)
    ctor <- syntax_call_arguments(syntax, syntax$parent_row[[row]])
    if (identical(ctor$name, "data.frame") && (new %in% names(formals(base::data.frame)) || !identical(make.names(new), new))) {
        return("The constructor would change the glue data field name")
    }
    NULL
}

# Existing member analysis checks aliases and dynamic subscripts. In files
# with templates, include mapped uses of the same list/data.frame declaration.
glue_member_rename <- function(id, workspace, member, uris, refs, new_name) {
    target <- member$definition
    owner <- workspace$documents$get(target$uri)
    parsed <- owner$parse_data
    at <- member_navigation_token(parsed, owner$from_lsp_position(target$range$start))
    if (!at) return(Response$new(id, result = refs))
    ctor <- syntax_call_arguments(parsed$syntax_data, parsed$syntax_data$parent_row[[at]])
    if (!length(ctor$name) || !ctor$name %in% c("list", "data.frame")) return(Response$new(id, result = refs))
    key <- paste("glue", "field", target$uri, target$range$start$line, target$range$start$character, sep = ":")
    for (uri in uris) {
        doc <- workspace$documents$get(uri, NULL)
        sites <- glue_reference_occurrences(workspace, doc, member$name)
        problem <- glue_rename_problem(workspace, doc, member$name, key, new_name, sites = sites)
        if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
        hits <- Filter(function(site) identical(site$key, key), sites)
        if (!length(hits)) next
        problem <- glue_rename_field_problem(owner, target$range, new_name)
        if (!is.null(problem)) return(ResponseErrorMessage$new(id, "RequestFailed", problem))
        refs <- c(refs, glue_overlay_locations(workspace, doc, member$name, key, list(), occurrences = hits))
    }
    if (length(refs)) refs <- refs[!duplicated(vapply(refs, member_navigation_key, character(1L)))]
    Response$new(id, result = refs)
}
