# An ordinary function can be promoted in its own file or a different source
# file. A binding inside a function always belongs to that lexical scope.
s4_masking_definition <- function(workspace, document, point, token, name, direct) {
    uri <- document$uri
    parsed <- workspace$get_parse_data(uri)
    syntax <- parsed$syntax_data
    row <- s4_definition_binding_row(parsed, token, name)
    binding <- NULL
    if (!is.null(row)) {
        binding <- location(uri, navigation_range(document, syntax, row))
        parent <- syntax$parent_row[[row]]
        while (parent) {
            if (syntax$function_close[[parent]]) return(binding)
            parent <- syntax$parent_row[[parent]]
        }
    } else if (is.function(workspace$get_namespace)) {
        scope <- workspace$get_namespace(WORKSPACE, uri = uri)
        if (!is.null(scope)) binding <- scope$get_definition(name)
        if (identical(binding$uri, uri)) binding <- NULL
    }

    index <- workspace$index
    if (is.null(binding) && (!isTRUE(index$enabled) || !is.function(index$has_definition) || !index$has_definition(name))) return(NULL)
    at <- document$to_lsp_position(point$row, point$col)
    data <- parsed$member_data
    sources <- if (isTRUE(index$enabled)) data$sources
    open <- character()
    if (!is.null(workspace$documents) && (length(sources) || (!is.null(binding) && !identical(binding$uri, uri)))) {
        open <- workspace_s4_open_uris(workspace, uri, workspace_document_uris(workspace, uri))
    }
    source_binding <- function(key) {
        alias <- unname(open[key])
        source <- if (is.na(alias)) key else alias
        def <- workspace_s4_source_data(workspace, uri, source)$definitions[[name]]
        if (!is.null(def)) location(source, def$range)
    }
    if (!is.null(binding) && !identical(binding$uri, uri)) {
        key <- index_canonical_uri(binding$uri)
        if (key %in% names(open)) binding <- source_binding(key)
    }
    anchors <- list()
    sourced <- NULL
    # Top-level literal source calls order registrations in separate files.
    # Their physical end positions also distinguish repeated source calls.
    for (source in sources) {
        end <- data$items[[source$item]]$end
        if (source$bare && member_history_position(data$bindings$source, end)) next
        path <- index_resolve_source(source$spec, path_from_uri(uri), index$root)
        if (is.null(path)) next
        key <- index_canonical_uri(path_to_uri(path))
        pos <- document$to_lsp_position(end[[1L]], end[[2L]])
        if (!member_before(pos, at)) {
            if (is.null(anchors[[key]])) anchors[[key]] <- pos
            next
        }
        anchors[[key]] <- pos
        def <- source_binding(key)
        if (!is.null(def)) sourced <- def
    }
    event <- function(target) {
        if (identical(target$uri, uri)) target$range$end else anchors[[index_canonical_uri(target$uri)]]
    }
    if (!is.null(sourced)) {
        bound <- if (!is.null(binding)) event(binding)
        if (is.null(bound) || !member_before(bound, at) || member_before(bound, event(sourced))) binding <- sourced
    }
    if (is.null(binding) || !length(direct)) return(binding)

    # Prefer a visible S4 declaration when package files have no known order,
    # unless a reassignment in the same file explicitly masks the generic.
    same <- vapply(direct, function(target) identical(target$uri, binding$uri), logical(1L))
    masked <- any(vapply(direct[same], function(target) member_before(target$range$end, binding$range$start), logical(1L)))
    bound <- event(binding)
    for (i in seq_along(direct)) {
        target <- direct[[i]]
        registered <- event(target)
        if (!is.null(registered) && !member_before(registered, at)) next
        if (same[[i]]) {
            if (member_before(binding$range$end, target$range$start)) return(NULL)
        } else if (is.null(bound) || is.null(registered)) {
            if (!masked) return(NULL)
        } else if (!member_before(bound, at) || member_before(bound, registered)) {
            return(NULL)
        }
    }
    binding
}
