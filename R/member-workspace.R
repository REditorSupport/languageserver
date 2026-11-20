# Cursor recovery may collect local statements from an unfinished scope.
# Other files can only replay items that match a verified top-level source
# range. Preserve their original IDs for cross-file Go to Definition.
member_complete_item_ids <- function(parsed) {
    items <- parsed$member_data$items
    roots <- parsed$partial_complete_roots
    if (is.null(roots)) {
        if (isTRUE(parsed$parse_error)) return(integer())
        return(seq_along(items))
    }
    if (!is.null(parsed$complete_member_items)) return(parsed$complete_member_items)
    syntax <- parsed$syntax_data
    spans <- paste(syntax$line1[roots] - 1L, syntax$col1[roots] - 1L,
        syntax$line2[roots] - 1L, syntax$col2[roots])
    item_spans <- vapply(items, function(item) {
        paste(item$start[[1L]], item$start[[2L]], item$end[[1L]], item$end[[2L]])
    }, character(1L))
    ids <- which(item_spans %in% spans)
    if (is.environment(parsed)) parsed$complete_member_items <- ids
    ids
}

# Splice top-level literal source() calls into binding history in execution
# order. A sourced expression keeps its own declaration URI and range. Only
# its ordering coordinates change, so a later local write can use that value.
member_workspace_view <- function(workspace, document) {
    data <- document$parse_data$member_data
    if (!length(data$sources) || !isTRUE(workspace$index$enabled)) return(NULL)
    key <- list(parse = workspace$s3_revision, index = workspace$index$revision,
        documents = workspace$documents$size(), content = data$content_key)
    cached <- document$member_workspace_view
    if (identical(key, cached$key)) return(cached)

    items <- collections::queue()
    imports <- collections::queue()
    state <- new.env(parent = emptyenv())
    state$shadowed <- FALSE
    state$expansions <- state$statements <- 0L
    expand <- function(doc, trail, anchor = NULL) {
        data <- doc$parse_data$member_data
        # Bound the unfold when a script repeatedly sources other scripts.
        state$statements <- state$statements + length(data$items)
        if (state$statements > 20000L || state$expansions > 64L) return(FALSE)
        item_ids <- seq_along(data$items)
        sources <- data$sources
        source_items <- vapply(sources, `[[`, integer(1L), "item")
        source_bindings <- data$bindings$source
        if (!is.null(anchor) && !is.null(doc$parse_data$partial_complete_roots)) {
            item_ids <- member_complete_item_ids(doc$parse_data)
            keep <- source_items %in% item_ids
            sources <- sources[keep]
            source_items <- source_items[keep]
            if (length(source_bindings) && length(item_ids) != length(data$items)) {
                source_bindings <- member_index_bindings(data$items[item_ids])$source
            }
        }
        next_source <- 1L
        for (i in item_ids) {
            item <- data$items[[i]]
            end <- item$end
            if (!is.null(anchor)) {
                item$declaration <- list(uri = doc$uri, item = i)
                step <- 2L * items$size()
                item$start <- c(anchor, step)
                item$end <- c(anchor, step + 1L)
                item$cache_prefix <- NULL
            }
            items$push(item)
            if (member_head(item$expr, "library") || member_head(item$expr, "require")) {
                imports$push(item)
            }
            if (next_source > length(source_items) || i != source_items[[next_source]]) next
            source <- sources[[next_source]]
            next_source <- next_source + 1L
            if (member_history_position(source_bindings, end)) state$shadowed <- TRUE
            if (source$bare && state$shadowed) next
            paths <- index_source_candidates(source$spec, path_from_uri(doc$uri), workspace$index$root)
            if (!length(paths)) next
            targets <- vapply(paths, path_to_uri, character(1L), USE.NAMES = FALSE)
            available <- (file.exists(paths) & !dir.exists(paths)) | vapply(targets, function(uri) {
                isTRUE(workspace$documents$get(uri, NULL)$is_open)
            }, logical(1L))
            if (!any(available)) next
            uri <- targets[[which(available)[[1L]]]]
            if (uri %in% trail) return(FALSE)
            target <- workspace$documents$get(uri, NULL)
            if (is.null(target) || !identical(target$version, target$parse_data$version) ||
                    (isTRUE(target$parse_data$parse_error) && is.null(target$parse_data$partial_complete_roots))) next
            state$expansions <- state$expansions + 1L
            if (!expand(target, c(trail, uri), if (is.null(anchor)) end else anchor)) return(FALSE)
        }
        if (length(source_bindings)) state$shadowed <- TRUE
        TRUE
    }
    view <- list(key = key)
    if (expand(document, document$uri) && state$expansions) {
        view$bindings <- member_index_bindings(items$as_list())
        view$imports <- imports$as_list()
    }
    document$member_workspace_view <- view
    view
}
