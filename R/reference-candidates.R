# Only a plain top-level assignment has a single name by which an ordinary
# call can resolve to this function's source range. Other formal owners use
# the complete workspace, including member and factory declarations.
reference_package_formal <- function(document, formal, name) {
    parsed <- document$parse_data
    if (isTRUE(parsed$parse_error) || document$is_rmarkdown) return(NULL)
    row <- member_navigation_token(parsed, document$from_lsp_position(formal$range$start))
    syntax <- parsed$syntax_data
    if (!row) return(NULL)
    fn <- syntax$parent_row[[row]]
    assignment <- syntax$parent_row[[fn]]
    if (!assignment || syntax$parent_row[[assignment]] != 0L) return(NULL)
    children <- syntax_children(syntax, assignment)
    children <- children[syntax$token[children] != "COMMENT"]
    if (length(children) != 3L || children[[3L]] != fn ||
            !syntax$text[[children[[2L]]]] %in% c("<-", "=")) return(NULL)
    lhs <- syntax_children(syntax, children[[1L]])
    if (length(lhs) != 1L || syntax$token[[lhs]] != "SYMBOL") return(NULL)
    callee <- syntax$text[[lhs]]
    if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", callee)) return(NULL)
    query <- new.env(parent = emptyenv())
    query$callee <- callee
    query$formal <- name
    query
}

index_buffer_reference_tags <- function(contents) {
    lapply(contents, function(content) {
        expr <- tryCatch(parse(text = content, keep.source = FALSE), error = function(e) NULL)
        index_reference_tags(expr)
    })
}

# Discovery queues full parses for all package sources. Fill gaps in the
# shallow index from those same buffers on its worker. Concurrent requests
# share a batch, and an edit invalidates only the changed document.
reference_package_tags <- function(server, workspace, docs, complete) {
    key <- paste0("reference-callers-", workspace$root)
    manager <- server$index_task_manager
    if (manager$has(key)) return(FALSE)
    missing <- list()
    for (doc in docs) {
        if (!is.null(doc$reference_tags)) next
        tags <- if (identical(doc$content, doc$parsed_content) &&
                identical(doc$version, doc$parse_data$version)) doc$parse_data$reference_tags
        if (is.null(tags)) {
            summary <- workspace$index$summaries$get(doc$uri, NULL)
            if (!is.null(summary$reference_tags) && identical(summary$content_hash, get_content_hash(doc$content))) {
                tags <- summary$reference_tags
            }
        }
        if (!is.null(tags)) doc$reference_tags <- tags
        else missing[[length(missing) + 1L]] <- doc
    }
    if (!length(missing)) return(TRUE)
    if (complete) return(FALSE)
    # Bound the number of buffers and bytes transferred in one worker turn.
    # A larger file already has a full parse queued that will supply its tags.
    missing <- utils::head(missing, 256L)
    sizes <- vapply(missing, function(doc) as.numeric(utils::object.size(doc$content)), 0)
    missing <- missing[cumsum(sizes) <= 4 * 1024^2]
    if (!length(missing)) return(FALSE)
    contents <- lapply(missing, `[[`, "content")
    accept <- function(tags) {
        for (i in seq_along(missing)) {
            doc <- missing[[i]]
            if (identical(doc$content, contents[[i]])) doc$reference_tags <- tags[[i]]
        }
    }
    task <- create_task(package_call(index_buffer_reference_tags), list(contents = contents),
        callback = accept, error = function(e) accept(rep(list(FALSE), length(missing))), profiles = FALSE)
    manager$add_task(key, task)
    FALSE
}

reference_package_callers <- function(server, request, complete) {
    query <- request$formal_query
    index <- request$workspace_index
    if (isTRUE(query$fallback) || !isTRUE(index$full_started) || index$discovering || index$truncated) return(NULL)
    workspace <- request$workspace
    if (is.null(query$required)) {
        root <- index$package_root_for_uri(request$uri)
        if (is.null(root)) {
            query$fallback <- TRUE
            return(NULL)
        }
        uris <- index$package_source_uris(root)
        scope <- workspace$document_uris_for_references(request$uri)
        if (!request$uri %in% uris || !setequal(scope, uris)) return(NULL)
        docs <- lapply(scope, workspace$documents$get)
        if (any(vapply(docs, `[[`, logical(1L), "is_rmarkdown"))) {
            query$fallback <- TRUE
            return(NULL)
        }
        possible <- logical(length(docs))
        for (i in seq_along(docs)) {
            doc <- docs[[i]]
            # Keep both callers and competing definitions, including literal
            # assignment targets. Numeric escapes can encode an ASCII name.
            possible[[i]] <- any(grepl(query$callee, doc$content, fixed = TRUE)) ||
                any(grepl("\\\\([0-7]|[uxU])", doc$content))
        }
        query$required <- union(request$uri, scope[possible])
        query$uris <- scope
        query$docs <- docs
        query$contents <- lapply(docs, `[[`, "content")
    }
    ready <- TRUE
    for (uri in query$required) {
        doc <- workspace$documents$get(uri, NULL)
        if (is.null(doc$parse_data) || !identical(doc$version, doc$parse_data$version) ||
                !identical(doc$content, doc$parsed_content)) {
            if (is.function(server$parse_task_manager$promote)) server$parse_task_manager$promote(uri)
            ready <- FALSE
        }
    }
    if (!isTRUE(query$screened)) {
        if (!reference_package_tags(server, workspace, query$docs, complete)) return(NULL)
        for (doc in query$docs) {
            tags <- doc$reference_tags
            if (isFALSE(tags) || any(startsWith(query$formal, tags))) {
                query$fallback <- TRUE
                return(NULL)
            }
        }
        query$screened <- TRUE
    }
    if (!ready) return(NULL)
    # A file event or peer edit can arrive while a caller's parse is in flight.
    scope <- workspace$document_uris_for_references(request$uri)
    root <- index$package_root_for_uri(request$uri)
    if (is.null(root) || !identical(scope, query$uris) || !setequal(scope, index$package_source_uris(root)) ||
            !identical(query$docs, lapply(scope, workspace$documents$get)) ||
            !identical(query$contents, lapply(query$docs, `[[`, "content"))) {
        query$fallback <- TRUE
        return(NULL)
    }
    if (!complete && index$source_reads_pending(scope)) return(NULL)
    intersect(scope, query$required)
}
