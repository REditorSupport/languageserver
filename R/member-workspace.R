member_item_spans <- function(items) {
    positions <- vapply(items, function(item) c(item$start, item$end), numeric(4L))
    # A legacy double -0 denotes the same physical offset as integer 0.
    positions <- positions + 0
    sprintf("%.17g %.17g %.17g %.17g", positions[1L, ], positions[2L, ], positions[3L, ], positions[4L, ])
}

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
    ids <- .Call("member_complete_items_c", items, syntax, roots, PACKAGE = "languageserver")
    if (is.null(ids)) {
        spans <- paste(syntax$line1[roots] - 1L, syntax$col1[roots] - 1L,
            syntax$line2[roots] - 1L, syntax$col2[roots])
        ids <- which(member_item_spans(items) %in% spans)
    }
    if (is.environment(parsed)) parsed$complete_member_items <- ids
    ids
}

# Splice top-level literal source() calls into binding history in execution
# order. A sourced expression keeps its own declaration URI and range. Only
# its ordering coordinates change, so a later local write can use that value.
member_workspace_view <- function(workspace, document, positions = FALSE) {
    data <- document$parse_data$member_data
    if (!length(data$sources) || !isTRUE(workspace$index$enabled)) return(NULL)
    key <- list(parse = workspace$s3_revision, index = workspace$index$revision,
        documents = workspace$documents$size(), content = data$content_key)
    cached <- document$member_workspace_view
    if (identical(key, cached$key) && (!positions || !is.null(cached$positions))) return(cached)

    items <- collections::queue()
    imports <- collections::queue()
    state <- new.env(parent = emptyenv())
    state$shadowed <- FALSE
    state$expansions <- state$statements <- 0L
    if (positions) state$positions <- list()
    expand <- function(doc, trail, anchor = NULL) {
        data <- doc$parse_data$member_data
        if (isTRUE(data$accessor_registrations)) state$accessor_registrations <- TRUE
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
                # Only complete statements can shadow a later source(). These
                # records already carry their owner's exact physical range.
                verified <- member_item_spans(data$items[item_ids])
                source_bindings <- source_bindings[member_item_spans(source_bindings) %in% verified]
            }
        }
        next_source <- 1L
        if (positions && !is.null(anchor)) offsets <- rep(NA_integer_, length(data$items))
        for (i in item_ids) {
            item <- data$items[[i]]
            end <- item$end
            if (!is.null(anchor)) {
                item$declaration <- list(uri = doc$uri, item = i)
                step <- 2L * items$size()
                if (positions) offsets[[i]] <- step
                item$start <- c(anchor, step)
                item$end <- c(anchor, step + 1L)
                item$cache_prefix <- NULL
            }
            items$push(item)
            import <- member_attachment_item(item)
            if (!is.null(import)) imports$push(import)
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
                    (isTRUE(target$parse_data$parse_error) && is.null(target$parse_data$partial_complete_roots))) {
                if (positions) state$unavailable <- TRUE
                next
            }
            state$expansions <- state$expansions + 1L
            if (!expand(target, c(trail, uri), if (is.null(anchor)) end else anchor)) return(FALSE)
        }
        if (positions && !is.null(anchor)) {
            visits <- state$positions[[doc$uri]]
            state$positions[[doc$uri]] <- c(visits, list(list(anchor = anchor, offsets = offsets)))
        }
        if (length(source_bindings)) state$shadowed <- TRUE
        TRUE
    }
    view <- list(key = key)
    expanded <- expand(document, document$uri)
    if (expanded && state$expansions) {
        view$bindings <- member_index_bindings(items$as_list())
        view$imports <- imports$as_list()
        view$accessor_registrations <- state$accessor_registrations
    }
    if (positions) {
        view$positions <- state$positions
        view$positions_complete <- expanded && !isTRUE(state$unavailable)
    }
    document$member_workspace_view <- view
    view
}

# A package source file sees NAMESPACE imports without calling library().
# Collect this once per manifest revision. Ordinary scripts never consult
# the filesystem, and a parser worker never evaluates NAMESPACE conditions.
member_namespace_imports <- local({
    cache <- NULL
    function(uri, type_hints = FALSE) {
        if (length(uri) != 1L || !startsWith(uri, "file:///") ||
                !grepl("/R/[^/]+\\.[rR]$", uri)) return(NULL)
        root <- dirname(dirname(path_from_uri(uri)))
        paths <- file.path(root, c("DESCRIPTION", "NAMESPACE"))
        info <- file.info(paths, extra_cols = FALSE)
        if (anyNA(info$size) || any(info$isdir) || any(info$size > 1024^2)) return(NULL)
        if (is.null(cache)) cache <<- ByteLruCache$new(1024^2, max_entries = 64L)
        stamp <- info[c("size", "mtime", "ctime")]
        previous <- cache$get(root)
        field <- if (type_hints) "all_imports" else "imports"
        if (identical(stamp, previous$stamp)) return(previous[[field]])
        pkg <- tryCatch(read.dcf(paths[[1L]], fields = "Package"), error = function(e) NULL)
        imports <- directives <- NULL
        if (length(pkg) == 1L && !is.na(pkg[[1L]]) && nzchar(pkg[[1L]])) {
            directives <- extract_namespace_imports(paths[[2L]], unconditional = TRUE)$directives
            # S7 uses imports to identify its binding operator. Other sources
            # consult them when a documented class needs a package owner.
            packages <- unlist(lapply(directives, function(x) if (x$type == "import") x$packages else x$package), use.names = FALSE)
            imports <- if ("S7" %in% packages) directives else list()
            attr(imports, "package") <- pkg[[1L]]
            attr(directives, "package") <- pkg[[1L]]
        }
        entry <- list(stamp = stamp, imports = imports, all_imports = directives)
        cache$set(root, entry)
        entry[[field]]
    }
})

# Limit worker requests and request-time bindings to names used by this file.
member_import_bindings <- function(directives, symbols) {
    result <- list()
    for (item in directives) {
        if (identical(item$type, "import")) {
            names <- setdiff(symbols, item$except)
            for (pkg in item$packages) {
                if (length(names)) result[[length(result) + 1L]] <- list(package = pkg, names = names, targets = names)
            }
        } else if (identical(item$type, "importFrom")) {
            keep <- item$symbols %in% symbols
            if (any(keep)) result[[length(result) + 1L]] <- list(package = item$package,
                names = item$symbols[keep], targets = item$targets[keep])
        }
    }
    result
}

# Look up a package declaration in its own document so its parent,
# imports, source coordinates and lexical assignments all come from there.
# A duplicated binding, stale buffer or unverified declaration has no owner.
member_package_declaration <- function(name, index, budget, depth, trail) {
    workspace <- index$source_workspace
    uri <- index$source_uri
    if (is.null(workspace) || !workspace$index$has_definition(name, uri)) return(NULL)
    root <- workspace$index$package_root_for_uri(uri)
    if (is.null(root)) return(NULL)
    owner <- member_package_binding_owner(name, workspace, root, uri)
    if (is.null(owner)) return(member_value(reason = "uncertain_package_binding"))
    if (identical(owner, uri)) return(NULL)
    doc <- workspace$documents$get(owner)
    classes <- doc$parse_data$class_data$declarations
    records <- c(classes, attr(classes, "s7_bindings", exact = TRUE))
    binding <- doc$parse_data$member_data$bindings[[name]]
    fn <- if (length(binding)) binding[[length(binding)]]$expr
    # A documented return can travel through an ordinary wrapper or alias in
    # a third file. Resolve only the requested binding in its owning file.
    callable <- (member_head(fn, "function") || is.symbol(fn)) &&
        (isTRUE(doc$parse_data$member_data$has_type_hints) || workspace$index$package_has_type_hints(uri))
    if (!callable && !any(vapply(records, function(cls) {
        isTRUE(cls$binding) && (isTRUE(cls$named_bind) || cls$type %in% c("S7", "S7Generic", "S4", "R6", "RefClass")) &&
            identical(cls$name, name)
    }, logical(1L)))) return(member_value(reason = "uncertain_package_binding"))
    key <- paste(root, name, sep = "\n")
    peers <- budget$package_declarations
    if (key %in% peers || length(peers) >= 16L) return(member_value(reason = "recursive_package_binding"))
    budget$package_declarations <- c(peers, key)
    on.exit(budget$package_declarations <- peers)
    context <- member_context_index(workspace, owner, doc, c(Inf, Inf), referenced = name)
    if (!is.null(budget$origin_name)) {
        previous <- budget$document
        budget$document <- doc
        on.exit(budget$document <- previous, add = TRUE)
    }
    value <- member_resolve_document(name, context, list(), budget, depth + 1L, trail)
    if (callable && !is.null(value$function_expr) && is.null(value$function_uri)) value$function_uri <- owner
    value
}

member_package_binding_owner <- function(name, workspace, root, uri) {
    cache <- analysis_cache(workspace, "member-package-owners",
        c(workspace$s3_revision, workspace$index$revision, workspace$documents$size()))
    key <- paste(root, name, sep = "\n")
    hit <- analysis_cache_get(cache, key)
    if (hit$hit) return(hit$value)
    owner <- NULL
    open <- workspace_s4_open_uris(workspace, uri, workspace_document_uris(workspace, uri))
    for (peer in workspace$index$package_source_uris(root)) {
        summary <- workspace$index$summaries$get(peer, NULL)
        if (!name %in% names(summary$definitions)) next
        if (peer %in% names(open)) peer <- open[[peer]]
        doc <- workspace$documents$get(peer, NULL)
        data <- doc$parse_data
        if (!is.null(owner) || is.null(data) || !identical(data$version, doc$version) ||
                !identical(doc$content, doc$parsed_content)) {
            owner <- NULL
            break
        }
        owner <- peer
    }
    analysis_cache_set(cache, key, owner)
}

member_package_shadows <- function(index, name) {
    workspace <- index$source_workspace
    uri <- index$source_uri
    if (is.null(workspace) || !workspace$index$has_definition(name, uri)) return(FALSE)
    root <- workspace$index$package_root_for_uri(uri)
    if (is.null(root)) return(FALSE)
    !identical(member_package_binding_owner(name, workspace, root, uri), uri)
}
