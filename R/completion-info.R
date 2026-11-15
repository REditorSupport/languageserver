# Enrich already-pruned symbols from syntax and cached metadata only. The caller
# controls inline display; the same facts also serve the selected-item detail.
completion_symbol_info <- function(uri, workspace, document, point, token, items,
    resolved = NULL, parse_data = document$parse_data) {
    state <- new.env(parent = emptyenv())
    state$context <- resolved
    state$ready <- is.list(resolved)
    source_context <- function() {
        if (state$ready) return(state$context)
        state$ready <- TRUE
        if (is.null(parse_data$member_data)) return(NULL)
        start <- token$range$start$col
        cursor <- list(operator = start + 1L, operator_row = point$row,
            start = start, end = token$range$end$col, accessor = NULL,
            before = substr(document$line0(point$row), 1L, start))
        state$context <- tryCatch(member_resolve_cursor(uri, workspace, document, point, cursor),
            error = function(e) NULL)
        state$context
    }
    matching_names <- completion_matching_names(items)
    loaded <- loadedNamespaces()
    for (i in seq_along(items)) {
        item <- items[[i]]
        type <- item$data$type
        if (is.null(type) || !type %in% c("function", "nonfunction", "scope", "lazydata")) next
        label <- matching_names[[i]]
        if (startsWith(label, "`")) label <- completion_source_name(label)
        package <- item$data$package
        signature <- item$data$signature
        tip <- item$data$tip
        if (is.null(signature) && is.null(tip)) {
            value <- NULL
            if (is.null(package) || package %in% c("", WORKSPACE)) {
                own_source <- (is.null(item$data$uri) || identical(item$data$uri, uri)) &&
                    (is.null(parse_data$version) || identical(parse_data$version, document$version))
                if (own_source) {
                    lexical <- source_context()
                    fields <- lexical$value$fields
                    history <- lexical$index$document_bindings[[label]]
                    visible <- label %in% c(names(fields), names(lexical$bindings)) ||
                        member_history_position(history, c(point$row, point$col)) > 0L
                    if (visible && !is.null(lexical$budget) && !isTRUE(lexical$budget$exhausted)) {
                        env <- c(fields, lexical$bindings[setdiff(names(lexical$bindings), names(fields))])
                        value <- if (label %in% names(fields)) {
                            member_completion_value(fields[[label]], lexical$index, lexical$budget)
                        } else {
                            member_infer(as.name(label), lexical$index, env, budget = lexical$budget)
                        }
                        signature <- member_symbol_info(label, value, value$function_key, lexical$index)$signature
                    }
                    if (is.null(value) && identical(type, "function") &&
                            !is.null(parse_data$navigation_data)) {
                        row <- navigation_definitions(parse_data, point$row + 1L, point$col + 1L,
                            label, mode = "signature", last = TRUE)
                        if (length(row) && row > 0L) signature <- navigation_signature(document, parse_data, row, label)
                    }
                }
                if (is.null(signature) && is.null(value) && identical(package, WORKSPACE)) {
                    ns <- if (is.function(workspace$get_namespace)) {
                        call_with_optional_uri(workspace$get_namespace, WORKSPACE, uri = uri)
                    } else {
                        workspace$global_env
                    }
                    if (!is.null(ns)) signature <- tryCatch(ns$get_signature(label), error = function(e) NULL)
                }
            } else {
                if (identical(type, "function") && package %in% loaded && !is.null(workspace$namespaces)) {
                    ns <- workspace$namespaces$get(package, NULL)
                    if (!is.null(ns)) signature <- tryCatch(ns$get_signature(label, exported_only = FALSE),
                        error = function(e) NULL)
                }
                metadata <- workspace$member_metadata
                if (is.function(metadata$catalog)) value <- metadata$catalog(package)$roots[[label]]
            }
            if (is.null(signature) && !is.null(value) &&
                    (!identical(type, "lazydata") || isTRUE(value$known_literal))) {
                tip <- member_completion_tip(value)
            }
        }
        if (is.null(signature) && is.null(tip)) next
        if (!is.null(signature)) signature <- trimws(signature, which = "right")
        item$data$signature <- signature
        item$data$tip <- tip
        summary <- if (!is.null(signature)) signature else paste0(tip$summary, " [", tip$certainty, "]")
        if (is.null(item$detail) || !startsWith(item$detail, summary)) {
            item$detail <- paste(c(summary, item$detail), collapse = " ")
        }
        items[[i]] <- completion_item_label_details(item, signature = signature, tip = tip)
    }
    items
}
