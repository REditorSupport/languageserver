# The parse worker keeps ordinary lexical tokens. Only the semantic-token
# requests use this view, whose lifetime is tied to the accepted document.
semantic_interpolation_config <- function(parsed) {
    if (!length(parsed$glue_templates) && !length(lsp_settings$get("glue_highlight_functions"))) return(NULL)
    config <- glue_highlight_config()
    if (!config$enabled) return(NULL)
    if (!length(parsed$glue_templates) && !any(parsed$range_data$calls$name %in%
                c(config$callees, paste0("`", config$callees, "`")))) return(NULL)
    config
}

semantic_interpolation_context <- function(workspace, document, config) {
    if (!is.null(config)) list(config, workspace$s3_revision, workspace$index$revision, workspace$loaded_packages,
        document$loaded_packages, workspace$namespace_file_mt)
}

semantic_interpolation_view <- function(workspace, document, parsed, config) {
    if (is.null(config)) return(NULL)
    context <- semantic_interpolation_context(workspace, document, config)
    cache <- document$glue_semantic_cache
    if (is.null(cache) || !identical(cache$parsed, parsed) || !identical(cache$context, context)) {
        cache <- new.env(parent = emptyenv())
        cache$parsed <- parsed
        cache$context <- context
        cache$config <- config
        document$glue_semantic_cache <- cache
    }
    cache
}

# An interior range may begin in the literal text between two expressions.
# Split complete host string tokens on those lines before selecting the range,
# so a viewport request has exactly the same boundaries as the full reply.
semantic_interpolation_data <- function(parsed, view, workspace, document, request_range = NULL) {
    raw <- parsed$semantic_data
    full <- if (is.null(view)) raw else view$data
    if (!is.null(full)) return(if (is.null(request_range)) full else semantic_data_for_range(full, request_range))
    if (is.null(request_range)) {
        fragments <- glue_highlight_fragments(workspace, document, parsed, view$config)
        view$data <- semantic_glue_overlay(raw, fragments, document$content, workspace, document)
        view$window <- view$window_data <- NULL
        return(view$data)
    }
    first <- request_range$start$line
    last <- request_range$end$line - as.integer(request_range$end$character == 0L)
    if (last < first) return(empty_semantic_data())
    window <- c(first, last)
    if (!identical(view$window, window)) {
        fragments <- glue_highlight_fragments(workspace, document, parsed, view$config, lines = window)
        from <- if (length(fragments)) min(first, fragments[[1L]]$start$row) else first
        to <- if (length(fragments)) max(last, fragments[[length(fragments)]]$end$row) else last
        rows <- semantic_data_for_range(raw, range(position(from, 0L), position(to + 1L, 0L)))
        view$window_data <- semantic_glue_overlay(rows, fragments, document$content, workspace, document)
        view$window <- window
    }
    semantic_data_for_range(view$window_data, request_range)
}

semantic_interpolation_full <- function(parsed, workspace, document) {
    view <- semantic_interpolation_view(workspace, document, parsed, semantic_interpolation_config(parsed))
    id <- semantic_result_id(parsed)
    data <- semantic_interpolation_data(parsed, view, workspace, document)$encoded
    if (!is.null(id) && !is.null(view)) {
        if (is.null(view$result_id)) {
            view$result_id <- if (identical(data, parsed$semantic_data$encoded)) id else
                digest::digest(list(id, data), algo = "xxhash64")
        }
        id <- view$result_id
    }
    result <- list(data = data)
    if (!is.null(id)) result$resultId <- id
    if (!is.null(view)) document$last_glue_semantic_result <- result
    result
}

# Legacy XML snapshots have no physical expression map. Reuse an accepted
# source parse when present, or project the source only for a potential call.
semantic_legacy_interpolation <- function(workspace, document, parsed, request_range) {
    config <- glue_highlight_config()
    if (!config$enabled || !any(vapply(c("glue", config$callees), function(name) {
        any(grepl(name, document$content, fixed = TRUE))
    }, logical(1L)))) return(NULL)
    source <- document$parse_data
    if (is.null(source$semantic_data) || !identical(document$content, document$parsed_content)) {
        if (!is.null(parsed$version) && !identical(parsed$version, document$version)) return(NULL)
        source <- parse_document(document$uri, document$content, isTRUE(document$is_rmarkdown), include_xml = FALSE)
        document <- document$clone(deep = FALSE)
        source$version <- document$version
        document$update_parse_data(source)
    }
    view <- semantic_interpolation_view(workspace, document, source, semantic_interpolation_config(source))
    if (!is.null(view)) semantic_interpolation_data(source, view, workspace, document, request_range)
}
