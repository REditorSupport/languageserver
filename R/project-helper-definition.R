# Go to Definition can return several candidates. Providers that need one
# binding identity (notably References and Rename) keep their lexical lookup.
project_helper_definition <- function(id, workspace, uri, symbol, package_callee = FALSE) {
    index <- workspace$index
    paths <- project_helper_paths(index$helper_rules, index$root, uri)
    if (is.function(workspace$workspace_for_uri)) {
        paths <- paths[vapply(names(paths), function(helper) {
            identical(workspace$workspace_for_uri(helper)$index, index)
        }, logical(1L))]
    }
    if (!length(paths)) return(NULL)
    # Glue's callee lookup already checked local and project functions. A
    # same-name non-function there does not shadow its package function.
    if (!package_callee) {
        ordinary <- workspace$get_namespace(WORKSPACE, uri = uri)$get_definition(symbol)
        if (!is.null(ordinary)) return(Response$new(id, ordinary))
    }

    # Use the editor's URI spelling even when it differs from the disk key.
    open <- workspace_s4_open_uris(workspace, uri, workspace$documents$keys())
    live <- unname(open[names(paths)])
    disk <- which(is.na(live))
    info <- file.info(paths[disk], extra_cols = FALSE)
    eligible <- !is.na(info$size) & !info$isdir & info$size <= index$max_file_bytes()
    keep <- !is.na(live)
    keep[disk] <- eligible
    helpers <- names(paths)[keep]
    live <- live[keep]
    summaries <- index$helper_summaries(uri, helpers, paths[disk[eligible]], info[eligible, , drop = FALSE])
    if (index$helpers_pending(helpers)) return(list(waiting_helpers = helpers))

    targets <- list()
    for (i in seq_along(helpers)) {
        helper <- helpers[[i]]
        if (!is.na(live[[i]])) {
            helper <- live[[i]]
            doc <- workspace$documents$get(helper)
            if (!identical(doc$version, doc$parse_data$version) ||
                    !identical(doc$content, doc$parsed_content)) next
            data <- doc$parse_data
        } else {
            data <- summaries[[helper]]
        }
        definition <- data$definitions[[symbol]]
        if (package_callee && !identical(definition$type, "function")) next
        if (!is.null(definition$range)) targets[[length(targets) + 1L]] <- location(helper, definition$range)
    }
    if (length(targets)) Response$new(id, if (length(targets) == 1L) targets[[1L]] else targets)
}
