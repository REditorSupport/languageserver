#' Read shallow disk summaries and maintain an inert persistent cache in a worker
#' @noRd
index_worker_batch <- function(key, root, updates, requests, bootstrap = FALSE,
    cache_file = NULL, revision = NULL, time_budget_ms = 25,
    max_file_bytes = 2 * 1024^2) {
    state_name <- ".languageserver_workspace_indexes"
    states <- get0(state_name, envir = globalenv(), inherits = FALSE)
    if (!is.environment(states) || !identical(states$.__class__, "ordered_dict")) {
        states <- collections::ordered_dict()
        assign(state_name, states, envir = globalenv())
    }
    state <- states$pop(key, NULL)
    sync_needed <- is.null(state) && !isTRUE(bootstrap)
    if (is.null(state) || isTRUE(bootstrap)) {
        state <- new.env(hash = TRUE, parent = emptyenv())
    }
    states$set(key, state)
    while (states$size() > 16L) states$pop(states$keys()[[1L]])
    for (uri in names(updates)) {
        summary <- updates[[uri]]
        if (is.null(summary) || identical(summary$cacheable, FALSE)) {
            if (exists(uri, envir = state, inherits = FALSE)) rm(list = uri, envir = state)
        } else {
            assign(uri, summary, envir = state)
        }
    }
    summaries <- vector("list", length(requests))
    retry <- logical(length(requests))
    processed <- 0L
    started <- proc.time()[[3L]]
    for (i in seq_along(requests)) {
        request <- requests[[i]]
        processed <- i
        path <- request$path
        info <- file.info(path)
        if (nrow(info) && !isTRUE(info$isdir[[1L]]) &&
                !is.na(info$size[[1L]]) && info$size[[1L]] <= max_file_bytes) {
            content <- tryCatch(stringi::stri_read_lines(path), error = function(e) NULL)
            if (!is.null(content)) {
                summary <- index_shallow_summary(path, content, root, info)
                after <- file.info(path)
                if (nrow(after) &&
                        identical(as.numeric(after$size[[1L]]), summary$size) &&
                        identical(as.numeric(after$mtime[[1L]]), summary$mtime)) {
                    summaries[[i]] <- summary
                } else {
                    retry[[i]] <- TRUE
                }
            }
        }
        if ((proc.time()[[3L]] - started) * 1000 >= time_budget_ms) break
    }
    # Only accepted main-process updates enter the persistent catalog. Disk
    # results above remain tentative until their generations are checked.
    temp_file <- NULL
    if (!is.null(cache_file) && !sync_needed) {
        dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
        temp_file <- tempfile("index-", tmpdir = dirname(cache_file))
        saved <- tryCatch({
            suppressWarnings(saveRDS(list(version = 1L, root = root,
                        summaries = unname(as.list(state))), temp_file))
            TRUE
        }, error = function(e) FALSE)
        if (!saved) {
            unlink(temp_file)
            temp_file <- NULL
        }
    }
    list(summaries = summaries, retry = retry, processed = processed, temp_file = temp_file,
        revision = revision, sync_needed = sync_needed)
}
