#' Discover files and restore cached summaries outside the server event loop
#' @noRd
index_worker_discover <- function(key, root, settings = NULL, cache_file = NULL,
    batch_size = 20L) {
    state_name <- ".languageserver_workspace_discoveries"
    states <- get0(state_name, envir = globalenv(), inherits = FALSE)
    if (!is.environment(states) || !identical(states$.__class__, "ordered_dict")) {
        states <- collections::ordered_dict()
        assign(state_name, states, envir = globalenv())
    }
    state <- states$pop(key, NULL)
    if (!is.null(settings)) {
        for (name in names(settings)) lsp_settings$set(name, settings[[name]])
        lsp_settings$set("index_persistent_cache", !is.null(cache_file))
        index <- WorkspaceIndex$new(root, cache_path = cache_file)
        index$discover()
        state <- list(files = index$files$values(), summaries = index$summaries,
            position = 1L, truncated = index$truncated)
    }
    if (is.null(state)) return(list(restart = TRUE))
    last <- min(state$position + batch_size - 1L, length(state$files))
    files <- if (last >= state$position) state$files[seq.int(state$position, last)] else list()
    summaries <- lapply(files, function(file) state$summaries$get(file$uri, NULL))
    state$position <- last + 1L
    done <- state$position > length(state$files)
    # These are live cursors, not reusable cache entries. Evicting an
    # unfinished scan makes sessions with many roots restart indefinitely.
    if (!done) states$set(key, state)
    list(files = files, summaries = summaries, done = done,
        truncated = state$truncated, restart = FALSE)
}

#' Release worker state when a workspace index is detached
#' @noRd
index_worker_release <- function(key) {
    for (name in c(".languageserver_workspace_discoveries", ".languageserver_workspace_indexes")) {
        states <- get0(name, envir = globalenv(), inherits = FALSE)
        if (is.environment(states) && identical(states$.__class__, "ordered_dict") && states$has(key)) {
            states$remove(key)
        }
    }
    invisible(NULL)
}

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
    # Catalog synchronization can span batches too. Retain attached workspace
    # state until index_worker_release(), just like discovery cursors above.
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
            suppressWarnings(saveRDS(list(version = index_cache_version, root = root,
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
