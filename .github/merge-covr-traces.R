default_covr_trace_dir <- function() {
    cov_dir <- Sys.getenv("COVERAGE_DIR", "")
    if (nzchar(cov_dir)) cov_dir else .libPaths()[1L]
}

trace_has_recorded_tests <- function(obj, keys) {
    if (is.environment(obj) && exists("tests", envir = obj, inherits = FALSE)) {
        return(TRUE)
    }
    if (is.list(obj) && "tests" %in% names(obj)) {
        return(TRUE)
    }
    for (key in keys) {
        if (!is.null(obj[[key]]$tests)) {
            return(TRUE)
        }
    }
    FALSE
}

read_covr_trace <- function(path, retries = 3L, delay = 0.2) {
    retries <- max(0L, as.integer(retries))
    last_err <- NULL
    for (attempt in seq.int(0L, retries)) {
        if (attempt > 0L) {
            Sys.sleep(delay)
        }
        if (!file.exists(path)) {
            return(NULL)
        }
        err <- NULL
        obj <- tryCatch(
            suppressWarnings(readRDS(path)),
            error = function(e) {
                err <<- e
                NULL
            }
        )
        if (is.null(err)) {
            if (!is.environment(obj) && !is.list(obj)) {
                stop("Unexpected covr trace object in ", path, call. = FALSE)
            }
            return(obj)
        }
        last_err <- err
    }
    if (!file.exists(path)) {
        return(NULL)
    }
    message(
        "Skipping unreadable covr trace ", basename(path), ": ",
        conditionMessage(last_err)
    )
    unlink(path)
    NULL
}

merge_covr_traces <- function(trace_dir = default_covr_trace_dir(),
    retries = 3L, delay = 0.2) {
    if (!is.character(trace_dir) || length(trace_dir) != 1L ||
            is.na(trace_dir) || !nzchar(trace_dir) || !dir.exists(trace_dir)) {
        return(invisible(character()))
    }

    trace_files <- list.files(
        path = trace_dir,
        pattern = "^covr_trace_[^/]+$",
        full.names = TRUE
    )
    if (length(trace_files) == 0L) {
        return(invisible(character()))
    }

    if (isTRUE(getOption("covr.record_tests", FALSE))) {
        for (path in trace_files) {
            read_covr_trace(path, retries = retries, delay = delay)
        }
        return(invisible(character()))
    }

    valid_mask <- logical(length(trace_files))
    has_recorded_tests <- FALSE
    acc <- NULL

    for (i in seq_along(trace_files)) {
        path <- trace_files[[i]]
        obj <- read_covr_trace(path, retries = retries, delay = delay)
        if (is.null(obj)) {
            next
        }
        valid_mask[[i]] <- TRUE
        if (has_recorded_tests) {
            next
        }
        keys <- if (is.environment(obj)) ls(obj, all.names = TRUE) else names(obj)
        if (trace_has_recorded_tests(obj, keys)) {
            has_recorded_tests <- TRUE
            acc <- NULL
            next
        }
        if (is.null(acc)) {
            if (is.environment(obj) && !is.null(env.profile(obj))) {
                acc <- obj
            } else {
                entries <- if (is.environment(obj)) {
                    as.list(obj, all.names = TRUE)
                } else {
                    obj
                }
                acc <- list2env(entries, hash = TRUE, parent = emptyenv())
            }
        } else {
            for (key in keys) {
                item <- obj[[key]]
                cur <- acc[[key]]
                if (is.null(cur)) {
                    acc[[key]] <- item
                } else {
                    cur$value <- cur$value + item$value
                    acc[[key]] <- cur
                }
            }
        }
    }

    valid_paths <- trace_files[valid_mask]
    if (has_recorded_tests || length(valid_paths) <= 1L) {
        return(invisible(valid_paths))
    }

    tmp_file <- tempfile(pattern = ".merge_covr_trace_", tmpdir = trace_dir, fileext = ".rds")
    on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)
    saveRDS(acc, file = tmp_file)
    out_file <- tempfile(pattern = "covr_trace_merged_", tmpdir = trace_dir)
    if (!isTRUE(file.rename(tmp_file, out_file))) {
        stop("Failed to atomically rename merged covr trace in ", trace_dir, call. = FALSE)
    }
    unlink(valid_paths)
    invisible(out_file)
}

if (identical(Sys.getenv("R_COVR"), "true")) {
    merge_covr_traces()
} else if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    merge_covr_traces(if (length(args) >= 1L) args[[1L]] else default_covr_trace_dir())
}
