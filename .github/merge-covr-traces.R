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

merge_covr_traces <- function(trace_dir = default_covr_trace_dir()) {
    if (isTRUE(getOption("covr.record_tests", FALSE))) {
        return(invisible(character()))
    }
    if (!is.character(trace_dir) || length(trace_dir) != 1L ||
            is.na(trace_dir) || !nzchar(trace_dir) || !dir.exists(trace_dir)) {
        return(invisible(character()))
    }

    trace_files <- list.files(
        path = trace_dir,
        pattern = "^covr_trace_[^/]+$",
        full.names = TRUE
    )
    if (length(trace_files) <= 1L) {
        return(invisible(trace_files))
    }

    first <- suppressWarnings(readRDS(trace_files[[1L]]))
    if (is.environment(first)) {
        first_keys <- ls(first, all.names = TRUE)
        if (trace_has_recorded_tests(first, first_keys)) {
            return(invisible(trace_files))
        }
        acc <- if (is.null(env.profile(first))) {
            list2env(as.list(first, all.names = TRUE), hash = TRUE, parent = emptyenv())
        } else {
            first
        }
    } else if (is.list(first)) {
        first_keys <- names(first)
        if (trace_has_recorded_tests(first, first_keys)) {
            return(invisible(trace_files))
        }
        acc <- list2env(first, hash = TRUE, parent = emptyenv())
    } else {
        stop("Unexpected covr trace object in ", trace_files[[1L]], call. = FALSE)
    }

    for (path in trace_files[-1L]) {
        env <- suppressWarnings(readRDS(path))
        if (is.environment(env)) {
            if (exists("tests", envir = env, inherits = FALSE)) {
                return(invisible(trace_files))
            }
            keys <- ls(env, all.names = TRUE)
        } else if (is.list(env)) {
            if ("tests" %in% names(env)) {
                return(invisible(trace_files))
            }
            keys <- names(env)
        } else {
            stop("Unexpected covr trace object in ", path, call. = FALSE)
        }
        for (key in keys) {
            item <- env[[key]]
            if (!is.null(item$tests)) {
                return(invisible(trace_files))
            }
            cur <- acc[[key]]
            if (is.null(cur)) {
                acc[[key]] <- item
            } else {
                cur$value <- cur$value + item$value
                acc[[key]] <- cur
            }
        }
    }

    tmp_file <- tempfile(pattern = ".merge_covr_trace_", tmpdir = trace_dir, fileext = ".rds")
    on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)
    saveRDS(acc, file = tmp_file)
    out_file <- tempfile(pattern = "covr_trace_merged_", tmpdir = trace_dir)
    if (!isTRUE(file.rename(tmp_file, out_file))) {
        stop("Failed to atomically rename merged covr trace in ", trace_dir, call. = FALSE)
    }
    unlink(trace_files)
    invisible(out_file)
}

if (identical(Sys.getenv("R_COVR"), "true")) {
    merge_covr_traces()
} else if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    merge_covr_traces(if (length(args) >= 1L) args[[1L]] else default_covr_trace_dir())
}
