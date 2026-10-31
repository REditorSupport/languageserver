# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-tasks.R results.csv 3 polars
# Fresh workers match member tasks: profiles are disabled and namespaces are
# not retained between requests. Measure startup and metadata transfer too.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "member-task-performance.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 3L
package <- if (length(args) > 2L) args[[3L]] else "polars"
stopifnot(!is.na(passes), passes > 0L)
libraries <- .libPaths()
directory <- tempfile("member-task-benchmark-")
dir.create(directory)
worker <- function(libraries, package, directory, known) {
    .libPaths(libraries)
    namespace <- asNamespace("languageserver")
    namespace$member_resolve_packages(package, libraries, prepare_s3 = FALSE,
        known_members = known, cache_dir = directory)
}
invoke <- function(directory, known = list()) {
    callr::r(worker,
        args = list(libraries = libraries, package = package, directory = directory, known = known),
        system_profile = FALSE, user_profile = FALSE)
}
run <- function() {
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    rows <- list()
    known <- list()
    for (mode in c("cold", "disk", "known")) {
        if (mode == "disk") {
            # Populate outside the warm timers in a separate fresh worker.
            seed <- invoke(directory)
            stopifnot(is.null(seed$members[[package]]$error), !is.null(seed$members[[package]]$identity))
            known[package] <- list(seed$members[[package]]$identity)
        }
        for (pass in seq_len(passes)) {
            cache <- if (mode == "cold") file.path(directory, paste0("cold-", pass)) else directory
            gc()
            started <- proc.time()[[3L]]
            result <- invoke(cache, if (mode == "known") known else list())
            elapsed <- proc.time()[[3L]] - started
            if (mode == "known") {
                stopifnot(length(result$members) == 0L)
            } else {
                snapshot <- result$members[[package]]
                stopifnot(is.null(snapshot$error), identical(snapshot$schema, 1L))
                if (package == "polars") {
                    namespace <- asNamespace("languageserver")
                    index <- namespace$member_index_thaw(snapshot)
                    value <- namespace$member_infer(quote(pl$DataFrame(list(reading = 1L))$lazy()), index, index$roots)
                    stopifnot(identical(value$type, "polars_lazy_frame"))
                }
            }
            rows[[length(rows) + 1L]] <- data.frame(mode = mode, package = package,
                pass = pass, elapsed_ms = 1000 * elapsed,
                bytes = as.numeric(utils::object.size(result)))
        }
    }
    results <- do.call(rbind, rows)
    utils::write.csv(results, output, row.names = FALSE)
    print(aggregate(elapsed_ms ~ mode, results, stats::median), row.names = FALSE)
}
run()
