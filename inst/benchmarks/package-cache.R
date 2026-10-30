# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/package-cache.R results.csv 3 polars
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "package-cache-performance.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 3L
package <- if (length(args) > 2L) args[[3L]] else "polars"
stopifnot(!is.na(passes), passes > 0L)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$passes <- passes
benchmark$package <- package
evalq({
    run <- function() {
        directory <- tempfile("package-cache-benchmark-")
        dir.create(directory)
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        previous <- options(languageserver.package_cache_dir = FALSE)
        on.exit(options(previous), add = TRUE)
        measurements <- new.env(parent = emptyenv())
        measurements$rows <- list()
        measure <- function(mode, operation, n = passes) {
            for (pass in seq_len(n)) {
                gc()
                start <- proc.time()
                value <- operation()
                elapsed <- proc.time() - start
                stopifnot(is.list(value), is.null(value$error))
                measurements$rows[[length(measurements$rows) + 1L]] <- data.frame(package = package, mode = mode, pass = pass,
                    elapsed_ms = 1000 * elapsed[[3L]], cpu_ms = 1000 * sum(elapsed[1:2]),
                    bytes = as.numeric(utils::object.size(value)))
            }
        }
        # The first namespace load is labelled separately. Later rows compare
        # preparation, cache creation and hits in the same loaded process.
        measure("first_uncached", function() member_prepare_package(package), 1L)
        measure("uncached", function() member_prepare_package(package))
        options(languageserver.package_cache_dir = directory)
        measure("cache_creation", function() member_prepare_package(package), 1L)
        measure("warm_disk_hit", function() member_prepare_package(package))
        measure("namespace_validation", function() package_metadata_live_identity(package))
        results <- do.call(rbind, measurements$rows)
        utils::write.csv(results, output, row.names = FALSE)
        print(aggregate(cbind(elapsed_ms, cpu_ms) ~ mode, results, stats::median), row.names = FALSE)
        cat("R:", as.character(getRversion()), "Package:", as.character(utils::packageVersion(package)), "\n")
    }
    run()
}, benchmark)
