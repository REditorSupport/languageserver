# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-preparation.R results.csv 3 polars
# This workload reads installed namespace metadata only; it executes no query.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "member-preparation-performance.csv"
passes <- if (length(args) > 1L) as.integer(args[[2L]]) else 3L
package <- if (length(args) > 2L) args[[3L]] else "polars"
stopifnot(!is.na(passes), passes > 0L)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- output
benchmark$passes <- passes
benchmark$package <- package
evalq({
    run <- function() {
        directory <- tempfile("member-preparation-benchmark-")
        dir.create(directory)
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        previous <- options(languageserver.package_cache_dir = FALSE)
        on.exit(options(previous), add = TRUE)
        measurements <- new.env(parent = emptyenv())
        measurements$rows <- list()
        measure <- function(stage, operation, validate = function(value) !is.null(value), n = passes) {
            for (pass in seq_len(n)) {
                gc()
                start <- proc.time()
                value <- operation()
                elapsed <- proc.time() - start
                stopifnot(validate(value))
                size_value <- if (is.environment(value) && identical(stage, "model_construction")) as.list(value) else value
                measurements$rows[[length(measurements$rows) + 1L]] <- data.frame(
                    package = package, stage = stage, pass = pass,
                    elapsed_ms = 1000 * elapsed[[3L]], cpu_ms = 1000 * sum(elapsed[1:2]),
                    bytes = as.numeric(utils::object.size(size_value)))
            }
            value
        }
        namespace <- measure("namespace_load", function() asNamespace(package), is.environment, 1L)
        identity <- measure("live_identity", function() package_metadata_live_identity(package), is.list)
        input <- measure("namespace_extraction", function() member_namespace_input(namespace), is.list)
        index <- measure("model_construction", function() member_package_index(input), is.environment)
        prepare_returns <- function() {
            context <- list2env(as.list(index), parent = emptyenv())
            context$cache <- new.env(parent = emptyenv())
            member_prepare_returns(context)
            list(method_results = context$method_results, generic_returns = context$generic_returns)
        }
        returns <- prepare_returns()
        measure("return_preparation", prepare_returns, function(value) identical(value, returns))
        index$identity <- identity
        snapshot <- measure("snapshot_freeze", function() member_index_freeze(index), is.list)
        if (identical(package, "polars")) {
            value <- member_infer(quote(pl$DataFrame(list(value = 1L))$lazy()$filter(pl$col("value") > 0L)),
                index, index$roots)
            stopifnot(identical(value$type, "polars_lazy_frame"))
        }
        measure("uncached_preparation", function() member_prepare_package(package), is.list)
        options(languageserver.package_cache_dir = directory)
        path <- package_metadata_cache_path("members", identity)
        measure("disk_write", function() {
            package_metadata_cache_write(path, identity, snapshot)
            stopifnot(file.exists(path))
            snapshot
        }, is.list)
        measure("disk_read", function() package_metadata_cache_read(path, identity),
            function(value) identical(value, snapshot))
        unlink(path)
        prepared <- measure("cache_creation", function() member_prepare_package(package), is.list, 1L)
        measure("warm_preparation", function() member_prepare_package(package),
            function(value) identical(value, prepared))
        results <- do.call(rbind, measurements$rows)
        utils::write.csv(results, output, row.names = FALSE)
        print(aggregate(cbind(elapsed_ms, cpu_ms) ~ stage, results, stats::median), row.names = FALSE)
        cat("R:", as.character(getRversion()), "Package:", as.character(utils::packageVersion(package)), "\n")
    }
    run()
}, benchmark)
