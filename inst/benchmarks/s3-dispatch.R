# Run against the installed revision selected by R_LIBS:
# Rscript inst/benchmarks/s3-dispatch.R results.csv 2000 datawizard,tibble
args <- commandArgs(trailingOnly = TRUE)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- if (length(args)) args[[1L]] else "s3-performance.csv"
benchmark$bindings <- if (length(args) > 1L) as.integer(args[[2L]]) else 2000L
benchmark$packages <- if (length(args) > 2L) strsplit(args[[3L]], ",", fixed = TRUE)[[1L]] else character()
stopifnot(!is.na(benchmark$bindings), benchmark$bindings >= 1L)
evalq({
    lsp_settings$set("index_persistent_cache", FALSE)
    lsp_settings$set("diagnostics", FALSE)
    startup <- system.time(workspace <- Workspace$new(NULL))[[3L]]
    preparation <- system.time(catalogs <- callr::r(package_call(s3_prepare_packages),
            args = list(packages), system_profile = FALSE, user_profile = FALSE))[[3L]]
    for (package in names(catalogs)) workspace$s3_metadata$set(package, catalogs[[package]])
    prefix <- sprintf("binding_%04d <- %d", seq_len(bindings), seq_len(bindings))
    rows <- list()
    measure <- function(run, iterations = 100L) {
        invisible(run())
        repeat {
            elapsed <- system.time(for (i in seq_len(iterations)) invisible(run()))[[3L]]
            if (elapsed >= 0.05 || iterations >= 6400L) break
            iterations <- iterations * 2L
        }
        samples <- vapply(seq_len(5L), function(i) {
            gc()
            1000 * system.time(for (j in seq_len(iterations)) invisible(run()))[[3L]] / iterations
        }, numeric(1L))
        c(median_ms = stats::median(samples), min_ms = min(samples), max_ms = max(samples))
    }
    workloads <- list(
        ordinary = list(lines = "sum(1, )", method = NULL),
        source = list(lines = c(
            "g <- function(x, ...) UseMethod('g')",
            "g.data.frame <- function(x, columns=NULL, ...) NULL", "g(data.frame(a=1), )"
        ), method = "g.data.frame"),
        dataset = list(lines = "summary(iris, )", method = "summary.data.frame")
    )
    if ("datawizard" %in% packages) workloads$rescale <- list(
        lines = "datawizard::rescale(iris, )", method = "rescale.data.frame"
    )
    uri <- "file:///s3-performance.R"
    for (name in names(workloads)) {
        workload <- workloads[[name]]
        content <- c(prefix, workload$lines)
        document <- Document$new(uri, content = content, version = 1L)
        workspace$documents$set(uri, document)
        parsed <- parse_document(uri, content)
        parsed$version <- 1L
        workspace$update_parse_data(uri, parsed)
        point <- list(row = length(content) - 1L, col = nchar(utils::tail(content, 1L)) - 1L)
        hover_point <- list(row = point$row, col = if (name == "rescale") 14L else 1L)
        lsp_settings$set("s3_dispatch", TRUE)
        result <- resolve_s3_call(uri, workspace, document, point)
        stopifnot(identical(result$method$name, workload$method))
        providers <- list(
            resolver_cold = function() {
                document$s3_calls$clear()
                resolve_s3_call(uri, workspace, document, point)
            },
            signature = function() signature_reply(1L, uri, workspace, document, point),
            hover = function() hover_reply(1L, uri, workspace, document, hover_point),
            completion = function() completion_reply(1L, uri, workspace, document, point, list())
        )
        for (enabled in if (name == "ordinary") c(FALSE, TRUE) else TRUE) {
            lsp_settings$set("s3_dispatch", enabled)
            for (provider in names(providers)) {
                times <- measure(providers[[provider]])
                rows[[length(rows) + 1L]] <- data.frame(
                    workload = name, s3_enabled = enabled, provider = provider,
                    median_ms = times[[1L]], min_ms = times[[2L]], max_ms = times[[3L]]
                )
            }
        }
    }
    results <- do.call(rbind, rows)
    utils::write.csv(results, output, row.names = FALSE)
    print(results, row.names = FALSE)
    cat("Workspace startup seconds:", startup, "\n")
    cat("Metadata worker seconds:", preparation, "\n")
    cat("Metadata namespaces:", length(workspace$s3_metadata$keys()), "\n")
    cat("Retained metadata cache bytes:", workspace$s3_metadata$bytes(), "\n")
}, benchmark)
