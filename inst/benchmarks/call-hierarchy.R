# Select an installed version with R_LIBS and retain full responses for comparison:
# Rscript inst/benchmarks/call-hierarchy.R before.rds 1000
# Rscript inst/benchmarks/call-hierarchy.R after.rds 1000 before.rds
# The optional third argument verifies identical serialized response contents.
args <- commandArgs(trailingOnly = TRUE)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- if (length(args)) args[[1L]] else "call-hierarchy-performance.rds"
benchmark$blocks <- if (length(args) > 1L) as.integer(args[[2L]]) else 1000L
benchmark$previous <- if (length(args) > 2L) args[[3L]] else NULL
stopifnot(!is.na(benchmark$blocks), benchmark$blocks >= 1L)

evalq({
    lsp_settings$set("index_persistent_cache", FALSE)
    lsp_settings$set("diagnostics", FALSE)
    uri <- "file:///call-hierarchy-performance.R"
    content <- c("target <- function(x) x", unlist(lapply(seq_len(blocks), function(i) {
        c(sprintf("caller_%d <- function(x) {", i),
            "    target(x)", "    target(x)", "}")
    }), use.names = FALSE))
    document <- Document$new(uri, version = 1L, content = content)
    workspace <- Workspace$new(NULL)
    workspace$documents$set(uri, document)
    parsed <- parse_document(uri, content)
    parsed$version <- 1L
    workspace$update_parse_data(uri, parsed)
    item <- function(name) {
        definition <- parsed$definitions[[name]]
        list(name = name, uri = uri, range = definition$range,
            data = list(definitionKey = paste0("global:", name),
                definition = list(uri = uri, range = definition$range)))
    }
    incoming_item <- item("target")
    outgoing_item <- item(paste0("caller_", blocks))
    measure <- function(name, fun, iterations = 1L) {
        result <- fun()
        times <- vapply(seq_len(3L), function(i) {
            gc()
            start <- proc.time()[[3L]]
            for (iteration in seq_len(iterations)) invisible(fun())
            (proc.time()[[3L]] - start) * 1000 / iterations
        }, numeric(1L))
        cat(sprintf("%s %.3f ms (min %.3f, max %.3f), %d result groups\n",
                name, median(times), min(times), max(times), length(result)))
        list(result = result, times = times, iterations = iterations)
    }
    cat(sprintf("%d lines, %d callers, %d target calls\n",
            length(content), blocks, blocks * 2L))
    results <- list(
        incoming = measure("incoming", function() {
            indexed_incoming_calls(workspace, incoming_item)
        }),
        outgoing = measure("outgoing last function", function() {
            indexed_outgoing_calls(workspace, outgoing_item)
        }, 20L)
    )
    if (!is.null(previous)) {
        before <- readRDS(previous)
        for (provider in names(results)) {
            stopifnot(identical(before[[provider]]$result, results[[provider]]$result))
        }
        cat("Both providers return identical responses to the saved baseline.\n")
    }
    attr(results, "input") <- list(lines = length(content), callers = blocks, calls = blocks * 2L)
    saveRDS(results, output)
    cat(sprintf("R %s; %s; languageserver %s\n", getRversion(), R.version$platform,
            as.character(utils::packageVersion("languageserver"))))
}, benchmark)
