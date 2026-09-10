# Compare installed revisions with R_LIBS=/path/to/library Rscript
# inst/benchmarks/range-providers.R results.csv 1000
args <- commandArgs(trailingOnly = TRUE)
benchmark <- new.env(parent = asNamespace("languageserver"))
benchmark$output <- if (length(args)) args[[1L]] else "range-provider-performance.csv"
benchmark$blocks <- if (length(args) > 1L) as.integer(args[[2L]]) else 1000L
evalq({
    stopifnot(!is.na(blocks), blocks >= 10L)
    fixture <- function(content) {
        uri <- "file:///range-performance.R"
        document <- Document$new(uri, version = 1L, content = content)
        parsed <- parse_document(uri, content)
        parsed$version <- 1L
        parsed$xml_doc <- xml2::read_xml(parsed$xml_data)
        attr(parsed$xml_doc, "top_level_index") <- xdoc_top_level_index(parsed$xml_doc)
        document$update_parse_data(parsed)
        workspace <- new.env(parent = emptyenv())
        workspace$get_parse_data <- function(...) document$parse_data
        workspace$get_formals <- function(...) formals(function(alpha, beta) NULL)
        list(uri = uri, workspace = workspace, document = document)
    }
    content <- unlist(lapply(seq_len(blocks), function(i) {
        c(
            sprintf("fn_%d <- function(alpha, beta) {", i),
            "  answer <- target(alpha + 1, beta + 2)",
            "  answer", "}"
        )
    }), use.names = FALSE)
    source <- fixture(content)
    parsed <- utils::getParseData(parse(text = content, keep.source = TRUE))
    viewport <- range(position(length(content) - 40L, 0L), position(length(content), 0L))
    measurements <- new.env(parent = emptyenv())
    measurements$rows <- list()
    measure <- function(operation, run, iterations = 20L, samples = 5L) {
        invisible(run())
        times <- vapply(seq_len(samples), function(sample) {
            gc()
            start <- proc.time()[[3L]]
            for (i in seq_len(iterations)) invisible(run())
            (proc.time()[[3L]] - start) * 1000 / iterations
        }, numeric(1L))
        measurements$rows[[length(measurements$rows) + 1L]] <- data.frame(
            operation = operation, lines = length(content),
            median_ms = median(times), min_ms = min(times), max_ms = max(times)
        )
        cat(sprintf("%-38s %10.4f ms\n", operation, median(times)))
    }
    measure("semantic_assignment_index", function() function_assignment_symbol_ids(parsed), 2L, 3L)
    semantic <- source$document$parse_data$semantic_data
    measure("semantic_40_line_viewport", function() semantic_data_for_range(semantic, viewport), 500L)
    previous <- rep(c(1L, 0L, 1L, 8L, 0L), 50000L)
    current <- previous
    current[[length(current) - 2L]] <- 2L
    measure("semantic_delta_50000_tokens", function() semantic_token_delta(previous, current), 20L)
    measure("inlay_40_line_viewport", function() {
        inlay_hint_reply(1L, source$uri, source$workspace, source$document, viewport)
    })
    measure("inline_40_line_viewport", function() {
        inline_value_reply(1L, source$uri, source$workspace, source$document, viewport)
    })
    measure("folding_warm", function() {
        document_folding_range_reply(1L, source$uri, source$workspace, source$document)
    }, 5L)

    documented <- fixture(unlist(lapply(seq_len(min(blocks, 100L)), function(i) {
        c("#' @param alpha input", sprintf("fn_%d <- function(alpha) alpha", i))
    }), use.names = FALSE))
    point <- position(length(documented$document$content) - 2L, 11L)
    measure("linked_editing_warm_100_functions", function() {
        linked_editing_range_reply(
            1L, documented$uri, documented$workspace, documented$document, point)
    }, 5L)
    links <- fixture(rep('source("nonexistent-range-benchmark.R")', blocks))
    measure("document_links_repeated_literals", function() {
        document_link_reply(1L, links$uri, links$workspace, links$document, tempdir())
    }, 5L)
    write.csv(do.call(rbind, measurements$rows), output, row.names = FALSE)
    cat(sprintf("R %s; %s; languageserver %s\n", getRversion(), R.version$platform,
            as.character(utils::packageVersion("languageserver"))))
}, benchmark)
