# Run this same script against each installed revision in a fresh R session:
# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/partial-parse.R results.csv 250 5
# Each function contributes four lines. Worker times exclude IPC; request times
# use an accepted parse and warm provider caches. Output checks run outside timing.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "partial-parse.csv"
functions <- if (length(args) >= 2L) as.integer(args[[2L]]) else 250L
samples <- if (length(args) >= 3L) as.integer(args[[3L]]) else 5L
stopifnot(!is.na(functions), functions >= 6L, !is.na(samples), samples >= 1L)
ns <- asNamespace("languageserver")

function_names <- sprintf("fn_%04d", seq_len(functions))
headers <- paste0(function_names, " <- function(value) {")
blocks <- lapply(headers, function(header) {
    c(header, "  local <- value + 1L", "  sum(local, value)", "}")
})
middle <- (functions + 1L) %/% 2L
blocks[[middle]] <- append(blocks[[middle]], "", after = 2L)
script <- c(unlist(blocks, use.names = FALSE), "")
cells <- split(blocks, ((seq_along(blocks) - 1L) * 3L) %/% functions)
quarto <- unlist(lapply(cells, function(cell) {
    c("```{r}", unlist(cell, use.names = FALSE), "```", "")
}), use.names = FALSE)

measure <- function(run, iterations) {
    invisible(run())
    times <- vapply(seq_len(samples), function(sample) {
        gc()
        start <- proc.time()[[3L]]
        for (i in seq_len(iterations)) invisible(run())
        (proc.time()[[3L]] - start) * 1000 / iterations
    }, numeric(1L))
    data.frame(iterations, samples, median_ms = stats::median(times),
        min_ms = min(times), max_ms = max(times))
}

benchmark <- function(scenario) {
    literate <- scenario %in% c("quarto_valid", "quarto_middle")
    clean <- if (literate) quarto else script
    uri <- if (literate) "file:///benchmark.qmd" else "file:///benchmark.R"
    middle_gap <- match(headers[[middle]], clean) + 2L
    content <- clean
    if (scenario == "trailing_namespace") content[[length(content)]] <- "pkg::"
    if (scenario %in% c("middle_function", "quarto_middle")) content[[middle_gap]] <- "  pkg::"

    # The range starts below the middle gap and stops before either the next
    # fence or the trailing edit. Both revisions receive identical visible text.
    limit <- if (literate) which(clean == "```" & seq_along(clean) > middle_gap)[[1L]] - 1L else length(clean) - 1L
    viewport <- ns$range(ns$position(middle_gap, 0L), ns$position(min(middle_gap + 40L, limit), 0L))
    parse_worker <- function(text) ns$parse_document(uri, text, is_rmarkdown = literate, include_xml = FALSE)
    providers <- function(parsed, text) {
        doc <- ns$Document$new(uri, version = 1L, content = text)
        parsed$version <- doc$version
        doc$update_parse_data(parsed)
        workspace <- list(get_parse_data = function(...) parsed,
            get_definitions_for_uri = function(...) parsed$definitions)
        list(range = function() ns$semantic_tokens_range_reply(1L, uri, workspace, doc, viewport)$result$data,
            symbols = function() ns$document_symbol_reply(1L, uri, workspace, doc, list())$result)
    }
    function_symbols <- function(symbols) {
        names <- vapply(symbols, `[[`, character(1L), "name")
        unname(symbols[names %in% function_names])
    }
    reference <- parse_worker(clean)
    expected <- providers(reference, clean)
    expected_range <- expected$range()
    expected_symbols <- function_symbols(expected$symbols())
    stopifnot(setequal(names(reference$definitions), function_names),
        length(expected_range) > 0L, length(expected_symbols) == functions)

    parsed <- parse_worker(content)
    requests <- providers(parsed, content)
    actual_range <- requests$range()
    actual_symbols <- function_symbols(requests$symbols())
    row <- function(operation, timing, actual, expected, count) {
        data.frame(scenario, lines = length(content), operation, timing,
            actual_count = count(actual), expected_count = count(expected),
            matches_reference = identical(actual, expected))
    }
    rbind(
        row("parse_worker_definitions", measure(function() parse_worker(content), 1L),
            parsed$definitions, reference$definitions, length),
        row("semantic_range_warm", measure(requests$range, 100L),
            actual_range, expected_range, function(data) length(data) %/% 5L),
        row("document_symbols_warm", measure(requests$symbols, 100L),
            actual_symbols, expected_symbols, length)
    )
}

scenarios <- c("valid", "trailing_namespace", "middle_function", "quarto_valid", "quarto_middle")
results <- do.call(rbind, lapply(scenarios, benchmark))
utils::write.csv(results, output, row.names = FALSE)
print(results, row.names = FALSE)
