# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-semantic.R results.csv 200,1000 5
# Measure the first range and first full request on separate fresh documents.
# Then rotate across 17 one-line viewports on the accepted full result.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "glue-semantic.csv"
sizes <- if (length(args) >= 2L) as.integer(strsplit(args[[2L]], ",", fixed = TRUE)[[1L]]) else c(200L, 1000L)
passes <- if (length(args) >= 3L) as.integer(args[[3L]]) else 5L
stopifnot(all(sizes > 0L), passes > 0L)
ns <- asNamespace("languageserver")
uri <- "file:///semantic-benchmark.R"

workload <- function(n, shape) {
    i <- seq_len(n)
    text <- sprintf("v%d <- sum(x%d) + %dL", i, i, i)
    at <- if (shape == "ordinary") integer() else if (startsWith(shape, "sparse")) which(i %% 25L == 0L) else i
    fields <- if (shape == "dense-symbol") rep("value", length(at)) else if (endsWith(shape, "same")) {
        rep("sum(value) + 1L", length(at))
    } else {
        sprintf("sum(x%d) + %dL", at, at)
    }
    if (shape == "dense-custom") {
        text[at] <- sprintf('v%d <- custom("tag", "Result {%s}")', at, fields)
    } else {
        callee <- if (shape %in% c("dense-bare", "dense-shadow")) "glue" else "glue::glue"
        text[at] <- sprintf('v%d <- %s("Result {%s}")', at, callee, fields)
    }
    if (shape == "dense-shadow") {
        text <- c('f <- function() { glue <- function(s) s; glue("{x1}") }', text)
    }
    text
}

measure_document <- function(text) {
    start <- as.numeric(Sys.time())
    data <- ns$parse_document(uri, text, include_xml = FALSE)
    parse_ms <- 1000 * (as.numeric(Sys.time()) - start)
    stopifnot(!data$parse_error)
    ws <- ns$Workspace$new(NULL)
    accept <- function() {
        doc <- ns$Document$new(uri, "r", 1L, text)
        data$version <- doc$version
        ws$documents$set(uri, doc)
        ws$update_parse_data(uri, data)
        doc
    }
    doc <- accept()
    last <- length(text) - 1L
    span <- ns$range(ns$position(last, 0L), ns$position(last + 1L, 0L))
    start <- as.numeric(Sys.time())
    first_range <- ns$semantic_tokens_range_reply(1L, uri, ws, doc, span)$result$data
    first_range_ms <- 1000 * (as.numeric(Sys.time()) - start)
    doc <- accept()
    start <- as.numeric(Sys.time())
    full <- ns$semantic_tokens_full_reply(1L, uri, ws, doc)$result
    full_ms <- 1000 * (as.numeric(Sys.time()) - start)
    stopifnot(length(full$data) > 0L, length(full$data) %% 5L == 0L)
    targets <- as.integer(seq(0L, length(text) - 1L, length.out = 17L))
    expected <- tabulate(cumsum(matrix(full$data, ncol = 5L, byrow = TRUE)[, 1L]) + 1L, nbins = length(text))
    spans <- lapply(targets, function(row) ns$range(ns$position(row, 0L), ns$position(row + 1L, 0L)))
    start <- as.numeric(Sys.time())
    ranges <- lapply(spans, function(span) ns$semantic_tokens_range_reply(1L, uri, ws, doc, span)$result$data)
    range_ms <- 1000 * (as.numeric(Sys.time()) - start) / length(spans)
    stopifnot(identical(first_range, ranges[[length(ranges)]]), all(lengths(ranges) == expected[targets + 1L] * 5L))
    c(parse_ms = parse_ms, first_full_ms = full_ms, first_range_ms = first_range_ms, warm_range_ms = range_ms, full_tokens = length(full$data) %/% 5L)
}

invisible(measure_document(workload(24L, "dense-unique")))
shapes <- c("ordinary", "sparse-same", "sparse-unique", "dense-symbol", "dense-same", "dense-unique",
    "dense-bare", "dense-shadow", "dense-custom")
cases <- expand.grid(lines = sizes, shape = shapes, stringsAsFactors = FALSE)
inputs <- Map(workload, cases$lines, cases$shape)
records <- vector("list", nrow(cases))
set.seed(521L)
for (pass in seq_len(passes)) {
    for (i in sample.int(nrow(cases))) {
        ns$lsp_settings$set("glue_highlight_functions", if (cases$shape[[i]] == "dense-custom") list(custom = 2L) else list())
        gc()
        records[[i]] <- rbind(records[[i]], measure_document(inputs[[i]]))
    }
}
result <- cbind(cases, passes = passes, do.call(rbind, lapply(records, function(values) apply(values, 2L, median))))
utils::write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
