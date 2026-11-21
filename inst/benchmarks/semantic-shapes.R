# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/semantic-shapes.R
# Arguments: number of functions or SQL lines, number of timing samples.
args <- commandArgs(trailingOnly = TRUE)
size <- if (length(args)) as.integer(args[[1L]]) else 2000L
samples <- if (length(args) > 1L) as.integer(args[[2L]]) else 5L
stopifnot(size >= 2L, samples >= 1L)
ns <- asNamespace("languageserver")

measure <- function(workload, operation, run) {
    result <- run()
    timings <- replicate(samples, {
        gc()
        system.time(invisible(run()), gcFirst = FALSE)[["elapsed"]] * 1000
    })
    allocated <- NA_real_
    if (capabilities("profmem")) {
        log <- tempfile()
        on.exit(unlink(log))
        Rprofmem(log)
        tryCatch(invisible(run()), finally = Rprofmem(NULL))
        bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(log))))
        allocated <- sum(bytes, na.rm = TRUE)
    }
    data.frame(workload, operation, median_ms = median(timings),
        allocated_MiB = allocated / 1024^2,
        result_hash = digest::digest(result, algo = "xxhash64"))
}

ids <- seq_len(size)
inputs <- list(
    shared = sprintf("fn_%d <- function(value) value", ids),
    distinct = sprintf("fn_%d <- function(arg_%d) arg_%d", ids, ids, ids),
    sql = c(sprintf("input_%d <- %dL", ids, ids), 'query <- "SELECT id',
        rep("UNION ALL SELECT id FROM source_table", size), 'ORDER BY id"')
)
for (workload in names(inputs)) {
    content <- inputs[[workload]]
    data <- utils::getParseData(parse(text = content, keep.source = TRUE))
    full <- function() ns$parse_document("file:///fixture.R", content, include_xml = FALSE)$semantic_data
    tokens <- full()
    if (workload == "sql") {
        stopifnot(sum(tokens$types == ns$SemanticTokenTypes$string) == size + 2L)
        stage <- function() ns$semantic_parse_data(data, content)
        stopifnot(identical(stage(), tokens))
        operation <- "semantic tokens"
    } else {
        stage <- function() ns$formal_parameter_symbol_ids(data)
        body <- data$token == "SYMBOL" & data$text %in% data$text[data$token == "SYMBOL_FORMALS"]
        stopifnot(identical(stage(), data$id[body]),
            sum(tokens$types == ns$SemanticTokenTypes$parameter) == 2L * size)
        operation <- "classify parameters"
    }
    results <- rbind(measure(workload, operation, stage), measure(workload, "parse document", full))
    print(results, row.names = FALSE)
}

# Partial recovery can request just a few lines from one long literal.
content <- c('query <- "SELECT id', rep("FROM source_table", size * 100), 'ORDER BY id"')
data <- utils::getParseData(parse(text = content, keep.source = TRUE))
wanted <- as.integer(c(1L, length(content) %/% 2L, length(content) - 1L))
partial <- function() ns$semantic_parse_data(data, content, only_lines = wanted)
tokens <- partial()
stopifnot(identical(tokens$lines, wanted), all(tokens$cols == 0L),
    identical(tokens$lengths, nchar(content[wanted + 1L])),
    all(tokens$types == ns$SemanticTokenTypes$string))
print(measure("sql", "three lines from a long literal", partial), row.names = FALSE)
