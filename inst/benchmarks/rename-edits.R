# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/rename-edits.R results.csv 10000 3
# Isolate edit construction from reference resolution, parsing, and transport.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "rename-edits.csv"
size <- if (length(args) > 1L) as.integer(args[[2L]]) else 10000L
passes <- if (length(args) > 2L) as.integer(args[[3L]]) else 3L
stopifnot(!is.na(size), size > 0L, !is.na(passes), passes > 0L)
ns <- asNamespace("languageserver")
uri <- "file:///rename-benchmark.R"
refs <- lapply(seq_len(size), function(i) {
    ns$location(uri, ns$range(ns$position(i, 0L), ns$position(i, 5L)))
})
benchmark <- new.env(parent = ns)
benchmark$references_reply <- function(...) list(result = refs)
rename <- ns$rename_reply
environment(rename) <- benchmark
document <- ns$Document$new(uri, content = "value")
run <- function() rename(1L, uri, NULL, document, list(row = 0L, col = 2L), "renamed")$result
expected <- list(changes = stats::setNames(list(lapply(refs, function(ref) {
    ns$text_edit(ref$range, "renamed")
})), uri))
stopifnot(identical(run(), expected))
times <- replicate(passes, {
    gc()
    unname(system.time(invisible(run()))[["elapsed"]]) * 1000
})
allocation <- NA_real_
if (capabilities("profmem")) {
    path <- tempfile()
    Rprofmem(path)
    tryCatch(invisible(run()), finally = Rprofmem(NULL))
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path))))
    allocation <- sum(bytes, na.rm = TRUE)
    unlink(path)
}
results <- data.frame(edits = size, median_ms = stats::median(times),
    min_ms = min(times), max_ms = max(times), passes = passes, allocated_bytes = allocation)
utils::write.csv(results, output, row.names = FALSE)
print(results, row.names = FALSE)
