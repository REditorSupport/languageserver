# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/cli-diagnostics.R results.csv 500 5000 3
# Measure conversion of one diagnostic batch, including one byte-index build.
# CLI execution, document parsing, transport, and editor rendering are excluded.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "cli-diagnostics.csv"
diagnostics <- if (length(args) > 1L) as.integer(args[[2L]]) else 500L
lines <- if (length(args) > 2L) as.integer(args[[3L]]) else 5000L
passes <- if (length(args) > 3L) as.integer(args[[4L]]) else 3L
stopifnot(!is.na(diagnostics), diagnostics > 0L, !is.na(lines), lines > 0L,
    !is.na(passes), passes > 0L)
ns <- asNamespace("languageserver")
content <- rep(paste0(strrep("x", 80L), "\U0001f600"), lines)
offsets <- rep(list(c(0L, 80L)), diagnostics)
convert <- if ("index" %in% names(formals(ns$backend_byte_range))) function() {
    index <- ns$backend_byte_index(content)
    lapply(offsets, function(offset) ns$backend_byte_range(content, offset, index))
} else function() lapply(offsets, function(offset) ns$backend_byte_range(content, offset))
expected <- rep(list(ns$range(ns$position(0L, 0L), ns$position(0L, 80L))), diagnostics)
stopifnot(identical(convert(), expected))
invisible(convert())
times <- vapply(seq_len(passes), function(pass) {
    gc()
    unname(system.time(invisible(convert()))[["elapsed"]]) * 1000
}, numeric(1L))
results <- data.frame(diagnostics = diagnostics, lines = lines,
    median_ms = stats::median(times), min_ms = min(times), max_ms = max(times), passes = passes)
utils::write.csv(results, output, row.names = FALSE)
print(results, row.names = FALSE)
