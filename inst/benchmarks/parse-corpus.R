# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/parse-corpus.R /path/to/source/R [samples]
# Both installed revisions must parse the same directory of valid R sources.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 1:2)
samples <- if (length(args) == 2L) as.integer(args[[2L]]) else 5L
paths <- sort(list.files(args[[1L]], pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE))
stopifnot(length(paths) > 0L, samples > 0L)
ns <- asNamespace("languageserver")
inputs <- lapply(paths, ns$read_text_lines)
uris <- vapply(paths, ns$path_to_uri, character(1L))
parse_all <- function() {
    vapply(seq_along(inputs), function(i) {
        result <- ns$parse_document(uris[[i]], inputs[[i]], include_xml = FALSE)
        stopifnot(!result$parse_error)
        # Compare the token classifications, member flags and discovered names.
        facts <- list(result$semantic_data, result$member_data$packages,
            result$member_data$class_scope, result$member_data$symbol_scope,
            names(result$definitions))
        digest::digest(facts, algo = "xxhash64")
    }, character(1L))
}

expected <- parse_all()
times <- replicate(samples, {
    gc()
    system.time(stopifnot(identical(parse_all(), expected)), gcFirst = FALSE)
})
result <- data.frame(files = length(inputs), lines = sum(lengths(inputs)),
    median_cpu_s = median(times["user.self", ] + times["sys.self", ]),
    median_elapsed_s = median(times["elapsed", ]),
    result_hash = digest::digest(expected, algo = "xxhash64"))
print(result)
