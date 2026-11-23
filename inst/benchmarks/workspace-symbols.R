# R_LIBS=/path/to/installation Rscript --vanilla inst/benchmarks/workspace-symbols.R source-root/ggplot2 geom_point
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
ns <- asNamespace("languageserver")
workspace <- ns$Workspace$new(args[[1L]])
paths <- list.files(file.path(args[[1L]], "R"), pattern = "[.][Rr]$", full.names = TRUE)
stopifnot(length(paths) > 0L)
for (path in paths) {
    workspace$index$update_path(path)
    uri <- ns$path_to_uri(path)
    workspace$documents$set(uri, ns$Document$new(uri, version = 1L, content = ns$read_text_lines(path)))
}

query <- function() workspace$get_definitions_for_query(args[[2L]])
expected <- query()
stopifnot(args[[2L]] %in% vapply(expected, `[[`, character(1L), "name"))
times <- replicate(7L, {
    gc()
    system.time(for (i in seq_len(10L)) stopifnot(identical(query(), expected)), gcFirst = FALSE)[["elapsed"]] * 100
})
print(data.frame(files = length(paths), matches = length(expected), median_ms = stats::median(times)))
