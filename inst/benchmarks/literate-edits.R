# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/literate-edits.R results.csv 100 10 5
# Time synchronous content updates on a Quarto document. This includes region
# detection and excludes background parsing, JSON transport and rendering.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "literate-edits.csv"
cells <- if (length(args) > 1L) as.integer(args[[2L]]) else 100L
edits <- if (length(args) > 2L) as.integer(args[[3L]]) else 10L
passes <- if (length(args) > 3L) as.integer(args[[4L]]) else 5L
stopifnot(cells > 0L, edits > 0L, passes > 0L)

block <- c("# Results", paste("Discussion point", seq_len(34L)),
    "```{r}", paste0("x", seq_len(6L), " <- plot(mtcars)"), "``` ",
    paste("Additional discussion point", seq_len(8L)))
content <- rep(block, cells)
document <- languageserver:::Document$new("file:///report.qmd", version = 1L, content = content)
expected_regions <- document$regions
stopifnot(length(expected_regions$cells) == cells,
    sum(expected_regions$line_type == "r") == cells * 6L)

measure <- function(operation, row, col, alternatives) {
    range <- list(start = list(line = row, character = col), end = list(line = row, character = col + 1L))
    changed <- list(list(range = range))
    results <- lapply(seq_len(passes), function(pass) {
        first_version <- document$version
        gc()
        started <- proc.time()
        for (edit in seq_len(edits)) {
            changed[[1L]]$text <- alternatives[[1L + (document$version %% 2L)]]
            document$apply_content_changes(document$version + 1L, changed)
        }
        elapsed <- proc.time() - started
        stopifnot(identical(document$regions, expected_regions),
            identical(substr(document$content[[row + 1L]], col + 1L, col + 1L), changed[[1L]]$text),
            document$version == first_version + edits)
        data.frame(operation = operation, pass = pass, cells = cells, lines = length(content), edits = edits,
            elapsed_ms_per_edit = elapsed[[3L]] * 1000 / edits,
            cpu_ms_per_edit = sum(elapsed[1:2]) * 1000 / edits)
    })
    do.call(rbind, results)
}
results <- rbind(
    measure("prose", length(content) - 1L, 0L, c("A", "a")),
    measure("heading", max(which(content == "# Results")) - 1L, 2L, c("R", "r")),
    measure("closing fence", max(which(content == "``` ")) - 1L, 3L, c(" ", "\t"))
)
utils::write.csv(results, output, row.names = FALSE)
print(results, row.names = FALSE)
