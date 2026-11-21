# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/code-actions.R results.csv 1000 3
# Measure synchronous provider replies after the worker's parse has been accepted.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else "code-actions.csv"
size <- if (length(args) >= 2L) as.integer(args[[2L]]) else 1000L
passes <- if (length(args) >= 3L) as.integer(args[[3L]]) else 3L
stopifnot(size > 0L, passes > 0L)
ns <- asNamespace("languageserver")
uri <- "file:///code-action-performance.R"
content <- unlist(lapply(seq_len(size), function(i) {
    c(sprintf("fun_%d <- function(x) {", i), "  y <- x + 1", "  y * 3", "}")
}), use.names = FALSE)
document <- ns$Document$new(uri, language = "r", version = 1L, content = content)
workspace <- ns$Workspace$new(NULL)
workspace$documents$set(uri, document)
parsed <- ns$parse_document(uri, content, include_xml = FALSE)
parsed$version <- document$version
workspace$update_parse_data(uri, parsed)
last <- length(content) - 3L
point <- function(row, col) list(row = row, col = col)
selection <- function(start, end = start) list(start = start, end = end)
request <- function(item_range) {
    ns$document_code_action_reply(1L, uri, workspace, document, item_range, list(diagnostics = list()))$result
}
fix_doc <- ns$Document$new(uri, content = rep("x=1", size))
single_line_doc <- ns$Document$new(uri, content = paste(rep("x=1", size), collapse = ";"))
diagnostic <- function(row, col) {
    list(source = "lintr", code = "assignment_linter",
        range = ns$range(ns$position(row, col), ns$position(row, col + 1L)))
}
offsets <- seq_len(size) - 1L
diagnostics <- lapply(offsets, diagnostic, col = 1L)
single_line_diagnostics <- lapply(4L * offsets + 1L, diagnostic, row = 0L)
fix_all <- function(doc, items) {
    context <- list(diagnostics = items, only = "source.fixAll")
    ns$document_code_action_reply(1L, uri, NULL, doc, list(), context)$result
}
operations <- list(
    cursor = function() request(selection(point(last, 7L))),
    expression = function() request(selection(point(last, 7L), point(last, 12L))),
    statements = function() request(selection(point(last, 2L), point(last + 1L, 7L))),
    fix_all = function() fix_all(fix_doc, diagnostics),
    fix_all_single_line = function() fix_all(single_line_doc, single_line_diagnostics)
)
expected_titles <- list(cursor = character(),
    expression = c("Extract expression to variable", "Extract selection to function"),
    statements = c("Extract selection to function", "Inline local variable `y`"),
    fix_all = "Fix all auto-fixable lintr problems", fix_all_single_line = "Fix all auto-fixable lintr problems")
rows <- list()
for (op in names(operations)) {
    for (pass in seq_len(passes)) {
        parsed$xml_doc <- NULL
        is_fix_all <- startsWith(op, "fix_all")
        for (state in if (is_fix_all) "first" else c("first", "repeat")) {
            gc()
            start <- proc.time()
            response <- operations[[op]]()
            elapsed <- proc.time() - start
            stopifnot(identical(vapply(response, `[[`, "", "title"), expected_titles[[op]]))
            if (is_fix_all) {
                edits <- response[[1L]]$edit$changes[[uri]]
                items <- if (op == "fix_all") diagnostics else single_line_diagnostics
                stopifnot(length(edits) == size, identical(response[[1L]]$diagnostics, items),
                    all(vapply(seq_along(edits), function(i) {
                        identical(edits[[i]], ns$text_edit(items[[i]]$range, " <- "))
                    }, logical(1L))))
            }
            rows[[length(rows) + 1L]] <- data.frame(operation = op, state = state, pass = pass,
                size = size, cpu_ms = 1000 * sum(elapsed[1:2]), elapsed_ms = 1000 * elapsed[[3L]])
        }
    }
}
results <- do.call(rbind, rows)
utils::write.csv(results, output, row.names = FALSE)
print(stats::aggregate(cbind(cpu_ms, elapsed_ms) ~ operation + state, results, stats::median), row.names = FALSE)
