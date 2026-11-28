# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/roxygen-type-hints.R [samples]
args <- commandArgs(trailingOnly = TRUE)
samples <- if (length(args)) as.integer(args[[1L]]) else 5L
stopifnot(length(args) <= 1L, samples >= 1L)
ns <- asNamespace("languageserver")
measure <- function(label, run) {
    tryCatch({
        first <- system.time(run())[["elapsed"]]
        iterations <- if (first < 0.015) 8L else 1L
        elapsed <- replicate(samples, system.time(for (i in seq_len(iterations)) run())[["elapsed"]] / iterations)
        cat(sprintf("%-24s %9.2f ms\n", label, 1000 * median(elapsed)))
        TRUE
    }, error = function(e) {
        cat(label, conditionMessage(e), "\n")
        FALSE
    })
}

deep <- c("#' @param x (`Record`)", paste0("f <- function(x) {", paste(rep("x", 2000L), collapse = "+"), "}"))
ok <- measure("parse 2000 terms", function() {
    parsed <- ns$parse_document("file:///deep.R", deep, include_xml = FALSE)
    stopifnot(!parsed$parse_error, parsed$member_data$has_type_hints)
})

for (n in c(1000L, 2000L, 4000L, 8000L)) {
    lines <- c("#' @param x (`Record`)", "f <- function(x) {", rep("  foo(x)", n), "}")
    expr <- parse(text = lines, keep.source = TRUE)
    hints <- ns$member_roxygen_type_index(lines, utils::getParseData(expr))
    ok <- measure(paste("attach", n, "calls"), function() {
        annotated <- ns$member_attach_type_hints(expr, hints)
        stopifnot(identical(attr(annotated[[1L]][[3L]], "member_type_hints")$params$x, "Record"))
    }) && ok
}

slots <- sprintf("p%03d", 1:100)
source <- c(paste0("mk <- function() list(", paste(rep("1", 64L), collapse = ", "), ")"),
    paste0('methods::setClass("Fields", slots = c(', paste0(slots, ' = "list"', collapse = ", "),
        "), prototype = list(", paste0(slots, " = mk()", collapse = ", "), "))"),
    'methods::setClass("Left", contains = "Fields")', 'methods::setClass("Right", contains = "Fields")')
for (type in c("Left", "Left | Right")) {
    code <- c(source, sprintf("#' @param x (`%s`)", type), "f <- function(x) x@p001")
    uri <- "file:///slots.R"
    doc <- ns$Document$new(uri, version = 1L, content = code)
    parsed <- ns$parse_document(uri, code)
    parsed$version <- doc$version
    doc$update_parse_data(parsed)
    point <- list(row = length(code) - 1L, col = nchar(tail(code, 1L)))
    ok <- measure(paste("complete", type), function() {
        items <- ns$member_completion(uri, list(), doc, point, FALSE, 100L)
        stopifnot(identical(vapply(items, `[[`, "", "label"), "p001"))
    }) && ok
}
if (!ok) stop("A roxygen type-hint workload failed")
