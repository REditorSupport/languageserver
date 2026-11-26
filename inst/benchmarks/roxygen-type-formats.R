# R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/roxygen-type-formats.R /path/to/unpacked/packages [samples]
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 1:2)
root <- normalizePath(args[[1L]])
samples <- if (length(args) == 2L) as.integer(args[[2L]]) else 5L
stopifnot(!is.na(samples), samples >= 1L)
ns <- asNamespace("languageserver")
paths <- unlist(lapply(list.dirs(root, recursive = FALSE), function(package) {
    list.files(file.path(package, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
}), use.names = FALSE)
stopifnot(length(paths) > 0L)
sources <- lapply(paths, readLines, warn = FALSE, encoding = "UTF-8")
names(sources) <- substring(paths, nchar(root) + 2L)

milliseconds <- function(run, repeats = 1L) {
    median(replicate(samples, {
        start <- Sys.time()
        for (i in seq_len(repeats)) run()
        1000 * as.numeric(difftime(Sys.time(), start, units = "secs")) / repeats
    }))
}

scan <- function() sum(vapply(sources, ns$roxygen_has_type_span, logical(1L)))
cat(sprintf("all candidate scans (%d files, %d candidates): %.2f ms\n", length(paths), scan(), milliseconds(scan)))
selected <- c("rlang/R/arg.R", "dplyr/R/across.R", "checkmate/R/AssertCollection.R", "glue/R/glue.R", "hms/R/parse.R",
    "shiny/R/test-server.R")
for (file in intersect(selected, names(sources))) {
    lines <- sources[[file]]
    parsed <- parse(text = lines, keep.source = TRUE)
    syntax <- ns$syntax_parse_data(utils::getParseData(parsed))
    uri <- ns$path_to_uri(file.path(root, file))
    index <- function() ns$member_roxygen_type_index(lines, syntax, uri = uri, parsed = parsed)
    sites <- attr(index(), "occurrences", exact = TRUE)
    cat(sprintf("%-23s  sites %3d  index %7.3f ms\n", file, NROW(sites), milliseconds(index, 10L)))
}

workspace <- ns$Workspace$new(NULL)
formats <- c(canonical = "(`Box`)", markdown = "[`Box`]", rd = "[\\code{Box}]", class = "An object of class [Box].")
if (!exists("roxygen_leading_types", ns, inherits = FALSE)) formats <- formats["canonical"]
for (label in names(formats)) {
    uri <- paste0("file:///", label, ".R")
    lines <- c('Box <- R6::R6Class("Box", public = list(read = function() self))',
        paste("#' @param value", formats[[label]]), "use <- function(value) value$read()")
    prepare <- function() {
        doc <- ns$Document$new(uri, language = "r", version = 1L, content = lines)
        data <- ns$parse_document(uri, lines)
        data$version <- 1L
        doc$update_parse_data(data)
        workspace$documents$set(uri, doc)
        doc
    }
    point <- list(row = 2L, col = nchar("use <- function(value) value$"))
    complete <- function(doc) ns$member_completion(uri, workspace, doc, point, TRUE, 100L)
    cold <- replicate(samples, {
        doc <- prepare()
        start <- Sys.time()
        items <- complete(doc)
        elapsed <- 1000 * as.numeric(difftime(Sys.time(), start, units = "secs"))
        stopifnot("read" %in% vapply(items, `[[`, "", "label"))
        elapsed
    })
    doc <- prepare()
    invisible(complete(doc))
    cat(sprintf("%-23s  cold %7.3f ms  warm %7.3f ms\n", label, median(cold), milliseconds(function() complete(doc), 10L)))
}
