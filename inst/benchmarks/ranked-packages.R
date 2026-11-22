# R_LIBS=/path/to/installation Rscript --vanilla inst/benchmarks/ranked-packages.R source-root results.csv
# source-root contains one unpacked package per subdirectory, e.g. source-root/rlang/R/.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, dir.exists(args[[1L]]))
packages <- list.dirs(args[[1L]], recursive = FALSE, full.names = TRUE)
packages <- packages[file.exists(file.path(packages, "DESCRIPTION"))]
stopifnot(length(packages) > 0L)
ns <- asNamespace("languageserver")
invisible(ns$lsp_settings$set("index_persistent_cache", FALSE))
elapsed <- function() proc.time()[["elapsed"]]

# Enumerate unconditional literal bindings independently of the server.
# R parses right arrows as left assignments. A standalone brace contains
# statements, while a brace on the RHS can compute an arbitrary value.
binding_names <- function(expr, statement = TRUE) {
    if (!is.call(expr)) return(character())
    if (statement && identical(expr[[1L]], as.name("{"))) {
        return(unlist(lapply(as.list(expr)[-1L], binding_names), use.names = FALSE))
    }
    if (length(expr) != 3L || !(identical(expr[[1L]], as.name("<-")) ||
                identical(expr[[1L]], as.name("=")))) return(character())
    target <- expr[[2L]]
    if (missing(target) || !(is.symbol(target) || is.character(target) && length(target) == 1L)) {
        return(character())
    }
    c(as.character(target), binding_names(expr[[3L]], statement = FALSE))
}

probe_package <- function(root) {
    index <- ns$WorkspaceIndex$new(root)
    paths <- list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)
    if (!length(paths)) return(NULL)
    do.call(rbind, lapply(paths, function(path) {
        content <- ns$read_text_lines(path)
        expected <- unique(unlist(lapply(as.list(parse(text = content, keep.source = FALSE)),
                    binding_names), use.names = FALSE))
        started <- elapsed()
        summary <- index$update_path(path)
        indexed <- elapsed()
        parsed <- ns$parse_document(ns$path_to_uri(path), content, include_xml = FALSE)
        finished <- elapsed()
        stopifnot(!is.null(summary), !summary$parse_error, !parsed$parse_error)
        data.frame(package = basename(root), file = basename(path), expected = length(expected),
            missing_closed = paste(setdiff(expected, names(summary$definitions)), collapse = "|"),
            missing_open = paste(setdiff(expected, names(parsed$definitions)), collapse = "|"),
            shallow_ms = 1000 * (indexed - started), full_ms = 1000 * (finished - indexed))
    }))
}

results <- do.call(rbind, lapply(packages, probe_package))
utils::write.csv(results, args[[2L]], row.names = FALSE)
cat(sprintf("%d packages with R source, %d files, %d expected bindings\n",
        length(unique(results$package)), nrow(results), sum(results$expected)))
cat(sprintf("Files missing bindings when closed / open: %d / %d\n",
        sum(nzchar(results$missing_closed)), sum(nzchar(results$missing_open))))
