# R_LIBS=/path/to/lib Rscript --vanilla inst/benchmarks/s7-provider-latency.R base|pr|fixed [samples]
# The base supplies the generic signature. The PR and the fix also supply the method's argument.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 1:2, args[[1L]] %in% c("base", "pr", "fixed"))
revision <- args[[1L]]
samples <- if (length(args) == 2L) as.integer(args[[2L]]) else 9L
stopifnot(samples >= 3L, as.character(packageVersion("S7")) == "1.0.0")
ns <- asNamespace("languageserver")
invisible(ns$lsp_settings$set("index_persistent_cache", FALSE))

open_document <- function(workspace, lines, name) {
    uri <- ns$path_to_uri(file.path(tempdir(), paste0(name, ".R")))
    doc <- ns$Document$new(uri, language = "r", version = 1L, content = lines)
    doc$did_open()
    doc$loaded_packages <- workspace$loaded_packages
    workspace$documents$set(uri, doc)
    accept_parse(workspace, doc)
    doc
}

accept_parse <- function(workspace, doc) {
    parsed <- ns$parse_document(doc$uri, doc$content, include_xml = FALSE)
    stopifnot(!isTRUE(parsed$parse_error))
    parsed$version <- doc$version
    workspace$update_parse_data(doc$uri, parsed)
}

measure <- function(label, request, check, prepare = function() NULL) {
    prepare()
    check(request())
    elapsed <- numeric(samples)
    for (i in seq_len(samples)) {
        prepare()
        start <- Sys.time()
        result <- request()
        elapsed[[i]] <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
        check(result)
    }
    cat(sprintf("%-42s %9.2f %9.2f\n", label, median(elapsed), unname(quantile(elapsed, 0.9))))
}

cat(sprintf("%-42s %9s %9s\n", "Workload (ms)", "median", "p90"))

prices <- ns$Workspace$new(NULL)
prices$load_packages("data.table")
prices$member_metadata$set("data.table", ns$member_prepare_package("data.table"))
prices$loaded_packages <- union(prices$startup_packages, "data.table")
lines <- c("library(data.table)", sprintf("prices[, col%03d := round(price, 2)]", seq_len(150L)))
price_hints <- function(label, lines) {
    doc <- open_document(prices, lines, "prices")
    first <- match("prices[, col001 := round(price, 2)]", lines) - 1L
    viewport <- ns$range(ns$position(first, 0L), ns$position(length(lines), 0L))
    measure(label, function() ns$inlay_hint_reply(1L, doc$uri, prices, doc, viewport)$result,
        function(hints) stopifnot(identical(vapply(hints, `[[`, "", "label"), rep("digits =", 150L))),
        function() doc$member_receivers$clear())
}
price_hints("150 data.table RHS inlay hints", lines)
prices$load_packages("S7")
s7 <- ns$member_prepare_package("S7")
prices$member_metadata$set("S7", s7)
prices$loaded_packages <- union(prices$loaded_packages, "S7")
price_hints("Same hints with S7 attached", c("library(S7)", lines))

workspace <- ns$Workspace$new(NULL)
workspace$load_packages("S7")
workspace$member_metadata$set("S7", s7)
workspace$loaded_packages <- union(workspace$startup_packages, "S7")
invisible(ns$lsp_settings$set("s3_dispatch", FALSE))
generic <- c("library(S7)", 'describe := new_generic("x")')
lines <- c(generic, "method(describe, class_double) <- function(x, extra = TRUE) NULL",
    "f <- function() {", sprintf('  v%03d <- "%s"', seq_len(180L), strrep("v", 64L)),
    "  describe(1, ex)", sprintf('  w%03d <- "%s"', seq_len(160L), strrep("w", 64L)), "}")
doc <- open_document(workspace, lines, "signature")
point <- list(row = match("  describe(1, ex)", lines) - 1L, col = nchar("  describe(1, ex"))
signature <- function() ns$signature_reply(1L, doc$uri, workspace, doc, point)$result$signatures[[1L]]$label
expected <- if (revision == "base") "describe(x, ...)" else "describe(x, extra = TRUE)"
measure("Signature inside a 340-statement body", signature, function(sig) stopifnot(identical(sig, expected)),
    function() doc$member_receivers$clear())

count <- 96L
lines <- c(generic, sprintf("C%d := new_class(properties = list(val = class_double))", seq_len(count)),
    sprintf("method(describe, C%d) <- function(x, ..., arg%03d = 1L) NULL", seq_len(count), seq_len(count)),
    "describe(C96(), )")
doc <- open_document(workspace, lines, "methods")
point <- list(row = length(lines) - 1L, col = nchar("describe(C96(), "))
expected <- if (revision == "base") "describe(x, ...)" else "describe(x, ..., arg096 = 1L)"
measure("First signature across 96 registrations", signature, function(sig) stopifnot(identical(sig, expected)),
    function() {
        doc$member_receivers$clear()
        if (!is.null(doc$member_binding_cache)) doc$member_binding_cache$clear()
    })
edit_call <- function() {
    # The preceding declarations are identical across actual buffer revisions.
    lines[[length(lines)]] <- if (doc$version %% 2L) "describe(C96(),  )" else "describe(C96(), )"
    doc$set_content(doc$version + 1L, lines)
    accept_parse(workspace, doc)
}
measure("Signature after editing a 96-method file", signature, function(sig) stopifnot(identical(sig, expected)), edit_call)

reads <- c("f <- function(obj) {", rep('  prop(obj, "field")', 1200L), "}")
doc <- open_document(workspace, reads, "properties")
measure("Index 1,200 quoted property reads", function() ns$member_navigation_syntax(doc$parse_data),
    function(index) stopifnot(sum(index$name == "field" & index$kind == "read") == 1200L),
    function() doc$parse_data$member_navigation_syntax <- NULL)
if (capabilities("profmem")) {
    doc$parse_data$member_navigation_syntax <- NULL
    path <- tempfile()
    utils::Rprofmem(path)
    tryCatch(invisible(ns$member_navigation_syntax(doc$parse_data)), finally = utils::Rprofmem(NULL))
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path))))
    unlink(path)
    cat(sprintf("Recorded allocations for property indexing: %.2f MiB\n", sum(bytes, na.rm = TRUE) / 1024^2))
}
