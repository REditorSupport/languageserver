# Run from the repository root. Demo files are parsed, never sourced/evaluated.
if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(quiet = TRUE)
}
ns <- asNamespace("languageserver")
api <- function(name) get(name, ns, inherits = FALSE)
demo_dir <- "inst/demos/members"
if (!dir.exists(demo_dir)) demo_dir <- system.file("demos", "members", package = "languageserver")
stopifnot(nzchar(demo_dir))

metadata <- collections::dict()
package_docs <- collections::dict()
if (requireNamespace("polars", quietly = TRUE)) {
    metadata$set("polars", api("member_index_thaw")(api("member_prepare_package")("polars")))
    package_docs$set("polars", api("PackageNamespace")$new("polars"))
    cat("Polars version:", as.character(utils::packageVersion("polars")), "\n")
} else {
    cat("Polars not installed; skipping that demo.\n")
}

fixture <- function(file) {
    path <- file.path(demo_dir, file)
    content <- readLines(path, warn = FALSE)
    uri <- api("path_to_uri")(normalizePath(path))
    document <- api("Document")$new(uri, language = "r", version = 1L, content = content)
    parsed <- api("parse_document")(uri, content)
    parsed$version <- 1L
    document$update_parse_data(parsed)
    workspace <- list(member_metadata = metadata, get_documentation = function(topic, pkgname, ...) {
        package <- package_docs$get(pkgname, NULL)
        if (!is.null(package)) package$get_documentation(topic)
    })
    list(uri = uri, document = document, workspace = workspace)
}

locate <- function(f, text) {
    rows <- which(vapply(f$document$content, function(line) {
        !startsWith(trimws(line), "#") && grepl(text, line, fixed = TRUE)
    }, logical(1L)))
    stopifnot(length(rows) == 1L)
    row <- rows[[1L]] - 1L
    col <- regexpr(text, f$document$line0(row), fixed = TRUE)[[1L]] - 1L
    list(row = row, col = col + nchar(text))
}

complete <- function(f, text, expected) {
    point <- locate(f, text)
    items <- api("member_completion")(f$uri, f$workspace, f$document, point, TRUE, 500L)
    labels <- vapply(items, `[[`, character(1L), "label")
    stopifnot(all(expected %in% labels))
    cat("  ", text, " -> ", paste(expected, collapse = ", "), "\n", sep = "")
}

signature <- function(f, text, expected) {
    point <- locate(f, text)
    result <- api("signature_reply")(1L, f$uri, f$workspace, f$document, point)$result
    stopifnot(length(result$signatures) == 1L)
    observed <- result$signatures[[1L]]$label
    stopifnot(identical(observed, expected))
    # Hover on the member immediately before this opening parenthesis.
    point$col <- point$col - 2L
    hover <- api("hover_reply")(1L, f$uri, f$workspace, f$document, point)$result
    stopifnot(grepl(expected, hover$contents[[1L]], fixed = TRUE))
    cat("  Signature + hover: ", observed, "\n", sep = "")
}

field_hover <- function(f, text, expected) {
    point <- locate(f, text)
    point$col <- point$col - 1L
    hover <- api("hover_reply")(1L, f$uri, f$workspace, f$document, point)$result
    stopifnot(grepl(expected, hover$contents[[1L]], fixed = TRUE))
    cat("  Field hover: ", text, " -> ", expected, "\n", sep = "")
}

cat("\nList factory\n")
f <- fixture("02-list-factory.R")
complete(f, "response <- client$", c("config", "request"))
complete(f, "decoded <- response$", c("decode", "status"))
complete(f, "decoded$data$", c("endpoint", "path"))
complete(f, "client$config$", c("endpoint", "retries", "api key"))
signature(f, "client$request(", "request(path, timeout = 30)")
signature(f, "response$decode(", "decode(simplify = TRUE)")
field_hover(f, "response$status", "200L")

cat("\nFluent environment\n")
f <- fixture("03-fluent-environment.R")
complete(f, 'pipeline$filter("Sepal.Length > 5")$', c("collect", "filter", "limit", "source"))
complete(f, "output$metadata$", "cached")
signature(f, 'pipeline$filter("Sepal.Length > 5")$limit(', "limit(n = 10L)")
signature(f, "limited$collect(", 'collect(format = "list")')
field_hover(f, "output$metadata$cached", "FALSE")

cat("\nR6 inheritance\n")
f <- fixture("04-r6-inheritance.R")
complete(f, 'dataset$select(c("Species"))$log("selected")$', c("preview", "select", "source", "log", "expensive"))
complete(f, "preview$", c("data", "rows"))
signature(f, 'dataset$select(c("Species"))$log(', 'log(message, level = "info")')
signature(f, 'dataset$select(c("Species"))$log("selected")$preview(', "preview(n = 6L)")
field_hover(f, "preview$rows", "3L")

if (metadata$has("polars")) {
    cat("\nPolars\n")
    f <- fixture("01-polars.R")
    complete(f, "result <- grouped$", c("agg", "head"))
    complete(f, "result$", c("collect", "filter"))
    complete(f, 'pl$col("Species")$str$', "to_uppercase")
    signature(f, "q$group_by(", "group_by(..., .maintain_order = FALSE)")
    signature(f, 'pl$col("Species")$str$to_uppercase(', "to_uppercase()")
    signature(f, "series$sum(", "sum()")
    point <- locate(f, ".maintain_order")
    point$col <- point$col - 3L
    argument <- api("hover_reply")(1L, f$uri, f$workspace, f$document, point)$result
    stopifnot(any(grepl("`.maintain_order`", argument$contents, fixed = TRUE)))
    cat("  Argument hover: .maintain_order documentation\n")
}

cat("\nAll available demos passed. Demo code was not executed.\n")
