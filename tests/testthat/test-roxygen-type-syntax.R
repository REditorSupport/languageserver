test_that("type hints use a leading code span with nominal alternatives", {
    hints <- roxygen_type_block(c(
        "#' @param x,y (`Pipeline | pkg::Frame | Pipeline`) Input.",
        "#' @param z", "#'   (`.Local`) Another input.",
        "#' @param prose An ordinary `Class` reference.",
        "#' @param call (`factory()`) Computed expression.",
        "#' @param empty (`Frame |`) Incomplete type.",
        "#' @returns (`pkg::Result`) Output."
    ))
    expect_identical(hints$params, list(x = c("Pipeline", "pkg::Frame"),
            y = c("Pipeline", "pkg::Frame"), z = ".Local"))
    expect_identical(hints$returns, "pkg::Result")
    expect_null(roxygen_type_block(c("#' @details (`Frame`)", "#' @param x `Class`")))
    expect_identical(roxygen_type_block("#' @param `a, b` (`Frame`)")$params, list(`a, b` = "Frame"))
    large <- paste0("Class", seq_len(max_type_hint_alternatives + 1L), collapse = " | ")
    expect_null(roxygen_type_block(sprintf("#' @param x (`%s`)", large)))
})

test_that("typed roxygen follows the function literal belonging to the declaration", {
    lines <- c("#' @param arg (`Outer`)", "#' @param typo (`Missing`)", "outer <-",
        "  function(arg) {", "    #' @return (`Inner`)",
        "    inner <- function(arg) arg; other <- function(arg) arg", "    inner(arg)", "  }",
        "methods <- list(", "  #' @param arg (`Method`)", "  m = function(arg) arg)",
        "#' @return (`Wrong`)", "factory <- list(f = function() NULL)",
        "#' @return (`Wrong`)", "computed <- (function() NULL)()",
        "#' @return (`Wrong`)", "", "separate <- function() NULL",
        "txt <- r\"(inside string", "#' @return (`Wrong`)", "unrelated <- function() NULL", ")\"",
        "#' @return (`pkg::Result`)", "nullary <- function() NULL")
    data <- parse_document("file:///roxygen-types.R", lines)$member_data
    hint <- function(fn) attr(fn, "member_type_hints", exact = TRUE)
    binding <- function(name) data$bindings[[name]][[1L]]$expr
    outer <- binding("outer")
    expect_identical(hint(outer)$params, list(arg = "Outer"))
    expect_identical(hint(outer[[3L]][[2L]][[3L]])$returns, "Inner")
    expect_null(hint(outer[[3L]][[3L]][[3L]]))
    expect_identical(hint(binding("methods")[[2L]])$params, list(arg = "Method"))
    expect_null(hint(binding("factory")[[2L]]))
    expect_null(hint(binding("computed")[[1L]][[2L]]))
    expect_null(hint(binding("separate")))
    expect_identical(hint(binding("nullary"))$returns, "pkg::Result")
    expect_identical(length(data$type_hints), 4L)
    expect_true(data$has_type_hints && data$symbol_scope)
    expect_true("pkg" %in% data$packages)
    expect_true(all(c("Outer", "Inner") %in% member_syntax_names(outer)))
    expect_false("Missing" %in% member_syntax_names(outer))
})

test_that("partial member parses retain the original function's annotation", {
    lines <- c("#' @param data (`Frame`)", "f <- function(data) {", "  data$", "}")
    fixture <- provider_fixture(lines)
    doc <- fixture$document
    expect_true(doc$parse_data$parse_error)
    point <- list(row = 2L, col = 7L)
    cursor <- member_cursor(doc, point)
    recovered <- member_recover_scope(doc, point, cursor, doc$parse_data$member_data)
    fn <- recovered$parsed[[1L]][[3L]]
    expect_identical(attr(fn, "member_type_hints", exact = TRUE)$params, list(data = "Frame"))

    # The fragment starts at the declaration or at the function keyword, so
    # its own parser can only see the attached block via the document index.
    for (column in c(0L, 5L)) {
        fragment <- c(substring(lines[[2L]], column + 1L), "  data$x", "}")
        syntax <- member_parse_recovered(fragment, NULL, 1L, column,
            type_hints = doc$parse_data$member_data$type_hints)[[1L]]
        fn <- if (column) syntax else syntax[[3L]]
        expect_identical(attr(fn, "member_type_hints", exact = TRUE)$params, list(data = "Frame"))
    }
})

test_that("an unfinished annotated function keeps package hints within its R cell", {
    lines <- c("#' @param data (`pkg::Frame`)", "f <- function(data) data$")
    standalone <- parse_document("file:///typing.R", lines)$member_data
    expect_identical(standalone$type_hints[["2:6"]]$params, list(data = "pkg::Frame"))
    expect_identical(standalone$packages, "pkg")

    content <- c("```{r}", lines, "```", "```{r}", "#' @return (`wrong::Type`)", "```",
        "```{r}", "g <- function() NULL", "```", "```{r}", "#' @return (`Second`)", "h <- function() NULL", "```")
    for (ext in c("Rmd", "qmd")) {
        data <- parse_document(paste0("file:///cells.", ext), content, is_rmarkdown = TRUE)$member_data
        expect_identical(data$type_hints[["3:6"]]$params, list(data = "pkg::Frame"))
        expect_identical(attr(data$bindings$h[[1L]]$expr, "member_type_hints")$returns, "Second")
        expect_null(attr(data$bindings$g[[1L]]$expr, "member_type_hints"))
        expect_identical(data$packages, "pkg")
    }
})

test_that("ordinary code and Markdown in strings never request or supply types", {
    lines <- c("#' Plain documentation", "fun <- function() list(`odd name` = 1L)",
        "marker <- 'foo (`no annotation`)'", "#' @param x A plain description.")
    # Force the production fast path to avoid materializing parse data.
    expect_null(member_roxygen_type_index(lines, stop("unused syntax")))
    multiline <- c('text <- "open', "#' @return (`False`)\" # another comment", "f <- function() NULL")
    expect_false(parse_document("file:///string.R", multiline)$member_data$has_type_hints)
})

test_that("package sources resolve documented types from relevant NAMESPACE imports", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: hintfixture", file.path(root, "DESCRIPTION"))
    manifest <- file.path(root, "NAMESPACE")
    writeLines(c("importFrom(processx, process)", "importFrom(polars, pl)", "importFrom(unused, other)"), manifest)
    uri <- path_to_uri(file.path(root, "R", "inspect.R"))
    workspace <- Workspace$new(root)
    workspace$member_metadata$set("processx", member_prepare_package("processx"))
    frame <- member_generic_index("")
    frame$package <- "polars"
    frame$exports <- "pl"
    frame$classes$polars_data_frame <- c("polars_data_frame", "polars_object")
    frame$members$polars_data_frame <- c(group_by = "group_by")
    workspace$member_metadata$set("polars", member_index_freeze(frame))
    inspect <- function(type, use_pl = FALSE) {
        lines <- c(sprintf("#' @param x (`%s`)", type), "inspect <- function(x) {",
            if (use_pl) "    pl$all()" else "", "    x$field", "}")
        doc <- Document$new(uri, version = 1L, content = lines)
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        items <- member_completion(uri, workspace, doc, list(row = 3L, col = 6L), TRUE, 200L)
        list(doc = doc, data = parsed$member_data, labels = vapply(items, `[[`, "", "label"))
    }
    process <- inspect("process")
    expect_identical(process$data$packages, "processx")
    expect_identical(process$data$type_hint_packages, "processx")
    expect_true("kill" %in% process$labels)
    polars <- inspect("polars_data_frame", use_pl = TRUE)
    expect_identical(polars$data$packages, "polars")
    expect_identical(polars$data$type_hint_packages, "polars")
    expect_identical(polars$labels, "group_by")

    refreshed <- FALSE
    server <- list(text_sync = function(...) refreshed <<- TRUE)
    refresh_namespace_document(server, polars$doc)
    expect_false(refreshed)
    writeLines("import(polars)", manifest)
    refresh_namespace_document(server, polars$doc)
    expect_true(refreshed)
    expect_identical(inspect("polars_data_frame")$labels, "group_by")
    expect_length(inspect("integer")$data$packages, 0L)
    expect_length(parse_document(uri, "f <- function(x) x")$member_data$packages, 0L)
})
