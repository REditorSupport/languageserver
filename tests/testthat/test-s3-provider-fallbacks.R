s3_parameter_fixture <- function(content) {
    definitions <- c("draw <- function(x, ...) UseMethod('draw')",
        "draw.integer <- function(x, mode = c('region', 'replace'), ...) NULL")
    f <- provider_fixture(c(definitions, content))
    f$workspace$s3_metadata <- S3MetadataCache$new()
    catalogs <- provider_startup_s3()
    for (package in names(catalogs)) f$workspace$s3_metadata$set(package, catalogs[[package]])
    f$workspace$get_documentation <- function(...) list(arguments = list(mode = "Drawing mode."))
    f
}

test_that("S3 argument resolution uses documentation from the completion's revision", {
    f <- s3_parameter_fixture("draw(1L, )")
    call <- resolve_s3_call(f$uri, f$workspace, f$document, list(row = 2L, col = 9L))
    item <- s3_arguments(call, "m")[[1L]]
    resolve <- function(item) completion_item_resolve_reply(1L, f$workspace, item, list())$result
    expect_match(resolve(item)$documentation$value, "Drawing mode.", fixed = TRUE)
    expect_match(resolve(item)$documentation$value, "draw.integer", fixed = TRUE)
    for (field in c("version", "revision", "source_revision")) {
        stale <- item
        stale$data[[field]] <- -1L
        expect_null(resolve(stale)$documentation, info = field)
    }
    f$workspace$documents$remove(f$uri)
    expect_null(resolve(item)$documentation)
})

test_that("S3 quoted completions replace only the string contents around the cursor", {
    line <- 'draw(1L, other = "a\\"b", mode = "re\\"old", 3)'
    f <- s3_parameter_fixture(line)
    # The cursor precedes the escaped quote; the closing quote ends the edit.
    before <- regexpr("re", line, fixed = TRUE)[[1L]] + 1L
    get_items <- function(col) s3_quoted_values(f$workspace, f$document, list(row = 2L, col = col))
    items <- get_items(before)
    expect_setequal(vapply(items, `[[`, "", "label"), c("region", "replace"))
    for (item in items) {
        expect_identical(item$textEdit$newText, item$label)
        expect_identical(item$textEdit$range,
            list(start = position(2L, before - 2L), end = position(2L, regexpr('", 3', line, fixed = TRUE)[[1L]] - 1L)))
    }
    expect_null(get_items(before - 3L))
    f <- s3_parameter_fixture(c("draw(1L, # 're", "     mode = 'region')"))
    expect_null(get_items(nchar(f$document$line0(2L))))
})

test_that("S3 index summaries retain navigation spans for large functions", {
    long <- paste(rep("a <- 1L", 600L), collapse = "; ")
    lines <- c(paste0("factory <- function(value) { ", long, "; value }"),
        paste0("if (flag) { ", long, " }"))
    index <- s3_index_summary(lines, "file:///large.R", parse(text = lines, keep.source = TRUE))
    expect_identical(names(index$items[[1L]]$expr[[3L]][[2L]]), "value")
    expect_identical(index$items[[1L]]$implementation_columns, c(0L, nchar(lines[[1L]])))
    expect_lt(as.numeric(object.size(index$items[[1L]]$expr)), 65536)
    expect_lt(as.numeric(object.size(index$items[[2L]]$expr)), 65536)
    expect_null(s3_index_summary(lines, "file:///large.R"))
})

test_that("namespace snapshots separate qualified S3 registrations from unrelated directives", {
    root <- withr::local_tempdir()
    path <- file.path(root, "NAMESPACE")
    directives <- c('export("render")', 'S3method("render", box)',
        "S3method(stats::predict, box, predict_box)", "S3method(opaque(), , missing)")
    writeLines(directives, path)
    expect_identical(s3_namespace_source(root), list(
        list(generic = "render", package = NULL, class = "box", method = "render.box"),
        list(generic = "predict", package = "stats", class = "box", method = "predict_box")))
    writeLines(strrep(" ", 65537L), path)
    expect_false(s3_namespace_snapshot(root)$valid)
    expect_length(s3_namespace_source(root), 0L)
})

test_that("disk S3 summaries retain physical method spans and exclude incomplete outer blocks", {
    lines <- c("{", "  draw <- function(x) { { UseMethod('draw') } }",
        "  { draw.integer <- function(x) x }", "}", "{", "  hidden <- 1", "}")
    parsed <- parse(text = lines, keep.source = TRUE)
    summary <- s3_index_summary(lines, "file:///blocks.R", parsed, incomplete_lines = 6L)
    expect_equal(names(summary$index$histories), c("draw", "draw.integer"))
    expect_identical(summary$index$functions$draw, list(name = "draw", formal = "x"))
    expect_identical(summary$items[[2L]]$start, c(2L, 4L))
    expect_identical(summary$items[[2L]]$end, c(2L, nchar(lines[[3L]]) - 2L))
})

test_that("invalid S3 catalogs cannot replace a usable cached generic", {
    cache <- S3MetadataCache$new()
    empty <- cache$bytes()
    expect_identical(cache$get("example", "missing"), "missing")
    catalog <- list(schema = 1L, generation = "first", package = "example",
        functions = list(draw = list(name = "draw", owner = "example", generic = list(name = "draw"))))
    cache$set("example", catalog)
    revision <- cache$revision()
    expect_true(cache$may_dispatch("draw"))
    expect_true(cache$has("example"))
    expect_gt(cache$bytes(), empty)
    invalid <- catalog
    invalid$schema <- 0L
    invalid$generation <- "replacement"
    cache$set("example", invalid)
    expect_identical(cache$revision(), revision)
    expect_identical(cache$get("example"), catalog)
    cache$clear()
    expect_false(cache$has("example"))
    expect_false(cache$may_dispatch("draw"))
    expect_identical(cache$bytes(), empty)
})
