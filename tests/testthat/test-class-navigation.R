class_navigation_fixture <- function(code) {
    f <- provider_fixture(code)
    catalogs <- provider_startup_s3()
    f$workspace$s3_metadata <- S3MetadataCache$new()
    for (p in names(catalogs)) f$workspace$s3_metadata$set(p, catalogs[[p]])
    f
}

test_that("implementation navigates from the callee to its selected method", {
    lines <- c("g <- function(x) UseMethod('g')", "g.numeric <- function(x) x", "g.character <- function(x) trimws(x)",
        "g(1)", "g('one')", "g(unknown)", "plain <- function(x) x", "plain(1)", "g(plain)", "list(g=plain)$g(1)")
    f <- class_navigation_fixture(lines)
    impl <- function(row, col = 0L) implementation_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col))$result
    expect_identical(impl(3L)$range$start$line, 1L)
    expect_identical(impl(4L)$range$start$line, 2L)
    expect_null(impl(5L))
    expect_null(impl(7L))
    expect_null(impl(8L, 3L))
    expect_null(impl(9L, 14L))
    f <- class_navigation_fixture(c('`paint me` <- function(x) UseMethod("paint me")',
            "`paint me.numeric` <- function(x) x", "`paint me`(1)", "unrelated <- "))
    expect_identical(impl(2L, 4L)$range$start$line, 1L)
    f$document$set_content(2L, f$document$content)
    expect_null(impl(2L, 4L))
})

test_that("implementation preserves source aliases and rejects conflicting peer owners", {
    f <- class_navigation_fixture(c('draw <- function(x) UseMethod("draw")', "draw(1)"))
    add <- function(uri, lines) {
        d <- Document$new(uri, language = "r", version = 1L, content = lines)
        parsed <- parse_document(uri, lines)
        parsed$version <- 1L
        d$update_parse_data(parsed)
        f$workspace$documents$set(uri, d)
    }
    one <- "file:///first-provider.R"
    add(one, c("paint <- function(x) x", "draw.numeric <- paint"))
    at <- function() implementation_reply(1L, f$uri, f$workspace, f$document, list(row = 1L, col = 1L))$result
    expect_identical(at()$uri, one)
    expect_identical(at()$range$start$line, 0L)
    add("file:///other-provider.R", "draw.numeric <- function(x) x + 1")
    expect_null(at())
})

test_that("implementation verifies installed method bindings and generated contents", {
    f <- class_navigation_fixture("stats::predict(stats::lm(mpg ~ wt, mtcars))")
    content <- TextDocumentContent$new()
    target <- implementation_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 9L), content)$result
    expect_match(target$uri, "predict.lm", fixed = TRUE)
    expect_match(content$get(target$uri), "function", fixed = TRUE)
    expect_null(implementation_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 1L))$result)
    current <- s3_prepare_namespace("stats")$methods[["stats::predict.lm"]]
    current$body_hash <- "stale"
    expect_null(s3_implementation_target(current, f$workspace, NULL))
    current$verified_binding <- FALSE
    expect_null(s3_implementation_target(current, f$workspace, NULL))
})

test_that("foreground implementation lookup never reads deferred or active bindings", {
    scope <- new.env(parent = emptyenv())
    delayedAssign("lazy", stop("promise forced"), assign.env = scope)
    makeActiveBinding("active", function() stop("active binding read"), scope)
    expect_identical(s3_verified_method_body("pkg", "lazy", "pkg", "hash", FALSE, scope), list(deferred = TRUE))
    expect_null(s3_verified_method_body("pkg", "active", "pkg", "hash", FALSE, scope))
})

test_that("type definition requires a unique visible declaration of the inferred class", {
    types <- list(
        c('Alias <- R6::R6Class("Widget")', "x <- Alias$new()"),
        c('Alias <- methods::setClass("Widget")', 'x <- methods::new("Widget")'),
        c('Alias <- methods::setRefClass("Widget", fields=c(count="numeric"))', "x <- Alias$new()")
    )
    if (requireNamespace("S7", quietly = TRUE)) types <- c(types, list(c(
        'Alias <- S7::new_class("Widget", properties=list(x=S7::class_numeric))', "x <- Alias()")))
    for (type in types) {
        lines <- c(type, "box <- list(child=x)", "x", "box$child", "x <- 1", "x", '"x"', "# x", "unrelated <-")
        f <- provider_fixture(lines)
        if (grepl("S7::", type[[1L]], fixed = TRUE)) {
            f$workspace$member_metadata <- MemberMetadataCache$new(20 * 1024^2)
            f$workspace$member_metadata$set("S7", member_prepare_package("S7"))
        }
        at <- function(row, col = 0L) type_definition_reply(1L, f$uri, f$workspace, f$document, list(row = row, col = col))$result
        expect_equal(at(3L), list(uri = f$uri, range = range(position(0L, 0L), position(0L, 5L))))
        expect_equal(at(4L, 6L), at(3L))
        for (row in 6:8) expect_null(at(row, 1L))
    }
    f <- provider_fixture(c('A <- R6::R6Class("Widget")', 'B <- R6::R6Class("Widget")', "x <- A$new()", "x"))
    expect_null(at(3L))
    # A class defined inside a function has no visible top-level declaration.
    f <- provider_fixture(c('Outer <- R6::R6Class("Widget")',
            'make <- function() { Inner <- R6::R6Class("Widget"); Inner$new() }', "x <- make()", "x"))
    expect_null(at(3L))
    f <- provider_fixture(c('`constructor name` <- R6::R6Class("Widget")', "`a b` <- `constructor name`$new()", "`a b`"))
    expect_identical(at(2L, 2L)$range$start$line, 0L)
    f$document$set_content(2L, f$document$content)
    expect_null(at(2L, 2L))
})

test_that("class navigation requests run on the current buffer over LSP", {
    skip_on_cran()
    root <- withr::local_tempdir()
    client <- language_client(working_dir = root)
    expect_true(client$ServerCapabilities$implementationProvider)
    expect_true(client$ServerCapabilities$typeDefinitionProvider)
    path <- file.path(root, "class-navigation.R")
    uri <- path_to_uri(path)
    did_open(client, path, text = "old <- 1")
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    lines <- c('C <- R6::R6Class("Widget")', "x <- C$new()", "x",
        'g <- function(x) UseMethod("g")', "g.numeric <- function(x) x", "g(1)", "incomplete <-")
    notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = 2L),
            contentChanges = list(list(text = paste(lines, collapse = "\n")))))
    at <- function(method, row) {
        respond(client, paste0("textDocument/", method),
            list(textDocument = list(uri = uri), position = list(line = row, character = 0L)), retry = FALSE)
    }
    expect_equal(at("typeDefinition", 2L)$range$start, list(line = 0L, character = 0L))
    expect_equal(at("implementation", 5L)$range$start, list(line = 4L, character = 0L))
    lines[[3L]] <- "x <- 1"
    lines[[6L]] <- "g(unknown)"
    notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = 3L),
            contentChanges = list(list(text = paste(lines, collapse = "\n")))))
    expect_null(at("implementation", 5L))
    expect_null(at("typeDefinition", 5L))
})
