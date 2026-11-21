s4_navigation_fixture <- function(lines) {
    fixture <- provider_fixture(lines)
    fixture$workspace$get_definition <- function(...) NULL
    fixture$lookup <- function(row, col = 2L) {
        definition_reply(1L, fixture$uri, fixture$workspace, fixture$document,
            list(row = row, col = col), getwd())$result
    }
    fixture
}

test_that("Go to Definition lists the S4 generic followed by its source methods", {
    lines <- c(
        'setMethod("compileStanModel", "First", function(object) 1)',
        'methods::setGeneric("compileStanModel", function(object)',
        '    standardGeneric("compileStanModel"))',
        'methods::setMethod(f = "compileStanModel", signature = "Second",',
        "    definition = function(object) 2)",
        "model <- compileStanModel(object)",
        "compileStanModel",
        'methods::selectMethod("compileStanModel", "First")',
        'methods::selectMethod("unrelated", "compileStanModel")',
        "unknownPkg :: compileStanModel(object)",
        "receiver $ compileStanModel(object)",
        "caller <- function(compileStanModel) compileStanModel(object)"
    )
    f <- s4_navigation_fixture(lines)
    targets <- lapply(c(1L, 0L, 3L), function(row) {
        end <- if (row == 0L) 0L else row + 1L
        location(f$uri, range(position(row, 0L), position(end, nchar(lines[[end + 1L]]))))
    })
    expect_equal(f$lookup(5L, 15L), targets)
    expect_equal(f$lookup(6L), targets)
    expect_equal(f$lookup(2L, 25L), targets)
    expect_equal(f$lookup(3L, 30L), targets)
    expect_equal(f$lookup(7L, 30L), targets)
    expect_null(f$lookup(8L, 40L))
    expect_null(f$lookup(9L, 20L))
    expect_null(f$lookup(10L, 18L))
    expect_equal(f$lookup(11L, 43L)$range, range(position(11L, 19L), position(11L, 35L)))
})

test_that("a single S4 method works without a source generic and distinguishes replacement calls", {
    f <- s4_navigation_fixture(c(
        'setMethod("value", "Widget", function(x) x)',
        'setReplaceMethod("value", "Widget", function(x, value) x)',
        'setMethod("+", c("Widget", "Widget"), function(e1, e2) e1)',
        'setMethod("%append%", "Widget", function(x, y) x)',
        'setReplaceMethod("[[", "Widget", function(x, i, j, ..., value) x)',
        "value(x)", "value(x) <- value(y)", "value(y) -> value(x)",
        "x + y", "x %append% y", "x[[i]] <- 2",
        "`value<-`(x, y)", "object <- list(value = function(x) x)", "object$value(x)",
        'setMethod("[", "Widget", function(x, i, j, ..., drop) x)', "x[i]",
        'setMethod("^", "Widget", function(e1, e2) e1)', "x**2", "x+y"
    ))
    line <- function(row, col) f$lookup(row, col)$range$start$line
    expect_equal(line(5L, 2L), 0L)
    expect_equal(line(6L, 2L), 1L)
    expect_equal(line(6L, 14L), 0L)
    expect_equal(line(7L, 14L), 1L)
    expect_equal(line(8L, 2L), 2L)
    expect_equal(line(9L, 4L), 3L)
    expect_equal(line(10L, 2L), 4L)
    expect_equal(line(11L, 4L), 1L)
    expect_equal(line(13L, 10L), 12L)
    expect_equal(line(15L, 1L), 14L)
    expect_equal(line(17L, 1L), 16L)
    expect_equal(line(18L, 1L), 2L)
})

test_that("S4 registrations promote an earlier definition and respect later shadowing", {
    f <- s4_navigation_fixture(c(
        "draw <- function(x) x",
        "draw(object)",
        'setGeneric("draw")',
        'setMethod("draw", "Model", function(x) 1)',
        "unrelated <- function() { draw <- 3 }",
        "draw(object)",
        "caller <- function(draw) draw(object)",
        "draw <- function(other) other",
        "draw(object)"
    ))
    expect_equal(f$lookup(1L)$range$start, position(0L, 0L))
    targets <- f$lookup(5L)
    expect_equal(vapply(targets, function(x) x$range$start$line, numeric(1L)), 2:3)
    expect_equal(f$lookup(6L, 27L)$range$start, position(6L, 19L))
    expect_equal(f$lookup(8L)$range$start, position(7L, 0L))
})

test_that("S4 definitions track a sourced file and unsaved edits over LSP", {
    skip_on_cran()
    root <- withr::local_tempdir()
    client <- language_client(root)
    main <- file.path(root, "main.R")
    methods <- file.path(root, "methods.R")
    declarations <- c('setGeneric("compileStanModel", function(object) standardGeneric("compileStanModel"))',
        'setMethod("compileStanModel", "Model", function(object) 1)')
    writeLines(declarations, methods)
    did_open(client, main, text = c('source("methods.R")', "model <- compileStanModel(object)"))
    targets <- respond_definition(client, main, c(1L, 16L))
    expect_equal(vapply(targets, `[[`, character(1L), "uri"), rep(path_to_uri(methods), 2L))
    expect_equal(vapply(targets, function(target) target$range$start$line, integer(1L)), 0:1)

    did_open(client, methods, text = declarations)
    text <- paste(c("# edited", declarations[[2L]], "compileStanModel(x)"), collapse = "\n")
    notify(client, "textDocument/didChange", list(textDocument = list(uri = path_to_uri(methods), version = 2L),
            contentChanges = list(list(text = text))))
    edited <- respond_definition(client, methods, c(2L, 8L), retry = FALSE)
    expect_equal(edited$uri, path_to_uri(methods))
    expect_equal(edited$range$start, list(line = 1L, character = 0L))
    expect_equal(respond_definition(client, main, c(1L, 16L), retry = FALSE), edited)
})
