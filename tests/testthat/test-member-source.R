test_that("Accepted scope indexes avoid sibling syntax analysis on cold requests", {
    fixture <- provider_fixture(c("probe <- function() {",
            sprintf("unused%d <- list(other = 1)", seq_len(1000L)),
            "value <- list(first = 1)", "value$first", "}"))
    point <- list(row = 1002L, col = 6L)
    cursor <- member_cursor(fixture$document, point)
    syntax_names <- member_syntax_names
    cursor_path <- member_cursor_path
    testthat::local_mocked_bindings(
        member_syntax_names = function(expr) {
            if (length(all.names(expr)) > 20L) stop("Sibling syntax scanned")
            syntax_names(expr)
        },
        member_cursor_path = function(node, sentinel, accessor) {
            if (length(all.names(node)) > 20L) stop("Sibling cursor search")
            cursor_path(node, sentinel, accessor)
        },
        member_function_parts = function(...) stop("Function ranges rebuilt"),
        member_scope_index = function(...) stop("Scope rebuilt"), .package = "languageserver")
    for (i in seq_len(2L)) {
        fixture$document$member_receivers$clear()
        result <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
        expect_identical(names(result$value$fields), "first")
        expect_lt(20000L - result$budget$remaining, 20L)
    }
})

test_that("Scoped histories select the same statements as conservative backwards replay", {
    statements <- c("x <- list(old = 1)", "alias <- x", "x <- list(new = 2)",
        "if (flag) x <- list(branch = 3)", "read <- function() alias", "unused <- list(a = 1)",
        'Leaf <- methods::setClass("Leaf", slots = c(value = "numeric"))',
        "x$a <- alias", "alias <- list(final = 4)", "alias$final")
    scope <- member_scope_index(parse(text = c("{", statements, "}"))[[1L]])
    scan <- function(target, needed) {
        selected <- target
        for (i in rev(seq_len(target - 1L))) {
            assigned <- scope$assigned[[i]]
            if (!nzchar(assigned) || i %in% scope$mandatory || assigned %in% needed) {
                selected <- c(i, selected)
                needed <- union(setdiff(needed, assigned), scope$reads[[i]])
            }
        }
        selected
    }
    for (target in seq_along(statements)) for (needed in list("x", "alias", c("read", "Leaf"), "unbound")) {
        expect_identical(member_scope_slice(scope, target, needed), scan(target, needed))
    }
})

test_that("Serialized source indexes preserve nested scopes, defaults and alias captures", {
    cases <- list(
        c("probe <- function() {", "x <- list(old = 1)", "read <- function() x",
            "x <- list(new = 2)", "read()$old", "}"),
        c("probe <- function() {", "x <- list(old = 1)", "alias <- x", "x <- list(new = 2)",
            "alias$old", "}"),
        c("probe <- function() {", "x <- list(old = 1)", "if (flag) x <- list(new = 2)", "x$old", "}"),
        c("probe <- function(argument = (function() {", "x <- list(old = 1)", "x$old", "})()) NULL"),
        c("probe <- function() {", "x <- list(old = 1)", "inner <- function() {", "x$old", "}", "}"),
        c("probe <- function() {", "x <- list(old = 1)", "{", "x <- list(new = 2)", "x$new", "}", "}"),
        c("probe <- function() {", "x <- list(old = 1)", "x$old", "}")
    )
    for (content in cases) {
        fixture <- provider_fixture(content)
        fixture$document$parse_data$member_data <- unserialize(serialize(fixture$document$parse_data$member_data, NULL))
        row <- which(grepl("[$]", content))[[1L]] - 1L
        point <- list(row = row, col = regexpr("[$]", content[[row + 1L]])[[1L]])
        cursor <- member_cursor(fixture$document, point)
        indexed <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
        fixture$document$member_receivers$clear()
        fixture$document$parse_data$member_data$items <- lapply(fixture$document$parse_data$member_data$items, function(item) {
            item$source <- NULL
            item
        })
        fallback <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
        expect_equal(indexed$value, fallback$value)
    }
})

test_that("Blank scopes retain locals and declared class context", {
    fixture <- provider_fixture(c('C <- R6::R6Class("C", public = list(run = function() {',
            "x <- list(first = 1)", "", "}))"))
    point <- list(row = 2L, col = 0L)
    cursor <- list(start = 0L, end = 0L, before = "")
    scope <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
    expect_true(all(c("self", "x") %in% names(scope$value$fields)))
    fixture <- provider_fixture(c("probe <- function() {", "x <- list(first = 1)", "", "}"))
    point <- list(row = 2L, col = 0L)
    scope <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point, cursor)
    expect_true("x" %in% names(scope$value$fields))
    fixture <- provider_fixture(c("probe <- function() {", "x <- list(first = 1)", "x$first", "}"))
    symbol <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document,
        list(row = 1L, col = 0L), list(start = 0L, end = 0L, before = ""), name = "unbound")
    expect_false(length(symbol$value$type) > 0L)
})

test_that("Source indexes replace on accepted edits and do not change invalid-source recovery", {
    fixture <- provider_fixture(c("probe <- function() {", "x <- list(old = 1)", "x$old", "}"))
    point <- list(row = 2L, col = 2L)
    old <- fixture$document$parse_data$member_data$items[[1L]]$source
    fixture$document$set_content(2L, c("probe <- function() {", "x <- list(new = 1)", "x$new", "}"))
    expect_null(member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
            member_cursor(fixture$document, point)))
    parsed <- parse_document(fixture$uri, fixture$document$content)
    parsed$version <- 2L
    fixture$document$update_parse_data(parsed)
    expect_false(identical(old, parsed$member_data$items[[1L]]$source))
    result <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
        member_cursor(fixture$document, point))
    expect_identical(names(result$value$fields), "new")
    fixture <- provider_fixture(c("probe <- function() {", "x <- list(first = 1)", "x$"))
    expect_true(fixture$document$parse_data$parse_error)
    expect_true(all(vapply(fixture$document$parse_data$member_data$items, function(item) is.null(item$source), logical(1L))))
    result <- member_completion(fixture$uri, fixture$workspace, fixture$document, point, FALSE, 200L)
    expect_true("first" %in% vapply(result, `[[`, character(1L), "label"))
})

test_that("Native syntax summaries include defaults without reading bindings or attributes", {
    for (expr in list(quote(x + y$z), quote(function(argument = a, nested = function(value = b) value) c),
        quote(function(a, b = S7::new_class("C")) {
            function(c = d) c
        }),
        expression(x, function(a = x) y), quote(base::list(x = a, y = b)))) {
        reference <- function(node) {
            names <- all.names(node, unique = TRUE)
            if (!"function" %in% names) return(names)
            defaults <- list()
            member_walk(node, function(child) {
                if (member_head(child, "function")) {
                    defaults[[length(defaults) + 1L]] <<- reference(as.expression(as.list(child[[2L]])))
                }
            })
            unique(c(names, unlist(defaults, use.names = FALSE)))
        }
        expect_setequal(member_syntax_names(expr), reference(expr))
        expect_false(anyDuplicated(member_syntax_names(expr)) > 0L)
    }
    expr <- quote(identity(x))
    attr(expr, "ignored") <- quote(unrelated)
    expect_identical(member_syntax_names(expr), c("identity", "x"))
    expect_identical(member_syntax_names(list(quote(x))), character())
    expr <- quote(x)
    for (i in seq_len(10000L)) expr <- as.call(list(as.name("identity"), expr))
    expect_setequal(member_syntax_names(expr), c("identity", "x"))
    expect_identical(member_syntax_names(quote(function(unused) NULL)), "function")
})

test_that("Parse cache accounts for retained scope index environments", {
    parsed <- parse_document("file:///indexed.R", c("probe <- function() {",
            sprintf("x%d <- list(value = 1)", seq_len(100L)), "}"))
    source <- parsed$member_data$items[[1L]]$source
    expect_gt(source$bytes, as.numeric(object.size(source)))
    value <- as.list(parsed)
    cache <- ByteLruCache$new(as.numeric(object.size(value)) + source$bytes - 1)
    cache$set("parse", value, additional_bytes = source$bytes)
    expect_false(cache$has("parse"))
    cache <- ByteLruCache$new(as.numeric(object.size(value)) + source$bytes)
    cache$set("parse", value, additional_bytes = source$bytes)
    expect_true(cache$has("parse"))
    expect_equal(cache$bytes(), as.numeric(object.size(value)) + source$bytes)
})
