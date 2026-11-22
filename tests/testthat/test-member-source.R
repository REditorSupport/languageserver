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

test_that("completed standalone blocks retain local members across writes and rebinding", {
    content <- c("f <- function() {", "obj <- new.env()", "{", "  alias <- obj",
        "  { alias$field <- list(first = 1L) }", "}", "original <- obj$field",
        "{ obj <- list(second = 2L) }", "original$first", "{ obj$second }", "}")
    f <- provider_fixture(content)
    check <- function(row, field) {
        at <- list(row = row, col = regexpr("$", content[[row + 1L]], fixed = TRUE)[[1L]])
        value <- member_resolve_cursor(f$uri, f$workspace, f$document, at, member_cursor(f$document, at))$value
        expect_identical(names(value$fields), field)
    }
    for (indexed in c(TRUE, FALSE)) {
        if (!indexed) {
            f$document$member_receivers$clear()
            f$document$parse_data$member_data$items[[1L]]$source <- NULL
        }
        check(8L, "first")
        check(9L, "second")
    }
})

test_that("member requests select dependencies through sibling and nested blocks", {
    code <- c("run <- function() {", "{", "  unused <- unneededPkg::make()",
        "  obj <- list(visible = 1L)", "}", "{", "  value <- obj", "  { value$visible }",
        "  value <- list(later = 2L)", "}", "}")
    f <- provider_fixture(code)
    at <- list(row = 7L, col = regexpr("$", code[[8L]], fixed = TRUE)[[1L]])
    cursor <- member_cursor(f$document, at)
    recovered <- member_recover_scope(f$document, at, cursor, f$document$parse_data$member_data)
    reads <- member_source_names(recovered$parsed, recovered$source, recovered$path)
    expect_true("obj" %in% reads)
    expect_false("unneededPkg" %in% reads)
    member <- member_resolve_cursor(f$uri, f$workspace, f$document, at, cursor)
    expect_identical(names(member$value$fields), "visible")
})

test_that("Scoped dependencies follow field writes at the point of each read", {
    statements <- c("x <- list()", "other <- list()", "other$skip <- 9", "x$first <- 5",
        "value <- x$first", "x$second <- 8", "message(value)", "x <- list(fresh = 3)",
        "x$third <- x$fresh", "x$third")
    scope <- member_scope_index(parse(text = c("{", statements, "}"))[[1L]])
    expect_identical(member_scope_slice(scope, 10L, "x"), c(1L, 4L, 5L, 7L, 8L, 9L, 10L))
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

test_that("Fresh unrelated member writes stay out of cursor inference", {
    for (call in c("target$fun()", "target$fun(")) {
        fixture <- provider_fixture(c("probe <- function() {", "scratch <- list()",
                sprintf("scratch$m%d <- list(inner = list(value = 1))", seq_len(60L)),
                "target <- list(fun = function(x, y) x + y)", call, "}"))
        point <- list(row = fixture$document$nline - 2L, col = 10L)
        result <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document, point,
            member_cursor(fixture$document, point), "fun")
        expect_identical(names(result$value$function_expr[[2L]]), c("x", "y"))
        expect_lt(20000L - result$budget$remaining, 20L)
    }
})

test_that("Scope slicing preserves direct writes through local and inherited aliases", {
    for (prefix in list(c("box <- new.env()", "alias <- box", "{"),
            c("{", "box <- new.env()", "alias <- box"))) {
        lines <- c("probe <- function() {", prefix, "alias$fun <- function(from_alias) NULL",
            "box$fun()", "}", "}")
        fixture <- provider_fixture(lines)
        point <- list(row = match("box$fun()", lines) - 1L, col = 7L)
        resolved <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document,
            point, member_cursor(fixture$document, point), "fun")
        expect_identical(names(resolved$value$function_expr[[2L]]), "from_alias")
    }
})

test_that("small recovered literate scopes are indexed on first member or S3 lookup", {
    uri <- "file:///deferred-scope.qmd"
    content <- c("```{r}", "small <- function() {", "  box <- list(alpha = 1L)", "  box$alpha", "}",
        "large <- function(x) {", rep("  x <- x + 1L", 50L), "}", "```", "```{r}", "pkg::", "```")
    doc <- Document$new(uri, version = 1L, content = content)
    workspace <- provider_fixture("")$workspace
    workspace$documents$set(uri, doc)
    workspace$s3_metadata <- list(may_dispatch = function(name) FALSE)

    accept <- function() {
        parsed <- parse_document(uri, doc$content, is_rmarkdown = TRUE)
        parsed$version <- doc$version
        doc$update_parse_data(unserialize(serialize(parsed, NULL)))
    }
    accept()
    items <- doc$parse_data$member_data$items
    expect_true(items[[1L]]$deferred_source)
    expect_null(items[[1L]]$source)
    expect_false(is.null(items[[2L]]$source))
    expect_null(doc$member_source_cache)

    point <- list(row = 3L, col = 6L)
    expect_true(s3_may_dispatch(workspace, doc, list(point = point, call = list(token = "unknown"))))
    expect_equal(doc$member_source_cache$size(), 1L)
    cached <- doc$member_source_cache$get("1")
    hover <- function() member_hover_reply(1L, uri, workspace, doc,
        member_hover_location(doc, list(row = 3L, col = 8L)))$result
    expect_match(hover()$contents, "1L", fixed = TRUE)
    expect_identical(member_item_source(doc, doc$parse_data$member_data, 1L), cached)

    content[[3L]] <- "  box <- list(beta = 2L)"
    content[[4L]] <- "  box$beta"
    doc$set_content(2L, content)
    expect_null(doc$member_source_cache)
    accept()
    cursor <- member_cursor(doc, point)
    expect_identical(names(member_resolve_cursor(uri, workspace, doc, point, cursor)$value$fields), "beta")
    expect_match(hover()$contents, "2L", fixed = TRUE)

    content[content == "pkg::"] <- "pkg::run"
    doc$set_content(3L, content)
    accept()
    expect_false(is.null(doc$parse_data$member_data$items[[1L]]$source))
    expect_null(doc$member_source_cache)
})

test_that("document indexing checks just the possible accessor registration calls", {
    content <- c("worker <- function(x, method) { method; x$a; x$b }",
        "method(print, Counter) <- function(x) { x$a; x$b }",
        "register <- function() base::registerS3method('$', 'Counter', accessor)")
    original <- member_s7_accessor_registration
    probed <- character()
    testthat::local_mocked_bindings(member_s7_accessor_registration = function(node) {
        probed <<- c(probed, s3_call_name(node[[1L]])$name)
        original(node)
    })
    parsed <- parse_document("file:///accessor-index.R", content)
    expect_true(parsed$member_data$accessor_registrations)
    expect_identical(probed, c("method", "registerS3method"))
})
