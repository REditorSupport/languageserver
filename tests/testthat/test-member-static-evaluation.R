test_that("Static switch inference selects indices, fallthroughs and the unnamed default", {
    index <- member_generic_index("")
    infer <- function(expr) member_infer(expr, index)
    expect_identical(infer(quote(switch(2L, "first", "second")))$literal, "second")
    expect_identical(infer(quote(switch("short", short = , long = list(found = TRUE))))$fields$found$literal, TRUE)
    expect_identical(infer(quote(switch("other", named = FALSE, "fallback")))$literal, "fallback")
    expect_identical(infer(quote(switch("other", named = FALSE)))$type, "NULL")
    both <- infer(quote(switch(key, a = , b = list(common = 1, left = 2), c = list(common = 3, right = 4))))
    expect_named(both$fields, "common")
    expect_identical(both$fields$common$type, "double")
    expect_false(both$fields$common$known_literal)
    expect_identical(infer(quote(switch(key)))$type, "NULL")
})

test_that("Static map and reduce retain element shapes only when every step supports them", {
    index <- member_generic_index(c("wrap <- function(x) list(item = x)", "last <- function(previous, value) value"))
    infer <- function(expr, scope = list()) member_infer(expr, index, scope)
    expect_identical(infer(quote(lapply(list(1L, "two"), wrap)[[2L]]$item))$literal, "two")
    expect_identical(infer(quote(Reduce(last, list(1L, "last"))))$literal, "last")
    expect_identical(infer(quote(Reduce(function(a, b) b, list("first", 3L))))$literal, 3L)
    expect_identical(infer(quote(Reduce(last, list())))$type, "NULL")
    expect_identical(infer(quote(Reduce(last, list(), "seed")))$literal, "seed")
    scope <- list(xs = member_value(type = "list", element_shape = member_literal(2L)))
    mapped <- infer(quote(if (condition) lapply(xs, wrap) else lapply(xs, function(x) list(item = "other"))), scope)
    expect_setequal(mapped$element_shape$fields$item$type, c("integer", "character"))
    expect_identical(infer(quote(Reduce(function(a, b) a, xs, list(ok = TRUE))), scope)$fields$ok$literal, TRUE)
    expect_null(infer(quote(Reduce(function(a, b) b, xs, list(ok = TRUE))), scope)$type)
    expect_null(infer(quote(Reduce(last, xs)), scope)$type)
    expect_null(infer(quote(Reduce(last, xs, init = 0L)), scope)$type)
    scope$xs <- member_value(type = "list", elements = rep(list(member_literal(1L)), 33L))
    expect_null(infer(quote(Reduce(last, xs)), scope)$type)
    expect_identical(infer(quote(lapply(xs, wrap)), scope)$type, "list")
    expect_null(infer(quote(lapply(xs, wrap)), scope)$elements)
})

test_that("Static dispatch joins method results and rejects conflicting operator methods", {
    index <- member_generic_index(c(
        "report <- function(x) UseMethod('report', x)",
        "report.integer <- function(x) list(total = 1L, integer_only = TRUE)",
        "report.double <- function(x) list(total = 2L, double_only = TRUE)",
        "`+.left` <- function(x, y) list(left = TRUE)",
        "`+.right` <- function(x, y) list(right = TRUE)",
        "`[.left` <- function(x, i) list(position = i)",
        "delegate <- function(x) UseMethod('delegate')",
        "delegate.integer <- function(x) NextMethod()",
        "alias <- report.integer",
        "cycle <- other", "other <- cycle",
        "report.character <- alias"))
    infer <- function(expr) member_infer(expr, index)
    common <- infer(quote(report(if (flag) 1L else 1.5)))
    expect_named(common$fields, "total")
    expect_false(common$fields$total$known_literal)
    expect_identical(infer(quote(report("sample")$total))$literal, 1L)
    expect_null(infer(quote(report(if (flag) 1L else TRUE)))$type)
    expect_null(infer(quote(delegate(1L)))$type)
    expect_null(infer(quote(delegate("no method")))$type)
    expect_identical(infer(quote(structure(list(), class = "left")[2L]$position))$literal, 2L)
    expect_named(infer(quote(structure(list(), class = "left") + 1L))$fields, "left")
    conflict <- infer(quote(structure(list(), class = "left") + structure(list(), class = "right")))
    expect_identical(conflict$reason, "conflicting_dispatch")
    expect_identical(infer(quote(cycle()))$reason, "alias_cycle")
    expect_null(member_definition(index$definitions, "cycle"))
})

test_that("Literal guards narrow branches only for known scalar inputs", {
    index <- member_generic_index("")
    index$intrinsics <- c("is_bool", "is_character", "is_list")
    infer <- function(expr) member_infer(expr, index)
    for (guard in expression(1L < 2L, 2L <= 2L, 2L != 1L, startsWith("record", "re"),
            is.integer(1L), is_character("value"), is_list(list()), is_bool(FALSE))) {
        expect_identical(infer(guard)$literal, TRUE)
    }
    for (guard in expression(1L != 1L, 2L < 2L, 2L <= 1L, is_bool(NA), is_bool(c(TRUE, FALSE)))) {
        expect_identical(infer(guard)$literal, FALSE)
    }
    expect_false(infer(quote(is_bool(as.logical(input))))$known_literal)
    expect_identical(infer(quote(head(list(first = 1))$first))$literal, 1)
    expect_null(infer(quote(structure(list(first = 1), class = 2L)))$type)
    expect_null(infer(quote(tryCatch(list(ok = 1), error = unknown_handler)))$type)
})

test_that("Open forwarded dots cannot supply a reliable default or count", {
    index <- member_generic_index(c("f <- function(x = 'default', ...) list(first = x, rest = list(...))",
            "g <- function(...) f(...)"))
    infer <- function(expr, scope = list()) member_infer(expr, index, scope)
    expect_identical(infer(quote(g()$first))$literal, "default")
    expect_identical(infer(quote(g(2L, key = 3L)$rest$key))$literal, 3L)
    scope <- list(... = member_value(type = "list", open = TRUE))
    open <- infer(quote(f(...)), scope)
    expect_null(open$fields$first$type)
    expect_true(open$fields$rest$open)
    expect_identical(infer(quote(g(, key = 3L)$first))$literal, "default")
})

test_that("Trimming the inference cache preserves its reservation callback and fresh results", {
    index <- member_generic_index("wrap <- function(x) list(item = x)")
    calls <- 0L
    index$cache$.reserve <- function(bytes) {
        calls <<- calls + 1L
        TRUE
    }
    expect_identical(member_infer(quote(wrap(1L)$item), index)$literal, 1L)
    index$cache$.bytes <- 4 * 1024^2
    expect_identical(member_infer(quote(wrap(2L)$item), index)$literal, 2L)
    expect_identical(member_infer(quote(wrap(1L)$item), index)$literal, 1L)
    expect_identical(calls, 3L)
    expect_lt(index$cache$.bytes, 4 * 1024^2)
})
