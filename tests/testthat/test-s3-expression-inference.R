# Analyze independent expressions against the same namespace records the
# server's worker supplies, without depending on the request time budget.
s3_expression_analyzer <- function(catalogs = provider_startup_s3()) {
    context <- new.env(parent = emptyenv())
    context$catalogs <- catalogs
    context$deadline <- Inf
    s3_analyzer(context)$infer
}

test_that("S3 inference follows the storage and dimensions of base constructors", {
    infer <- s3_expression_analyzer()
    cases <- expression(
        factor("a"), factor("a", ordered = TRUE), factor(ordered("a")),
        as.Date("2026-01-15"), as.POSIXct("2026-01-15", tz = "UTC"),
        as.POSIXlt("2026-01-15", tz = "UTC"), as.difftime(1, units = "days"),
        table("x"), as.integer("1"), as.numeric("1"), as.character(1),
        as.logical(1), as.raw(1), rep(1L, 3), invisible(1L), identity("x"),
        matrix(1L, 2, 3), matrix(1L, nrow = 1L), matrix(1L),
        array(1, c(2, 3, 1)), array(1, c(2, 3)),
        unclass(structure(1L, class = "item")),
        `+`(2), `-`(2), 3L + 2L, 3L - 2L, 3L * 2L, 3L / 2L,
        3L > 2L, 3L < 2L, 3L >= 2L, 3L <= 2L, 3L == 2L, 3L != 2L
    )
    for (expr in cases) {
        runtime <- eval(expr, baseenv())
        value <- infer(expr)
        expect_identical(value$classes, .class2(runtime), info = deparse1(expr))
        expect_identical(value$type, typeof(runtime), info = deparse1(expr))
        if (isTRUE(value$known)) expect_identical(value$literal, runtime)
        if ("array" %in% value$classes) expect_equal(value$dimensions, dim(runtime))
    }
    expect_identical(infer(quote(1:5000))$classes, .class2(1L))
    expect_identical(infer(quote(0.5:5000))$classes, .class2(0.5))
    expect_false(isTRUE(infer(quote(1:5000))$known))
    expect_null(infer(quote(1:Inf))$classes)
    expect_null(infer(quote(array(1, dim = unknown)))$classes)
    expect_null(infer(quote(factor("a", ordered = unknown)))$classes)
    expect_null(infer(quote(matrix(1, nrow = 1, nrow = 2)))$classes)
    expect_null(infer(quote(unclass(unknown)))$classes)
    expect_null(infer(quote(structure(1, class = unknown)))$classes)
    expect_null(infer(quote(structure(1, dim = c(1, 1))))$classes)
    expect_null(infer(quote(c(1, unknown)))$classes)
    expect_null(infer(quote(rep(structure(1, class = "custom"), 2)))$classes)
})

test_that("S3 inference distinguishes data frame and field extraction", {
    infer <- s3_expression_analyzer()
    infer(quote(df <- data.frame(a = 2L, b = "value")))
    for (expr in expression(df["a"], df[, c("a", "b")], df[, 1, drop = FALSE])) {
        expect_identical(infer(expr)$classes, "data.frame", info = deparse1(expr))
    }
    for (expr in expression(df[, "a"], df[["a"]], df[[1]], df$a, df$a[1])) {
        expect_identical(infer(expr)$classes, .class2(2L), info = deparse1(expr))
    }
    expect_identical(infer(quote(df[, 2]))$classes, "character")
    for (expr in expression(df[, ], df[, unknown], df[, "absent"], df[["absent"]],
            df[[unknown]], 1L[[1]], unknown[1], structure(1, class = "custom")[1])) {
        expect_null(infer(expr)$classes, info = deparse1(expr))
    }

    expect_identical(infer(quote(data.frame(a = "x", stringsAsFactors = TRUE)$a))$classes, "factor")
    expect_null(infer(quote(data.frame(a = "x", stringsAsFactors = unknown)$a))$classes)
    infer(quote(box <- structure(list(a = 1), class = "box")))
    infer(quote(`[.box` <- function(x, i) "subset result"))
    infer(quote(`$.box` <- function(x, name) "field result"))
    expect_identical(infer(quote(box[1]))$literal, "subset result")
    # A custom $ may compute an entirely different value from the stored field.
    expect_null(infer(quote(box$a))$classes)
})

test_that("S3 inference preserves only certain classes across control flow", {
    infer <- s3_expression_analyzer()
    expect_identical(infer(quote(if (TRUE) "yes" else 1))$literal, "yes")
    expect_identical(infer(quote(if (FALSE) "yes" else 1))$literal, 1)
    expect_identical(infer(quote(if (FALSE) "yes"))$classes, "NULL")
    same <- infer(quote(if (unknown) 1L else 2L))
    expect_identical(same$classes, .class2(1L))
    expect_false(same$known)
    expect_null(infer(quote(if (unknown) "yes" else 2L))$classes)
    infer(quote(x <- structure(1L, class = "custom")))
    infer(quote(class(x) <- NULL))
    expect_identical(infer(quote(x))$classes, .class2(1L))
    infer(quote(class(x) <- unknown))
    expect_null(infer(quote(x))$classes)
    infer(quote(x <- 1L))
    infer(quote(for (i in 1:3) x <- i))
    expect_null(infer(quote(x))$classes)
    infer(quote(if (unknown) new_binding <- 1))
    expect_null(infer(quote(new_binding))$classes)
})

test_that("S3 factories match defaults and forwarded dots before dispatch", {
    infer <- s3_expression_analyzer()
    infer(quote(make <- function(key, value = key) value))
    infer(quote(wrapper <- function(key, ...) make(key, ...)))
    expect_identical(infer(quote(wrapper(1, value = "text")))$literal, "text")
    expect_identical(infer(quote(make(, value = TRUE)))$literal, TRUE)
    expect_null(infer(quote(make(1, 2, 3)))$classes)
    expect_null(infer(quote(make(...)))$classes)
    infer(quote(empty <- function(flag = TRUE) {
        if (flag) return()
        1
    }))
    expect_identical(infer(quote(empty()))$classes, "NULL")

    infer(quote(g <- function(x) UseMethod("g")))
    infer(quote(g.integer <- function(x) "integer result"))
    expect_identical(infer(quote(g(1L)))$literal, "integer result")
    expect_null(infer(quote(g(TRUE)))$classes)
    infer(quote(alias <- base::identity))
    expect_identical(infer(quote(alias(7L)))$literal, 7L)
    infer(quote(alias <- 3L))
    expect_null(infer(quote(alias(7L)))$classes)
    expect_identical(infer(quote(datasets::iris))$classes, "data.frame")
})

test_that("S3 pipe inference handles tee, assignment, and shadowed operators", {
    infer <- s3_expression_analyzer()
    infer(quote(convert <- function(x) "converted"))
    infer(quote(x <- 1L))
    expect_identical(infer(quote(x %>% convert))$literal, "converted")
    expect_identical(infer(quote(x %T>% convert()))$literal, 1L)
    infer(quote(x %<>% convert()))
    expect_identical(infer(quote(x))$literal, "converted")
    expect_null(infer(quote(x %>% 1))$classes)
    infer(quote(`%>%` <- function(x, y) x))
    expect_null(infer(quote(x %>% convert()))$classes)
})

test_that("S3 model summaries account for formula response shape and method", {
    infer <- s3_expression_analyzer()
    infer(quote(df <- data.frame(a = 1, b = 2)))
    expect_identical(infer(quote(stats::lm(a ~ b, data = df)))$classes, "lm")
    expect_identical(infer(quote(stats::lm(cbind(a, b) ~ 1, data = df)))$classes, c("mlm", "lm"))
    expect_identical(infer(quote(stats::lm(I(a > 1) ~ b, data = df)))$classes, "lm")
    expect_identical(infer(quote(stats::lm(a ~ b, method = "model.frame")))$classes, "data.frame")
    expect_identical(infer(quote(stats::glm(a ~ b, method = "model.frame")))$classes, "data.frame")
    expect_identical(infer(quote(stats::glm(a ~ b)))$classes, c("glm", "lm"))
    for (expr in expression(stats::lm(a ~ b, data = unknown), stats::lm(formula),
            stats::lm(transform(a) ~ b, data = df), stats::lm(a ~ b, data = df, method = "custom"),
            stats::glm(a ~ b, method = unknown), stats::aov(cbind(a, b) ~ 1, data = df),
            stats::aov(a ~ b + Error(site)), stats::ts(1:2, class = unknown),
            stats::ts(array(1L, c(2, 1, 1))))) {
        expect_null(infer(expr)$classes, info = deparse1(expr))
    }
})

test_that("grouping constructors preserve only the classes implied by their arguments", {
    context <- new.env(parent = emptyenv())
    scope <- new.env(parent = emptyenv())
    summary <- function(package, name, x, ...) {
        s3_constructor_summary(name, package, list(quote(df)), c(list(x), list(...)), scope, NULL, context)
    }
    df <- s3_value("data.frame", "list")
    tibble <- s3_value(c("tbl_df", "tbl", "data.frame"), "list")
    grouped <- summary("dplyr", "group_by", tibble)
    expect_identical(grouped$classes, c("grouped_df", tibble$classes))
    expect_identical(summary("dplyr", "mutate", grouped)$classes, grouped$classes)
    drop <- s3_value("character", literal = "drop", known = TRUE)
    keep <- s3_value("character", literal = "keep", known = TRUE)
    expect_identical(summary("dplyr", "summarise", grouped, .groups = drop)$classes, tibble$classes)
    expect_identical(summary("dplyr", "summarise", grouped, .groups = keep)$classes, grouped$classes)
    expect_identical(summary("dplyr", "summarise", df)$classes, df$classes)
    expect_null(summary("dplyr", "summarise", grouped)$classes)
    rowwise <- summary("dplyr", "rowwise", df)
    expect_identical(rowwise$classes, c("rowwise_df", tibble$classes))
    expect_null(summary("dplyr", "summarise", rowwise)$classes)

    custom <- s3_value(c("remote_table", "data.frame"), "list")
    expect_null(summary("dplyr", "group_by", custom)$classes)
    expect_null(summary("dplyr", "mutate", custom)$classes)
    expect_null(summary("dplyr", "group_by", s3_unknown())$classes)
    context$incomplete <- TRUE
    expect_null(summary("dplyr", "group_by", df)$classes)
    context$incomplete <- FALSE

    converted <- summary("data.table", "setDT", df)
    expect_identical(converted$classes, c("data.table", "data.frame"))
    expect_identical(scope$df, converted)
    expect_identical(summary("data.table", "setDF", converted)$classes, "data.frame")
    expect_identical(scope$df$classes, "data.frame")
})

test_that("survival constructors require a known event type", {
    context <- new.env(parent = emptyenv())
    scope <- new.env(parent = emptyenv())
    summary <- function(name, formula, status) {
        data <- s3_value("data.frame", "list", fields = list(status = status))
        s3_constructor_summary(name, "survival", list(formula),
            list(s3_unknown(), data = data), scope, NULL, context)
    }
    formula <- quote(Surv(time, status) ~ treatment)
    for (name in c("coxph", "survfit")) {
        expect_identical(summary(name, formula, s3_value("logical", "logical"))$classes, name)
        expect_null(summary(name, formula, s3_value("factor", "integer"))$classes)
        expect_null(summary(name, quote(Surv(time) ~ treatment), s3_unknown())$classes)
    }
})

test_that("conditional opaque effects invalidate visible locals through nested branches", {
    infer <- s3_expression_analyzer()
    expect_null(infer(quote({
        x <- 1L
        if (a) if (b) opaque(x)
        x
    }))$classes)
    expect_null(infer(quote({
        x <- 1L
        if (a) if (b) eval(expr)
        x
    }))$classes)
    expect_identical(infer(quote({
        x <- 1L
        y <- "ok"
        if (a) if (b) opaque(x)
        y
    }))$classes, "character")
})
