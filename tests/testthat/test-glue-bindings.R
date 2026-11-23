test_that("glue resolution handles named argument order, static masks, and host formals", {
    f <- provider_fixture(c("f <- function(v) {", '  data <- list(v = "column", code = 7)',
            '  data %>% glue::glue_data("{v}{code}{extra}", v = v, extra = v)', "}",
            'stringr::str_glue("{f}")'))
    fragments <- f$document$parse_data$glue_templates
    ctx <- function(fragment, name) glue_context_at(f$workspace, f$document, fragment, name)
    expect_identical(ctx(fragments[[1L]], "v")$target$kind, "named")
    expect_identical(ctx(fragments[[2L]], "code")$target$kind, "field")
    expect_setequal(ctx(fragments[[1L]], NULL)$names, c("v", "code", "extra"))
    first <- list(call_row = fragments[[1L]]$call_row, before = 2L)
    expect_identical(ctx(first, "v")$target$kind, "field")
    first$before <- 3L
    expect_identical(ctx(first, "v")$target$kind, "named")
    expect_identical(ctx(fragments[[4L]], "f")$target$key, "global:f")
})

test_that("dynamic glue environments block lexical lookup and source mutation invalidates known fields", {
    f <- provider_fixture(c("x <- 1", 'glue::glue("{x}", .envir = get_env())',
            'glue::glue("{x}", .envir = environment())', "data <- list(x = 1)", "data$x <- NULL",
            'glue::glue_data(data, "{x}")', 'glue::glue_data(make_data(), "{x}", x = 2)'))
    frags <- f$document$parse_data$glue_templates
    targets <- lapply(frags, function(x) glue_context_at(f$workspace, f$document, x, "x")$target)
    expect_identical(vapply(targets, "[[", "", "kind"), c("unknown", "lexical", "unknown", "named"))
})

test_that("function lookup skips proven non-functions and stops at dynamic glue bindings", {
    f <- provider_fixture(c("f <- function(x) x + 10L", 'glue::glue("{f(3)} {f}", f = 4)',
            'glue::glue("{f(3)}", f = (function(y) y), f = 4)', "d <- list(sum = 4)",
            'glue::glue_data(d, "{sum(3)}")', 'glue::glue("{f(3)}", f = unknown())',
            'glue::glue("{alias(3)}", original = f, alias = original)',
            'demo <- function() { f <- 4; glue::glue("{f(3)}") }'))
    fragments <- f$document$parse_data$glue_templates
    lookup <- function(i, name, call = TRUE) glue_context_at(f$workspace, f$document, fragments[[i]], name, call)
    expect_identical(lookup(1, "f")$target$key, "global:f")
    expect_identical(lookup(2, "f", FALSE)$target$kind, "named")
    earlier <- lookup(3, "f")$target
    expect_identical(earlier$kind, "named")
    expect_identical(earlier$range$start$character, regexpr("f =", f$document$content[[3L]], fixed = TRUE)[[1L]] - 1L)
    expect_identical(lookup(4, "sum")$target$key, "package:base:sum")
    expect_identical(lookup(5, "f")$target$kind, "unknown")
    expect_true(lookup(5, "f")$blocked)
    expect_identical(lookup(6, "alias")$target$callable$key, "global:f")
    expect_identical(lookup(7, "f")$target$key, "global:f")
})
