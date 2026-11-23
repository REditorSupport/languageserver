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
