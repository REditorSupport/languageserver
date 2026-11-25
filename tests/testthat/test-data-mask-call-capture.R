test_that("renaming a caller function cannot redirect its calls to a masked function", {
    f <- provider_fixture(c("g <- function() 1", "base::with(list(f = function() 2), g())", "g()"))
    p <- list(row = 0L, col = 0L)
    for (indexed in c(TRUE, FALSE)) {
        if (!indexed) f$document$parse_data$reference_index <- NULL
        blocked <- rename_reply(1L, f$uri, f$workspace, f$document, p, "f")
        expect_null(blocked$result$changes)
        expect_match(blocked$error$message, "binding|different function")
        allowed <- rename_reply(1L, f$uri, f$workspace, f$document, p, "fresh")
        expect_length(allowed$result$changes[[f$uri]], 3L)
    }
})

test_that("mapped glue calls retain data-mask function lookup during Rename", {
    rename <- function(expression, new) {
        f <- provider_fixture(c("g <- function() 1", paste0("glue::glue('{", expression, "}')"), "g()"))
        rename_reply(1L, f$uri, f$workspace, f$document, list(row = 0L, col = 0L), new)
    }
    own <- rename("base::with(list(g = function() 2), g())", "outer")
    expect_equal(vapply(own$result$changes[[1L]], function(edit) edit$range$start$line, numeric(1L)), c(0L, 2L))
    expect_null(rename("base::with(unknown, g())", "outer")$result$changes)
    blocked <- rename("base::with(list(f = function() 2), g())", "f")
    expect_match(blocked$error$message, "different function")
    expect_length(rename("base::with(list(f = function() 2), g())", "fresh")$result$changes[[1L]], 3L)
})
