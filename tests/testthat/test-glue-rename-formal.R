test_that("formal rename preserves partial, exact, positional, and dots argument matching in glue", {
    probe <- function(signature, expression, new, old = "long_name") {
        f <- provider_fixture(c(paste0("f <- function(", signature, ") NULL"),
                paste0('glue::glue("{', expression, '}")')))
        idx <- f$document$parse_data$reference_index
        hit <- which(idx$is_definition & idx$definition_kind == "formal" & idx$name == old)
        formal <- indexed_reference_locations(idx, f$uri, hit)[[1L]]
        glue_rename_formal_problem(f$workspace, f$document, formal, old, new)
    }
    expect_match(probe("long_name", "f(long_n = 1)", "amount"), "argument matching")
    expect_null(probe("long_name", "f(long_n = 1)", "long_new_name"))
    expect_null(probe("long_name", "f(long_name = 1)", "amount"))
    expect_match(probe("long_name, ...", "f(1, qty = 2)", "qty_ordered"), "argument matching")
    expect_match(probe("long_name, other", "f(ot = 1, 2)", "other_total"), "argument matching")
    expect_null(probe("..., long_name", "f(long_n = 1)", "amount"))
    expect_null(probe("long_name, longer", "f(long_name = 1, long = 2)", "amount"))
    expect_null(probe("long_name", "stats::median(x = 1, long_n = 2)", "amount"))
    expect_null(probe("long_name", "(function(f) f(long_n = 1))(identity)", "amount"))
    expect_null(probe("long_name", "(function(f) f(long_name = 1))(identity)", "amount"))
    expect_match(probe("long_name", "f('long_name' = 1)", "amount"), "argument matching")
})

test_that("unrelated embedded tags cannot capture a lexical binding", {
    f <- provider_fixture(c("outer <- 1", 'glue::glue("{stats::median(1, outer = outer)}")'))
    sites <- glue_reference_occurrences(f$workspace, f$document, "outer")
    expect_length(sites, 2L)
    is_tag <- vapply(sites, function(x) isTRUE(x$argument_tag), logical(1L))
    expect_identical(is_tag, c(TRUE, FALSE))
})
