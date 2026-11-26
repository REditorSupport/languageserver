test_that("formal Rename in glue follows the function selected by an embedded mask", {
    f <- provider_fixture(c("f <- function(arg) arg",
            'glue::glue("{base::with(list(f=function(arg)2), f(arg=2))}")',
            'glue::glue("{base::with(list(f=function(arg)2), f(\'arg\'=2))}")',
            'glue::glue("{base::with(list(f=1), f(arg=2))}")',
            'glue::glue("{f(arg=2)}")',
            "f(arg=1)"))
    f$workspace$get_definition <- function(...) NULL
    query <- function(point) rename_reply(1L, f$uri, f$workspace, f$document, point, "beta")
    expected <- list(range(position(0, 14), position(0, 17)), range(position(0, 19), position(0, 22)),
        range(position(3, 37), position(3, 40)), range(position(4, 15), position(4, 18)),
        range(position(5, 2), position(5, 5)))
    for (point in list(list(row = 0L, col = 15L), list(row = 3L, col = 38L))) {
        result <- query(point)
        expect_null(result$error)
        edits <- lapply(result$result$changes[[f$uri]], "[[", "range")
        expect_setequal(edits, expected)
    }
    expect_null(query(list(row = 1L, col = 51L))$result$changes)
})

test_that("uncertain embedded callees protect the matching outer formal", {
    for (label in c("arg", "'arg'", "ar")) {
        f <- provider_fixture(c("f <- function(arg) arg", "g <- function(arg) arg",
                sprintf('glue::glue("{base::with(unknown, f(%s=2))}")', label)))
        f$workspace$get_definition <- function(...) NULL
        rename <- function(line) rename_reply(1L, f$uri, f$workspace, f$document, list(row = line, col = 15L), "beta")
        expect_match(rename(0L)$error$message, "argument matching")
        unrelated <- rename(1L)
        expect_null(unrelated$error)
        expect_length(unrelated$result$changes[[f$uri]], 2L)
    }
})
