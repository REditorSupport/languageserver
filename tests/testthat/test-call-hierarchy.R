context("Test call hierarchy")

test_that("Call hierarchy works in single file", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- function(x) { foo(x) + 1 }",
        "fun <- function(x) { foo(x) + bar(x) + 1 }"
    ), single_file)

    client %>% did_save(single_file)

    result <- client %>% respond_prepare_call_hierarchy(
        single_file, c(0, 1), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "foo")
    expect_equal(result[[1]]$kind, SymbolKind$Function)
    expect_equal(result[[1]]$uri, path_to_uri(single_file))
    expect_equal(result[[1]]$range, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))
    expect_equal(result[[1]]$selectionRange, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))

    result <- client %>% respond_prepare_call_hierarchy(
        single_file, c(1, 22), retry_when = function(result) length(result) == 0)
    expect_length(result, 1)
    expect_equal(result[[1]]$name, "foo")
    expect_equal(result[[1]]$kind, SymbolKind$Function)
    expect_equal(result[[1]]$uri, path_to_uri(single_file))
    expect_equal(result[[1]]$range, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))
    expect_equal(result[[1]]$selectionRange, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))

    result <- client %>% respond_prepare_call_hierarchy(
        single_file, c(2, 32), retry_when = function(result) length(result) == 0)
    expect_length(result, 1)
    expect_equal(result[[1]]$name, "bar")
    expect_equal(result[[1]]$kind, SymbolKind$Function)
    expect_equal(result[[1]]$uri, path_to_uri(single_file))
    expect_equal(result[[1]]$range, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_equal(result[[1]]$selectionRange, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
})

test_that("Call hierarchy works in multiple files", {
    skip_on_cran()
    client <- language_client()

    file1 <- withr::local_tempfile(fileext = ".R")
    file2 <- withr::local_tempfile(fileext = ".R")

    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- function(x) { foo(x) + 1 }"
    ), file1)

    writeLines(c(
        "fun <- function(x) { foo(x) + bar(x) + 1 }"
    ), file2)

    client %>% did_save(file1)
    client %>% did_save(file2)

    result <- client %>% respond_prepare_call_hierarchy(
        file2, c(0, 22), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "foo")
    expect_equal(result[[1]]$kind, SymbolKind$Function)
    expect_equal(result[[1]]$uri, path_to_uri(file1))
    expect_equal(result[[1]]$range, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))
    expect_equal(result[[1]]$selectionRange, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))

    result <- client %>% respond_prepare_call_hierarchy(
        file2, c(0, 32), retry_when = function(result) length(result) == 0)
    expect_length(result, 1)
    expect_equal(result[[1]]$name, "bar")
    expect_equal(result[[1]]$kind, SymbolKind$Function)
    expect_equal(result[[1]]$uri, path_to_uri(file1))
    expect_equal(result[[1]]$range, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_equal(result[[1]]$selectionRange, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
})

test_that("Call hierarchy incoming calls works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- function(x) { foo(x) + 1 }",
        "fun <- function(x) { foo(x) + bar(x) + foo(bar(x)) }"
    ), single_file)

    client %>% did_save(single_file)

    items <- client %>% respond_prepare_call_hierarchy(
        single_file, c(0, 1), retry_when = function(result) length(result) == 0)

    result <- client %>% respond_call_hierarchy_incoming_calls(
        items[[1]], retry_when = function(result) length(result) == 0)

    expect_length(result, 2)

    result1 <- result %>% keep(~ .$from$name == "bar")
    expect_equal(result1[[1]]$from$name, "bar")
    expect_equal(result1[[1]]$from$kind, SymbolKind$Function)
    expect_equal(result1[[1]]$from$uri, path_to_uri(single_file))
    expect_equal(result1[[1]]$from$range, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_equal(result1[[1]]$from$selectionRange, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_length(result1[[1]]$fromRanges, 1)
    expect_equal(result1[[1]]$fromRanges[[1]], list(
        start = list(line = 1, character = 21),
        end = list(line = 1, character = 24)
    ))

    result2 <- result %>% keep(~ .$from$name == "fun")
    expect_equal(result2[[1]]$from$name, "fun")
    expect_equal(result2[[1]]$from$kind, SymbolKind$Function)
    expect_equal(result2[[1]]$from$uri, path_to_uri(single_file))
    expect_equal(result2[[1]]$from$range, list(
        start = list(line = 2, character = 0),
        end = list(line = 2, character = 52)
    ))
    expect_equal(result2[[1]]$from$selectionRange, list(
        start = list(line = 2, character = 0),
        end = list(line = 2, character = 52)
    ))
    expect_length(result2[[1]]$fromRanges, 2)
    expect_equal(result2[[1]]$fromRanges[[1]], list(
        start = list(line = 2, character = 21),
        end = list(line = 2, character = 24)
    ))
    expect_equal(result2[[1]]$fromRanges[[2]], list(
        start = list(line = 2, character = 39),
        end = list(line = 2, character = 42)
    ))
})


test_that("Call hierarchy outgoing calls works", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- function(x) { foo(x) + 1 }",
        "fun <- function(x) { foo(x) + bar(x) + foo(bar(x)) }"
    ), single_file)

    client %>% did_save(single_file)

    items <- client %>% respond_prepare_call_hierarchy(
        single_file, c(2, 1), retry_when = function(result) length(result) == 0)

    result <- client %>% respond_call_hierarchy_outgoing_calls(
        items[[1]], retry_when = function(result) length(result) == 0)

    expect_length(result, 2)

    result1 <- result %>% keep(~ .$to$name == "foo")
    print(result1)

    expect_equal(result1[[1]]$to$name, "foo")
    expect_equal(result1[[1]]$to$kind, SymbolKind$Function)
    expect_equal(result1[[1]]$to$uri, path_to_uri(single_file))
    expect_equal(result1[[1]]$to$range, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))
    expect_equal(result1[[1]]$to$selectionRange, list(
        start = list(line = 0, character = 0),
        end = list(line = 0, character = 28)
    ))
    expect_length(result1[[1]]$fromRanges, 2)
    expect_equal(result1[[1]]$fromRanges[[1]], list(
        start = list(line = 2, character = 21),
        end = list(line = 2, character = 24)
    ))
    expect_equal(result1[[1]]$fromRanges[[2]], list(
        start = list(line = 2, character = 39),
        end = list(line = 2, character = 42)
    ))

    result2 <- result %>% keep(~ .$to$name == "bar")
    expect_equal(result2[[1]]$to$name, "bar")
    expect_equal(result2[[1]]$to$kind, SymbolKind$Function)
    expect_equal(result2[[1]]$to$uri, path_to_uri(single_file))
    expect_equal(result2[[1]]$to$range, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_equal(result2[[1]]$to$selectionRange, list(
        start = list(line = 1, character = 0),
        end = list(line = 1, character = 33)
    ))
    expect_length(result2[[1]]$fromRanges, 2)
    expect_equal(result2[[1]]$fromRanges[[1]], list(
        start = list(line = 2, character = 30),
        end = list(line = 2, character = 33)
    ))
    expect_equal(result2[[1]]$fromRanges[[2]], list(
        start = list(line = 2, character = 43),
        end = list(line = 2, character = 46)
    ))
})
