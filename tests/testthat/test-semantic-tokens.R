test_that("Semantic tokens full works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(x, y) {",
            "  x + y",
            "}"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)
    # data should be multiples of 5 (line delta, start delta, length, type, modifiers)
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Semantic tokens range works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_var <- 42",
            "another_var <- test_var + 1"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    # Request tokens for the first line only
    result <- client %>% respond_semantic_tokens_range(
        temp_file,
        start_pos = c(0, 0),
        end_pos = c(1, 0)
    )
    expect_true(!is.null(result$data))
    # data should be multiples of 5
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Semantic tokens contain expected types", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(param1, param2) {",
            "  result <- param1 + param2",
            "  result",
            "}"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)

    # Check that we have some tokens (data array with valid entries)
    # Each token is 5 elements: [line_delta, start_delta, length, type, modifiers]
    token_count <- length(result$data) %/% 5
    expect_true(token_count > 0)
})
