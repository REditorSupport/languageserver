test_that("Completion of user function works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_fun <- function(x) {}",
            "my_f"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(1, 4),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>%
            keep(~ .$label == "my_fun") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)

})

test_that("Completion of user function contains no duplicate symbols", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_fun <- function(x) {}",
            "my_fun <- function(x) {}",
            "my_f"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(2, 4),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>%
            keep(~ .$label == "my_fun") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)

})

test_that("Completion of symbols in scope works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "xvar0 <- rnorm(10)",
            "my_fun <- function(xvar1) {",
            "    xvar2 = 1",
            "    2 -> xvar3",
            "    for (xvar4 in 1:10) {",
            "        xvar",
            "    }",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 12),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )

    expect_length(result$items %>% discard(~ .$kind == CompletionItemKind$Text), 5)
    expect_length(result$items %>%
            keep(~ .$label == "xvar0") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar1") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar2") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar3") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar4") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
})

test_that("Completion of symbols in scope works with semi-colons", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "xvar0 <- rnorm(10);",
            "my_fun <- function(xvar1) {",
            "    xvar2 = 1;",
            "    2 -> xvar3;",
            "    for (xvar4 in 1:10) {",
            "        xvar",
            "    }",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 12),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )

    expect_length(result$items %>% discard(~ .$kind == CompletionItemKind$Text), 5)
    expect_length(result$items %>%
            keep(~ .$label == "xvar0") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar1") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar2") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar3") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
    expect_length(result$items %>%
            keep(~ .$label == "xvar4") %>%
            discard(~ .$kind == CompletionItemKind$Text), 1)
})
