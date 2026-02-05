test_that("Inlay hint works for function parameters", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test function with multiple parameters",
            "result <- plot(mtcars$wt, mtcars$mpg, \"red\", 16, 2)",
            "",
            "# Test with some named parameters",
            "data <- read.csv(\"data.csv\", TRUE)",
            "",
            "# Test with built-in function",
            "text <- paste(\"hello\", \"world\", \"-\")"
        ),
        temp_file)

    client %>% did_save(temp_file)

    # Request inlay hints for the entire document
    result <- client %>% respond_inlay_hint(
        temp_file,
        range = list(
            start = list(line = 0, character = 0),
            end = list(line = 7, character = 0)
        )
    )

    # Should have hints for unnamed parameters
    expect_true(length(result) > 0)
    
    # Check that we have parameter hints
    param_hints <- result %>% keep(~ .$kind == 2)  # InlayHintKind$Parameter
    expect_true(length(param_hints) > 0)
    
    # Verify hints contain parameter names with colons
    labels <- sapply(param_hints, function(h) h$label)
    expect_true(any(grepl(":", labels)))
})

test_that("Inlay hint skips named parameters", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# All parameters are named - should have no hints",
            "result <- plot(x = mtcars$wt, y = mtcars$mpg, col = \"red\")"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_inlay_hint(
        temp_file,
        range = list(
            start = list(line = 0, character = 0),
            end = list(line = 2, character = 0)
        )
    )

    # Should have fewer or no hints since parameters are named
    # We might still get hints on line 0 (the comment line wouldn't have any)
    param_hints <- result %>% keep(~ .$kind == 2)
    expect_true(length(param_hints) == 0)
})

test_that("Inlay hint works for return types", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test return type hints for common functions",
            "df <- read.csv(\"data.csv\")",
            "model <- lm(mpg ~ wt, data = mtcars)",
            "vec <- c(1, 2, 3)",
            "txt <- paste(\"a\", \"b\")"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_inlay_hint(
        temp_file,
        range = list(
            start = list(line = 0, character = 0),
            end = list(line = 5, character = 0)
        )
    )

    # Should have type hints
    type_hints <- result %>% keep(~ .$kind == 1)  # InlayHintKind$Type
    expect_true(length(type_hints) > 0)
    
    # Check that hints contain type information
    labels <- sapply(type_hints, function(h) h$label)
    # Should have format like " → data.frame"
    expect_true(any(grepl("→", labels)))
})

test_that("Inlay hint respects range parameter", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Line 0",
            "result1 <- plot(mtcars$wt, mtcars$mpg)",  # Line 1
            "result2 <- plot(iris$Sepal.Length, iris$Sepal.Width)",  # Line 2
            "result3 <- plot(1, 2)"  # Line 3
        ),
        temp_file)

    client %>% did_save(temp_file)

    # Request hints only for line 1
    result <- client %>% respond_inlay_hint(
        temp_file,
        range = list(
            start = list(line = 1, character = 0),
            end = list(line = 2, character = 0)
        )
    )

    # Should only have hints from the specified range
    if (length(result) > 0) {
        # All hints should be on line 1
        lines <- sapply(result, function(h) h$position$line)
        expect_true(all(lines == 1))
    }
})

test_that("Inlay hint handles nested function calls", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Nested function calls",
            "result <- mean(c(1, 2, 3), TRUE)"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_inlay_hint(
        temp_file,
        range = list(
            start = list(line = 0, character = 0),
            end = list(line = 2, character = 0)
        )
    )

    # Should handle nested calls and provide hints for both functions
    # At minimum, should not error
    expect_true(is.list(result))
})
