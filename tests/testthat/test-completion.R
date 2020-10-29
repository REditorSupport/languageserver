context("Test Completion")

test_that("Simple completion works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str",
            "file.c",
            "fs::path",
            "foo$sol",
            ".Mac",
            "grDev",
            "TRU",
            "utils:::.getHelp",
            "utils::.getHelp",
            "utils::osVer",
            "datasets::mtcar"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 3))

    expect_length(result$items %>% keep(~.$label == "strsplit"), 1)
    expect_length(result$items %>% keep(~.$label == "strrep"), 1)

    result <- client %>% respond_completion(temp_file, c(1, 6))
    expect_length(result$items %>% keep(~.$label == "file.choose"), 1)
    expect_length(result$items %>% keep(~.$label == "file.create"), 1)

    result <- client %>% respond_completion(temp_file, c(2, 8))
    expect_true("path_real" %in% (result$items %>% map_chr(~.$label)))

    result <- client %>% respond_completion(temp_file, c(3, 7))
    expect_length(result$items %>% discard(~.$kind == CompletionItemKind$Text), 0)

    result <- client %>% respond_completion(temp_file, c(4, 4))
    expect_length(result$items %>% keep(~ .$label == ".Machine"), 1)

    result <- client %>% respond_completion(temp_file, c(5, 5))
    expect_length(result$items %>% keep(~ .$label == "grDevices"), 1)

    result <- client %>% respond_completion(temp_file, c(6, 3))
    expect_length(result$items %>% keep(~ .$label == "TRUE"), 1)

    result <- client %>% respond_completion(temp_file, c(7, 16))
    expect_length(result$items %>% keep(~ .$label == ".getHelpFile"), 1)

    result <- client %>% respond_completion(temp_file, c(8, 15))
    expect_length(result$items, 0)

    result <- client %>% respond_completion(temp_file, c(9, 12))
    expect_length(result$items %>% keep(~ .$label == "osVersion"), 1)

    result <- client %>% respond_completion(temp_file, c(10, 15))
    expect_length(result$items %>% keep(~ .$label == "mtcars"), 1)
})

test_that("Completion of function arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str(obj",
            "utils::str(obj",
            "str(stats::o"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 6))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(1, 14))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(2, 12))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 0)
})

test_that("Completion of local function arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "local({",
            "  test <- function(vararg1, vararg2=1) {",
            "    vararg1 + vararg2",
            "  }",
            "  test(vararg",
            "  )",
            "})"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(4, 13),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>% keep(~ .$label == "vararg1")
    expect_length(arg_items, 1)

    arg_items <- result$items %>% keep(~ .$label == "vararg2")
    expect_length(arg_items, 1)
})

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

test_that("Completion inside a package works", {
    skip_on_cran()
    wd <- path_real(path_package("languageserver", "projects", "mypackage"))
    client <- language_client(working_dir = wd)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("noth"), temp_file)

    # client %>% did_save(path(wd, "R", "mypackage.R"))
    client %>% did_save(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 4),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>% keep(~.$label == "nothing"), 1)
})

test_that("Completion of imported objects works inside a package", {
    skip_on_cran()
    wd <- path_real(path_package("languageserver", "projects", "mypackage"))
    client <- language_client(working_dir = wd)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("dic"), temp_file)

    # client %>% did_save(path(wd, "R", "mypackage.R"))
    client %>% did_save(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 3),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>% keep(~.$label == "dict"), 1)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("lint_p"), temp_file)

    # client %>% did_save(path(wd, "R", "mypackage.R"))
    client %>% did_save(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 6),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>% keep(~.$label == "lint_package"), 1)
})

test_that("Completion of tokens in document works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "df1 <- data.frame(var1 = 1:10, var2 = 10:1)",
            "df1$var3 <- rnorm(10)",
            "df1$var"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(2, 7),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )

    expect_length(result$items %>% keep(~ .$label == "var1"), 1)
    expect_length(result$items %>% keep(~ .$label == "var2"), 1)
    expect_length(result$items %>% keep(~ .$label == "var3"), 1)
})

test_that("Completion item resolve works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "bas", # package: base
            "mtcars", # lazydata: mtcars
            "basename", # function: basename
            "basename(path", # function paraemter
            ".Mac" # non-functon: .Machine
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 2))
    items <- result$items %>% keep(~.$label == "base")
    # normally, we should do `expect_length(items, 1)`, but a bad interaction betwen
    # packrat and callr could result in two `base` namespaces
    # https://github.com/r-lib/callr/issues/131
    expect_gt(length(items), 0)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_equal(resolve_result$documentation$value, "**The R Base Package**\n\nBase R functions.")

    result <- client %>% respond_completion(temp_file, c(1, 5))
    items <- result$items %>% keep(~.$label == "mtcars")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "The data was extracted from the 1974 Motor Trend US magazine")

    result <- client %>% respond_completion(temp_file, c(2, 7))
    items <- result$items %>% keep(~ .$label == "basename")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "`basename` removes all of the path up to and including the last path separator")

    result <- client %>% respond_completion(temp_file, c(3, 12))
    items <- result$items %>% keep(~ .$label == "path")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "character vector, containing path names.")

    result <- client %>% respond_completion(temp_file, c(4, 3))
    items <- result$items %>% keep(~ .$label == ".Machine")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "`.Machine` is a variable holding information on the numerical characteristics of the machine \\*\\*R\\*\\* is running on")
})

test_that("Completion item resolve extracts symbol documentation", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# comment",
            "testvar <- 1",
            "testva"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(2, 6),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    items <- result$items %>% keep(~ .$label == "testvar")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "comment")
})

test_that("Completion item resolve extracts function documentation", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "#' test",
            "#' @param var1 a number",
            "testfun <- function(var1 = 1) {",
            "  var1 + 1",
            "}",
            "testfun(var1",
            ")"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 6),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    items <- result$items %>% keep(~ .$label == "testfun")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "test")

    result <- client %>% respond_completion(
        temp_file, c(5, 12),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    items <- result$items %>% keep(~ .$label == "var1")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "a number")
})

test_that("Completion item resolve extracts local function documentation", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "local({",
            "  #' test",
            "  #' @param var1 a number",
            "  testfun <- function(var1 = 1) {",
            "    var1 + 1",
            "  }",
            "  testfun(var1",
            "  )",
            "})"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(6, 8),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    items <- result$items %>% keep(~ .$label == "testfun")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "test")

    result <- client %>% respond_completion(
        temp_file, c(6, 14),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    items <- result$items %>% keep(~ .$label == "var1")
    expect_length(items, 1)
    resolve_result <- client %>% respond_completion_item_resolve(items[[1]])
    expect_equal(resolve_result$documentation$kind, "markdown")
    expect_match(resolve_result$documentation$value,
        "a number")
})

test_that("Completion in Rmarkdown works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".Rmd")
    writeLines(
        c(
            "Title",
            "",
            "```{r}",
            "str",
            "file.c",
            "fs::path",
            "foo$sol",
            ".Mac",
            "grDev",
            "TRU",
            "```",
            "str"
        ),
        temp_file
    )

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(3, 3))

    expect_length(result$items %>% keep(~ .$label == "strsplit"), 1)
    expect_length(result$items %>% keep(~ .$label == "strrep"), 1)

    result <- client %>% respond_completion(temp_file, c(4, 6))
    expect_length(result$items %>% keep(~ .$label == "file.choose"), 1)
    expect_length(result$items %>% keep(~ .$label == "file.create"), 1)

    result <- client %>% respond_completion(temp_file, c(5, 8))
    expect_true("path_real" %in% (result$items %>% map_chr(~ .$label)))

    result <- client %>% respond_completion(temp_file, c(6, 7))
    expect_length(result$items %>% discard(~ .$kind == CompletionItemKind$Text), 0)

    result <- client %>% respond_completion(temp_file, c(7, 4))
    expect_length(result$items %>% keep(~ .$label == ".Machine"), 1)

    result <- client %>% respond_completion(temp_file, c(8, 5))
    expect_length(result$items %>% keep(~ .$label == "grDevices"), 1)

    result <- client %>% respond_completion(temp_file, c(9, 3))
    expect_length(result$items %>% keep(~ .$label == "TRUE"), 1)

    result <- client %>% respond_completion(temp_file, c(10, 3))
    expect_length(result$items, 0)

    result <- client %>% respond_completion(temp_file, c(11, 3))
    expect_length(result$items, 0)
})
