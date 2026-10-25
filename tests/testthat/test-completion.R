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

    client %>% did_open(temp_file)

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

test_that("Simple completion is case insensitive", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "STR",
            "File.c",
            "fs::PATH",
            "foo$sol",
            ".mac",
            "grdev",
            "tru",
            "utils:::.gethelp",
            "utils::.gethelp",
            "utils::osver",
            "datasets::MTCAR"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 3))

    expect_length(result$items %>% keep(~ .$label == "strsplit"), 1)
    expect_length(result$items %>% keep(~ .$label == "strrep"), 1)

    result <- client %>% respond_completion(temp_file, c(1, 6))
    expect_length(result$items %>% keep(~ .$label == "file.choose"), 1)
    expect_length(result$items %>% keep(~ .$label == "file.create"), 1)

    result <- client %>% respond_completion(temp_file, c(2, 8))
    expect_true("path_real" %in% (result$items %>% map_chr(~ .$label)))

    result <- client %>% respond_completion(temp_file, c(3, 7))
    expect_length(result$items %>% discard(~ .$kind == CompletionItemKind$Text), 0)

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

test_that("Completion of attached package functions works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "library(jsonlite)",
            "require('xml2')",
            "fromJS",
            "read_xm"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(2, 6),
        retry_when = function(result) result$items %>% keep(~ .$label == "fromJSON") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "fromJSON"), 1)

    result <- client %>% respond_completion(temp_file, c(3, 7),
        retry_when = function(result) result$items %>% keep(~ .$label == "read_xml") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "read_xml"), 1)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "pacman::p_load(jsonlite, xml2)",
            "fromJS",
            "read_xm"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(1, 6),
        retry_when = function(result) result$items %>% keep(~ .$label == "fromJSON") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "fromJSON"), 1)

    result <- client %>% respond_completion(temp_file, c(2, 7),
        retry_when = function(result) result$items %>% keep(~ .$label == "read_xml") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "read_xml"), 1)
})

test_that("Completion of package functions attached in unscoped functions works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "suppressPackageStartupMessages(library(jsonlite))",
            "fromJS"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(1, 6),
        retry_when = function(result) result$items %>% keep(~ .$label == "fromJSON") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "fromJSON"), 1)

    writeLines(
        c(
            "suppressPackageStartupMessages({",
            "  library(jsonlite)",
            "  require('xml2')",
            "})",
            "fromJS",
            "read_xm"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(4, 6),
        retry_when = function(result) result$items %>% keep(~ .$label == "fromJSON") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "fromJSON"), 1)

    result <- client %>% respond_completion(temp_file, c(5, 7),
        retry_when = function(result) result$items %>% keep(~ .$label == "read_xml") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "read_xml"), 1)
})

test_that("Completion is robust to invalid source", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "library(jsonlite)",
            "require('xml2')",
            "require('')",
            "require('xml2', nonexist_arg = 0)",
            "fromJS",
            "read_xm"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(4, 6),
        retry_when = function(result) result$items %>% keep(~ .$label == "fromJSON") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "fromJSON"), 1)

    result <- client %>% respond_completion(temp_file, c(5, 7),
        retry_when = function(result) result$items %>% keep(~ .$label == "read_xml") %>% length() == 0)
    expect_length(result$items %>% keep(~ .$label == "read_xml"), 1)
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

test_that("Completion inside a package works", {
    skip_on_cran()
    wd <- path_real(path_package("languageserver", "projects", "mypackage"))
    client <- language_client(working_dir = wd)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("noth"), temp_file)

    # client %>% did_open(path(wd, "R", "mypackage.R"))
    client %>% did_open(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 4),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0 || length(result$items %>% keep(~.$label == "nothing")) == 0)

    expect_length(result$items %>% keep(~.$label == "nothing"), 1)
})

test_that("Completion of imported objects works inside a package", {
    skip_on_cran()
    wd <- path_real(path_package("languageserver", "projects", "mypackage"))
    client <- language_client(working_dir = wd)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("dic"), temp_file)

    # client %>% did_open(path(wd, "R", "mypackage.R"))
    client %>% did_open(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 3),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0 || length(result$items %>% keep(~.$label == "dict")) == 0)

    expect_length(result$items %>% keep(~.$label == "dict"), 1)

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c("lint_p"), temp_file)

    # client %>% did_open(path(wd, "R", "mypackage.R"))
    client %>% did_open(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 6),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0 || length(result$items %>% keep(~.$label == "lint_package")) == 0)

    expect_length(result$items %>% keep(~.$label == "lint_package"), 1)
})

test_that("Completion of re-exported objects works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "purrr::set_names"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 16))

    expect_length(result$items %>% keep(~ .$label == "set_names"), 1)
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

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(2, 7),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )

    expect_length(result$items %>% keep(~ .$label == "var1"), 1)
    expect_length(result$items %>% keep(~ .$label == "var2"), 1)
    expect_length(result$items %>% keep(~ .$label == "var3"), 1)
})
