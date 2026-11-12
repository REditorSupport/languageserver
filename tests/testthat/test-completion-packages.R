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
