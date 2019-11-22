context("Test Completion")

test_that("Simple completion works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "str",
            "file.c",
            "fs::path",
            "foo$sol",
            ".Mac"
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
    expect_length(result$items, 0)

    result <- client %>% respond_completion(temp_file, c(4, 4))
    expect_length(result$items %>% keep(~.$label == ".Machine"), 1)
})

test_that("Completion of function arguments works", {
    skip_on_cran()
    client <- language_client()
    
    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(
        c(
            "str(obj"
        ),
        temp_file)
    
    client %>% did_save(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 6))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)
})

test_that("Completion of user function works", {
    skip_on_cran()
    client <- language_client()

    withr::local_tempfile(c("temp_file"), fileext = ".R")
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

    expect_length(result$items %>% keep(~.$label == "my_fun"), 1)

})


test_that("Completion inside a package works", {
    skip_on_cran()
    wd <- path_real(path_package("languageserver", "projects", "mypackage"))
    client <- language_client(working_dir = wd)

    withr::local_tempfile(c("temp_file"), fileext = ".R")
    writeLines(c("noth"), temp_file)

    # client %>% did_save(path(wd, "R", "mypackage.R"))
    client %>% did_save(temp_file)
    result <- client %>% respond_completion(
        temp_file, c(0, 4),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0)

    expect_length(result$items %>% keep(~.$label == "nothing"), 1)
})

test_that("Completion item resolve works", {
    skip_on_cran()
    client <- language_client()
    
    withr::local_tempfile(c("temp_file"), fileext = ".R")
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
    expect_length(items, 1)
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
