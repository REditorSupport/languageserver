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

test_that("Completion of function arguments works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str(obj",
            "utils::str(obj",
            "str(stats::o",
            "seq.int(fr"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 6))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(1, 14))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(2, 12))
    arg_items <- result$items %>% keep(~.$label == "object")
    expect_length(arg_items, 0)

    result <- client %>% respond_completion(temp_file, c(3, 10))
    arg_items <- result$items %>% keep(~ .$label == "from")
    expect_length(arg_items, 1)
})

test_that("Completion of function arguments is case insensitive", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "str(OBJ",
            "utils::str(OBJ",
            "str(stats::O"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 6))
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(1, 14))
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 1)

    result <- client %>% respond_completion(temp_file, c(2, 12))
    arg_items <- result$items %>% keep(~ .$label == "object")
    expect_length(arg_items, 0)
})

test_that("Completion of options works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "options(sci",
            "options(scipen = 999, useFancy"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 11))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, "scipen")

    result <- client %>% respond_completion(temp_file, c(1, 30))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, "useFancyQuotes")
})

test_that("Completion of function arguments preserves the order of arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "eval(",
            "formatC(",
            "print.default(",
            "seq.int("
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(temp_file, c(0, 5))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(eval)))

    result <- client %>% respond_completion(temp_file, c(1, 8))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(formatC)))

    result <- client %>% respond_completion(temp_file, c(2, 14))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(print.default)))

    result <- client %>% respond_completion(temp_file, c(3, 8))
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, names(formals(args(seq.int))))
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

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(4, 13),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>% keep(~ .$label == "vararg1")
    expect_length(arg_items, 1)

    arg_items <- result$items %>% keep(~ .$label == "vararg2")
    expect_length(arg_items, 1)
})

test_that("Completion of local function arguments works in untitled documents", {
    skip_on_cran()
    client <- language_client()

    uri <- "untitled:Untitled-1"

    client %>% did_open(uri = uri, text = c(
        "local({",
        "  test <- function(vararg1, vararg2=1) {",
        "    vararg1 + vararg2",
        "  }",
        "  test(vararg",
        "  )",
        "})"
    ))

    result <- client %>% respond_completion(
        NULL, c(4, 13), uri = uri,
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>% keep(~ .$label == "vararg1")
    expect_length(arg_items, 1)

    arg_items <- result$items %>% keep(~ .$label == "vararg2")
    expect_length(arg_items, 1)
})

test_that("Completion of user function arguments preserves the order of arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(var3, var2, var1) {",
            "  var1 + var2 + var3",
            "}",
            "test(",
            ")"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(3, 5),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    arg_items <- result$items %>%
        keep(~ identical(.$data$type, "parameter")) %>%
        map_chr(~ .$label)
    expect_identical(arg_items, c("var3", "var2", "var1"))
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

    client %>% did_open(temp_file)

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

    client %>% did_open(temp_file)

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

    client %>% did_open(temp_file)

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

    client %>% did_open(temp_file)

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

    client %>% did_open(temp_file)

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

test_that("Completion in Rmarkdown specified by languageId works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".md")
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

    client %>% did_open(temp_file, languageId = "rmd")

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

test_that("Completion of argument values from defaults works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test function with default values",
            "my_func <- function(method = c('auto', 'manual', 'custom')) {",
            "  method <- match.arg(method)",
            "  method",
            "}",
            "",
            "# Test completion with named argument",
            "my_func(method = a)",
            "",
            "# Test completion with positional argument (first position)",
            "my_func(m)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    # Test named argument completion
    result <- client %>% respond_completion(
        temp_file, c(7, 17),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # With simplified implementation, substring 'a' matches 'auto' and 'manual'
    expect_true("auto" %in% labels)
    expect_true("manual" %in% labels)
    
    # Check that insertText is properly quoted
    insert_texts <- value_items %>% map_chr(~ .$insertText)
    expect_true('"auto"' %in% insert_texts)
    
    # Test positional argument completion
    result <- client %>% respond_completion(
        temp_file, c(10, 8),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'm' matches 'manual' and 'custom'
    expect_true("manual" %in% labels)
    expect_true("custom" %in% labels)
})

test_that("Completion of argument values with partial match works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(type = c('normal', 'special', 'advanced')) {",
            "  type <- match.arg(type)",
            "  type",
            "}",
            "",
            "my_func(type = a )"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 15),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'a' matches all values: 'normal', 'special', 'advanced'
    expect_true("advanced" %in% labels)
    expect_true("normal" %in% labels)
    expect_true("special" %in% labels)
})

test_that("Completion of argument values works with base R functions", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test with memCompress() which has type parameter with character vector defaults",
            "memCompress(raw(10), type = gz)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(1, 30),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # memCompress has type parameter with values "gzip", "bzip2", "xz", "zstd", "none"
    expect_true("gzip" %in% labels)
    expect_true(length(labels) > 0)
})

test_that("Completion of argument values for multiple parameter function", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(x, mode = c('read', 'write', 'append'), ",
            "                      style = c('plain', 'fancy')) {",
            "  mode <- match.arg(mode)",
            "  style <- match.arg(style)",
            "  list(x, mode, style)",
            "}",
            "",
            "# Test second argument",
            "test_func(1, rea)",
            "",
            "# Test third argument with named param",
            "test_func(mode = wri, style = pla)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    # Test second argument (mode) - positional
    result <- client %>% respond_completion(
        temp_file, c(8, 15),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'rea' matches 'read' only
    expect_true("read" %in% labels)
    expect_false("write" %in% labels)
    expect_false("append" %in% labels)
    expect_false("plain" %in% labels)
    expect_false("fancy" %in% labels)
    
    # Test third argument (style) - using named parameter
    result <- client %>% respond_completion(
        temp_file, c(11, 32),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'pla' matches 'plain' only
    expect_true("plain" %in% labels)
    expect_false("append" %in% labels)
    expect_false("fancy" %in% labels)
})

test_that("Completion of argument values works with named arguments out of order", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(a = 1, b = c('x', 'y', 'z'), c = 3) {",
            "  b <- match.arg(b)",
            "  b",
            "}",
            "",
            "# Named argument out of order",
            "test_func(c = 5, b = \"\")"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(6, 21),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # With empty string token, all values from all parameters are shown
    expect_true("x" %in% labels)
    expect_true("y" %in% labels)
    expect_true("z" %in% labels)
})

test_that("Completion of argument values is case insensitive", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(method = c('Auto', 'Manual', 'Custom')) {",
            "  method <- match.arg(method)",
            "  method",
            "}",
            "",
            "my_func(method = M)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 17),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'M' (case insensitive) matches 'Manual' and 'Custom'
    expect_true("Manual" %in% labels)
    expect_true("Custom" %in% labels)
})

test_that("No argument value completion for non-character defaults", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Function with numeric default",
            "my_func <- function(x = 10, y = c(1, 2, 3)) {",
            "  x + y",
            "}",
            "",
            "my_func(x = a)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 12),
        retry_when = function(result) length(result) == 0
    )
    
    # Should not have argument_value completions for numeric defaults
    value_items <- result$items %>% keep(~ !is.null(.$data$type) && .$data$type == "argument_value")
    expect_length(value_items, 0)
})

test_that("Completion of argument values works with positional arguments", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "fun0 <- function(x, status = c('running', 'done', 'error')) {",
            "  status <- match.arg(status)",
            "  status",
            "}",
            "",
            "fun0(1, run)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 11),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'run' matches 'running'
    expect_true("running" %in% labels)
    expect_false("done" %in% labels)
    expect_false("error" %in% labels)
})

test_that("Completion of argument values with positional partial match works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(mode = c('read', 'write', 'append')) {",
            "  mode <- match.arg(mode)",
            "  mode",
            "}",
            "",
            "my_func(r)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 9),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'r' matches 'read' and 'write'
    expect_true("read" %in% labels)
    expect_true("write" %in% labels)
    expect_false("append" %in% labels)
})

test_that("Completion of argument values for positional in multi-parameter function", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_func <- function(x, mode = c('fast', 'slow'), style = c('plain', 'fancy')) {",
            "  mode <- match.arg(mode)",
            "  style <- match.arg(style)",
            "  list(x, mode, style)",
            "}",
            "",
            "# Should suggest values from both mode and style parameters",
            "test_func(1, fa)"
        ),
        temp_file)

    client %>% did_open(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(7, 14),
        retry_when = function(result) {
            length(result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")) == 0
        }
    )
    
    value_items <- result$items %>% keep(~ !is.null(.$data) && .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Substring 'fa' matches 'fast' and 'fancy' from both parameters
    expect_true("fast" %in% labels)
    expect_true("fancy" %in% labels)
    # Should not include values that don't contain 'fa'
    expect_false("slow" %in% labels)
    expect_false("plain" %in% labels)
})

test_that("Completion providers use precomputed document indexes", {
    fixture <- provider_fixture(c(
        "xvar0 <- rnorm(10)",
        "my_fun <- function(xvar1) {",
        "    xvar2 = 1",
        "    2 -> xvar3",
        "    for (xvar4 in 1:10) {",
        "        xvar",
        "    }",
        "}"
    ))
    parse_data <- fixture$workspace$get_parse_data(fixture$uri)

    # Prove these providers do not need to traverse the XML document.
    parse_data$xml_doc <- NULL
    scope_items <- scope_completion(
        fixture$uri,
        fixture$workspace,
        "xvar",
        list(row = 5L, col = 12L)
    )
    scope_labels <- vapply(scope_items, `[[`, character(1L), "label")
    expect_setequal(scope_labels, c("xvar1", "xvar2", "xvar3", "xvar4"))

    token_items <- token_completion(
        fixture$uri, fixture$workspace, "xvar")
    token_labels <- vapply(token_items, `[[`, character(1L), "label")
    expect_setequal(token_labels,
        c("xvar0", "xvar1", "xvar2", "xvar3", "xvar4", "xvar"))
})

test_that("Completion providers bound broad result sets early", {
    variables <- sprintf("    value_%04d <- %d", 1:500, 1:500)
    fixture <- provider_fixture(c(
        "my_fun <- function() {",
        variables,
        "    value_",
        "}"
    ))

    items <- scope_completion(
        fixture$uri,
        fixture$workspace,
        "value_",
        list(row = 501L, col = 10L),
        limit = 20L
    )

    expect_length(items, 20L)
    expect_true(isTRUE(attr(items, "truncated")))
    expect_equal(
        sort(vapply(items, `[[`, character(1L), "label")),
        sprintf("value_%04d", 1:20)
    )

    namespace <- new.env(parent = baseenv())
    namespace$get_symbols <- function(want_functs, ...) {
        if (want_functs) {
            sprintf("value_function_%04d", 500:1)
        } else {
            sprintf("value_field_%04d", 500:1)
        }
    }
    namespace$get_lazydata <- function() sprintf("value_data_%04d", 500:1)
    workspace <- new.env(parent = baseenv())
    workspace$get_namespace <- function(...) namespace

    workspace_items <- workspace_completion(
        workspace,
        "value_",
        package = "example",
        exported_only = TRUE,
        limit = 20L
    )
    all_labels <- c(
        namespace$get_symbols(TRUE),
        namespace$get_symbols(FALSE),
        namespace$get_lazydata()
    )
    expected <- all_labels[
        order(paste0(sort_prefixes$global, all_labels), method = "radix")
    ][1:20]

    expect_length(workspace_items, 20L)
    expect_true(isTRUE(attr(workspace_items, "truncated")))
    expect_equal(
        vapply(workspace_items, `[[`, character(1L), "label"),
        expected
    )
})

test_that("Completion candidate selection uses stable UTF-8 radix ordering", {
    labels <- c("zeta", "äther", "Alpha", ".hidden", "_private", "alpha")
    sort_text <- paste0(sort_prefixes$global, labels)
    token <- "a"
    expected <- order(
        !startsWith(labels, token), sort_text, method = "radix")[1:4]

    expect_identical(
        completion_select_indices(labels, sort_text, token, 4L),
        expected
    )
})

test_that("Argument value completion resolves formals once", {
    calls <- 0L
    workspace <- new.env(parent = baseenv())
    workspace$guess_namespace <- function(...) "example"
    workspace$get_formals <- function(...) {
        calls <<- calls + 1L
        alist(
            method = c("auto", "manual"),
            style = c("plain", "fancy")
        )
    }

    items <- arg_value_completion(
        NULL, workspace, NULL, NULL, "a", "my_fun")

    expect_equal(calls, 1L)
    expect_setequal(
        vapply(items, `[[`, character(1L), "label"),
        c("auto", "manual", "plain", "fancy")
    )
})

test_that("Completion parse index handles supported symbol forms", {
    parse_data <- parse_document("file:///completion-index.R", c(
        "# assignments",
        "left_value <- 1",
        "2 -> right_value",
        "equal_value = 3",
        "left_fun <- function(argument) argument",
        "lambda_fun <- \\(lambda_argument) lambda_argument",
        "for (loop_value in 1:3) print(loop_value)",
        "object$member",
        "target(named = 1)"
    ))$completion_data

    expect_setequal(parse_data$symbols$name,
        c("left_value", "right_value", "equal_value", "loop_value"))
    expect_setequal(parse_data$functions$name, c("left_fun", "lambda_fun"))
    expect_setequal(parse_data$formals$name,
        c("argument", "lambda_argument"))
    expect_setequal(parse_data$empty_tokens, c("member", "named"))
})

completion_test_namespace <- function(name, functions = character(),
    values = character(), lazydata = character()) {
    namespace <- new.env(parent = baseenv())
    namespace$package_name <- name
    namespace$get_symbols <- function(want_functs, exported_only = TRUE) {
        if (want_functs) functions else values
    }
    namespace$get_lazydata <- function() lazydata
    namespace$exists_funct <- function(object) object %in% functions
    namespace
}

test_that("Namespace completions distinguish workspace and package functions", {
    package <- completion_test_namespace(
        "example", functions = c("alpha", "beta")
    )
    workspace <- completion_test_namespace(
        WORKSPACE, functions = "alpha_workspace"
    )

    package_items <- ns_function_completion(package, "al", TRUE, TRUE)
    expect_length(package_items, 1L)
    expect_equal(package_items[[1L]]$detail, "{example}")
    expect_equal(package_items[[1L]]$insertText, "alpha($0)")
    expect_equal(package_items[[1L]]$insertTextFormat, InsertTextFormat$Snippet)

    workspace_items <- ns_function_completion(
        workspace, "workspace", TRUE, FALSE
    )
    expect_length(workspace_items, 1L)
    expect_equal(workspace_items[[1L]]$detail, "[workspace]")
    expect_null(workspace_items[[1L]]$insertText)
})

test_that("Imported completions skip missing and non-function namespaces", {
    imports <- collections::dict()
    imports$set("alpha", "example")
    imports$set("value", "example")
    imports$set("missing", "missing-package")
    namespace <- completion_test_namespace(
        "example", functions = "alpha", values = "value"
    )
    workspace <- new.env(parent = baseenv())
    workspace$imported_objects <- imports
    workspace$get_namespace <- function(name) {
        if (identical(name, "example")) namespace else NULL
    }

    items <- imported_object_completion(workspace, "a", TRUE)
    expect_length(items, 1L)
    expect_equal(items[[1L]]$label, "alpha")
    expect_equal(items[[1L]]$insertText, "alpha($0)")

    plain <- imported_object_completion(workspace, "alpha", FALSE)
    expect_null(plain[[1L]]$insertText)
    expect_null(imported_object_completion(workspace, "unmatched", TRUE))
})

test_that("Workspace completion combines namespaces, imports, and limits", {
    imports <- collections::dict()
    imports$set("imported_fun", "example")
    global <- completion_test_namespace(
        WORKSPACE,
        functions = c("global_fun", "global_other"),
        values = "global_value"
    )
    package <- completion_test_namespace(
        "example",
        functions = c("exported_fun", "imported_fun"),
        values = "exported_value",
        lazydata = "example_data"
    )
    workspace <- new.env(parent = baseenv())
    workspace$loaded_packages <- "example"
    workspace$imported_objects <- imports
    workspace$get_namespace <- function(name) {
        if (identical(name, WORKSPACE)) global else if (identical(name, "example")) package
    }

    items <- workspace_completion(
        workspace, "", snippet_support = TRUE, limit = 4L
    )
    expect_length(items, 4L)
    expect_true(isTRUE(attr(items, "truncated")))
    expect_true(all(vapply(items, function(item) {
        !is.null(item$label) && !is.null(item$data$type)
    }, logical(1L))))

    private_items <- workspace_completion(
        workspace, "exported", package = "example",
        exported_only = FALSE, snippet_support = FALSE
    )
    expect_setequal(
        vapply(private_items, `[[`, character(1L), "label"),
        c("exported_fun", "exported_value")
    )
    expect_identical(
        workspace_completion(
            workspace, "nothing-matches", package = "example"
        ),
        list()
    )
})

test_that("Argument value completion accepts only literal character defaults", {
    defaults <- quote(c("first", I("second"), 3, identity("ignored")))
    expect_identical(extract_default_values(defaults), c("first", "second"))
    expect_identical(extract_default_values("single"), "single")
    expect_null(extract_default_values(quote(c(1, 2))))
    missing_default <- alist(value = )[[1L]]
    expect_null(extract_default_values(missing_default))

    workspace <- new.env(parent = baseenv())
    workspace$get_formals <- function(...) alist(
        mode = c("auto", "manual"),
        count = 1L
    )
    expect_identical(
        argument_value_completion(
            workspace, "fun", NULL, "missing", "", formals_list = list()
        ),
        list()
    )
    items <- argument_value_completion(
        workspace, "fun", NULL, "mode", "man"
    )
    expect_length(items, 1L)
    expect_equal(items[[1L]]$label, "manual")
    expect_equal(items[[1L]]$insertText, '"manual"')
})

test_that("Indexed and XML scope completions agree on local symbols", {
    content <- c(
        "outer <- function(argument) {",
        "  local_value <- 1",
        "  local_fun <- function() local_value",
        "  local_value",
        "}"
    )
    fixture <- provider_fixture(content)
    point <- list(row = 3L, col = 8L)

    indexed <- scope_completion(
        fixture$uri, fixture$workspace, "local_", point,
        snippet_support = TRUE
    )
    expect_length(indexed, 2L)

    limited <- scope_completion(
        fixture$uri, fixture$workspace, "local_", point,
        snippet_support = TRUE, limit = 1L
    )
    expect_length(limited, 1L)
    expect_true(isTRUE(attr(limited, "truncated")))

    parse_data <- fixture$document$parse_data
    parse_data$completion_data <- NULL
    legacy_workspace <- new.env(parent = baseenv())
    legacy_workspace$get_parse_data <- function(...) parse_data
    legacy <- scope_completion(
        fixture$uri, legacy_workspace, "local_", point,
        snippet_support = FALSE
    )
    expect_setequal(
        vapply(legacy, `[[`, character(1L), "label"),
        vapply(indexed, `[[`, character(1L), "label")
    )

    parse_data$xml_doc <- NULL
    expect_identical(
        scope_completion(fixture$uri, legacy_workspace, "x", point),
        list()
    )
})

test_that("Token completion supports indexed and XML parse data", {
    content <- c("object$member", "target(named = 1)", "member_other <- 2")
    fixture <- provider_fixture(content)

    indexed <- token_completion(
        fixture$uri, fixture$workspace, "mem", exclude = "member_other",
        limit = 1L
    )
    expect_length(indexed, 1L)
    expect_equal(indexed[[1L]]$label, "member")

    parse_data <- fixture$document$parse_data
    parse_data$completion_data <- NULL
    legacy_workspace <- list(get_parse_data = function(...) parse_data)
    legacy <- token_completion(fixture$uri, legacy_workspace, "mem")
    expect_true("member" %in% vapply(legacy, `[[`, character(1L), "label"))

    parse_data$xml_doc <- NULL
    expect_identical(
        token_completion(fixture$uri, legacy_workspace, "mem"),
        list()
    )
})
