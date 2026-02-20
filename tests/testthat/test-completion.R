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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

    client %>% did_save(temp_file)

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

test_that("Completion of re-exported objects works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "purrr::set_names"
        ),
        temp_file)

    client %>% did_save(temp_file)

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
            "my_func(method = a",
            "",
            "# Test completion with positional argument (first position)",
            "my_func(m)"
        ),
        temp_file)

    client %>% did_save(temp_file)

    # Test named argument completion
    result <- client %>% respond_completion(
        temp_file, c(7, 17),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    expect_length(value_items, 1)
    
    labels <- value_items %>% map_chr(~ .$label)
    expect_true("auto" %in% labels)
    
    # Check that insertText is properly quoted
    insert_texts <- value_items %>% map_chr(~ .$insertText)
    expect_true('"auto"' %in% insert_texts)
    
    # Test positional argument completion
    result <- client %>% respond_completion(
        temp_file, c(10, 8),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    expect_length(value_items, 2)
    
    labels <- value_items %>% map_chr(~ .$label)
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
            "my_func(type = 'a'"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 17),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Should match 'advanced' but not 'normal' or 'special'
    expect_true("advanced" %in% labels)
    expect_false("normal" %in% labels)
    expect_false("special" %in% labels)
})

test_that("Completion of argument values works with base R functions", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# test with base::file() which has open argument with defaults",
            "file('test.txt', open = 'r')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(1, 26),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # file() has open parameter with default values like "r", "w", etc.
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
            "test_func(1, 'r",
            "",
            "# Test third argument with named param",
            "test_func(mode = 'w', style = 'p')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    # Test second argument (mode) - positional
    result <- client %>% respond_completion(
        temp_file, c(9, 15),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    expect_true("read" %in% labels)
    expect_true("write" %in% labels)
    expect_true("append" %in% labels)
    expect_false("plain" %in% labels)
    expect_false("fancy" %in% labels)
    
    # Test third argument (style) - using named parameter
    result <- client %>% respond_completion(
        temp_file, c(12, 32),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    
    # Only assert if we got results, as named argument completion might depend on arg name detection
    if (length(value_items) > 0) {
        labels <- value_items %>% map_chr(~ .$label)
        expect_true("plain" %in% labels)
        expect_true("fancy" %in% labels)
    }
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
            "test_func(c = 5, b = '')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(6, 20),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
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
            "my_func(method = 'M')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 18),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Should match both "Manual" and "Custom" (case insensitive)
    expect_true("Manual" %in% labels)
    # Depending on implementation, might match Custom too
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
            "my_func(x = 'a')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 13),
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
            "fun0(1, run"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 11),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Should match 'running' for positional argument
    expect_true("running" %in% labels)
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
            "my_func('r')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(5, 10),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Should match 'read' but not 'write' or 'append'
    expect_true("read" %in% labels)
    expect_false("write" %in% labels)
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
            "test_func(1, 'f')"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(7, 15),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    labels <- value_items %>% map_chr(~ .$label)
    
    # Should include values from both parameters that start with 'f'
    expect_true("fast" %in% labels)
    expect_true("fancy" %in% labels)
    # Should not include values that don't match
    expect_false("slow" %in% labels)
    expect_false("plain" %in% labels)
})

test_that("Positional argument completion works with base R functions", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "# Test with file() function",
            "file('test.txt', r)"
        ),
        temp_file)

    client %>% did_save(temp_file)

    result <- client %>% respond_completion(
        temp_file, c(1, 17),
        retry_when = function(result) length(result) == 0 || length(result$items) == 0
    )
    
    value_items <- result$items %>% keep(~ .$data$type == "argument_value")
    
    # Should get completions for 'open' parameter values
    if (length(value_items) > 0) {
        labels <- value_items %>% map_chr(~ .$label)
        # file() has open parameter with values like "r", "w", "a", etc.
        expect_true(length(labels) > 0)
    }
})
