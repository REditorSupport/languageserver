parse_xdoc <- function(code) {
    parsed <- parse(text = code, keep.source = TRUE)
    xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
}

test_that("xdoc_find_token prefers strings at adjacent token boundaries", {
    xdoc <- parse_xdoc('setClass("BaseEntity",x+y)')

    token <- xdoc_find_token(xdoc, line = 1, col = 10)
    expect_equal(xml2::xml_name(token), "STR_CONST")
    expect_equal(xml2::xml_text(token), '"BaseEntity"')

    xdoc <- parse_xdoc('list(x,"Class")')
    token <- xdoc_find_token(xdoc, line = 1, col = 8)
    expect_equal(xml2::xml_name(token), "STR_CONST")
    expect_equal(xml2::xml_text(token), '"Class"')
})

test_that("xdoc_find_token retains document order for other boundaries", {
    xdoc <- parse_xdoc("f(x)")

    token <- xdoc_find_token(xdoc, line = 1, col = 3)
    expect_equal(xml2::xml_name(token), "OP-LEFT-PAREN")
})

test_that("indexed enclosing scope lookup preserves results", {
    xdoc <- parse_xdoc(c(
        "first <- 1",
        "fun <- function(argument) {",
        "  nested <- argument",
        "  nested",
        "}"
    ))
    expected <- xdoc_find_enclosing_scopes(xdoc, line = 4L, col = 5L,
        top = TRUE)

    attr(xdoc, "top_level_index") <- xdoc_top_level_index(xdoc)
    actual <- xdoc_find_enclosing_scopes(xdoc, line = 4L, col = 5L,
        top = TRUE)

    expect_equal(xml2::xml_path(actual), xml2::xml_path(expected))
})

test_that("stack-aware errors retain printable call information", {
    fail <- function() stop("failure from helper")
    captured <- tryCatchStack(fail(), error = identity)

    expect_s3_class(captured, "errorWithStack")
    expect_match(conditionMessage(captured), "failure from helper")
    output <- capture.output(print.errorWithStack(captured))
    expect_true(any(grepl("Error: failure from helper", output, fixed = TRUE)))
    if (length(captured$calls)) {
        expect_true(any(grepl("Stack trace:", output, fixed = TRUE)))
    }
    expect_match(capture_print(list(value = 1L)), "value")
})

test_that("expression types distinguish language object categories", {
    cases <- list(
        list(quote(function(x) x), "function"),
        list(quote(c(1, 2)), "array"),
        list(quote(matrix(1)), "array"),
        list(quote(list(1)), "list"),
        list(quote(R6::R6Class("Class")), "R6"),
        list(quote(methods::setClass("Class")), "S4"),
        list(quote(methods::setRefClass("Class")), "RefClass"),
        list(quote(custom_call()), "variable"),
        list(quote(name), "symbol"),
        list(1L, "integer")
    )

    actual <- vapply(cases, function(case) get_expr_type(case[[1L]]), character(1L))
    expected <- vapply(cases, `[[`, character(1L), 2L)
    expect_identical(actual, expected)
})

test_that("URI helpers handle files, notebooks, Unicode, and empty inputs", {
    expect_identical(uri_escape_unicode(character()), character())
    expect_match(uri_escape_unicode("file:///tmp/a b.R"), "a%20b.R", fixed = TRUE)
    expect_identical(path_from_uri(character()), character())
    expect_identical(path_to_uri(character()), character())
    expect_equal(path_from_uri("untitled:Untitled-1"), "")

    path <- file.path(tempdir(), paste0("space ", intToUtf8(0x4f62), ".R"))
    expect_equal(path_from_uri(path_to_uri(path)), path.expand(path))
    expect_equal(
        path_from_uri("vscode-notebook-cell:/tmp/notebook.ipynb#cell-1"),
        "/tmp/notebook.ipynb"
    )
    expect_equal(
        path_from_uri(
            "vscode-notebook-cell://wsl+ubuntu/tmp/notebook.ipynb#cell-1"
        ),
        "/tmp/notebook.ipynb"
    )
})

test_that("path helpers find package roots and restore working directories", {
    original <- getwd()
    package_root <- normalizePath(file.path(original, "..", ".."))
    expect_false(path_has_parent(package_root, NULL))
    expect_true(path_has_parent(file.path(package_root, "R"), package_root))
    expect_true(is_directory(package_root))
    expect_false(is_directory(file.path(package_root, "does-not-exist")))
    expect_equal(
        find_package(file.path(package_root, "R")),
        package_root
    )
    expect_null(find_package(file.path(package_root, "does-not-exist")))

    empty_dir <- withr::local_tempdir()
    expect_null(find_package(empty_dir))
    expect_equal(getwd(), original)
    expect_equal(
        normalizePath(with_wd(empty_dir, getwd())),
        normalizePath(empty_dir)
    )
    expect_equal(getwd(), original)
    expect_equal(with_wd(NULL, getwd()), original)

    uri <- path_to_uri(file.path(empty_dir, "file.R"))
    expect_equal(get_root_path_for_uri(uri, original), original)
    expect_equal(get_root_path_for_uri(uri, character()), empty_dir)
    expect_equal(get_root_path_for_uri("untitled:1", character()), original)
})

test_that("R Markdown block extraction handles empty and incomplete fences", {
    content <- c(
        "text",
        "```{r}",
        "x <- 1",
        "```",
        "```{R, echo=FALSE}",
        "y <- 2",
        "```",
        "```{r}",
        "unfinished <- TRUE"
    )
    blocks <- extract_blocks(content)
    expect_length(blocks, 2L)
    expect_equal(map_int(blocks, ~ .x$lines), c(3L, 6L))
    expect_equal(map_chr(blocks, ~ .x$text), c("x <- 1", "y <- 2"))
    expect_identical(extract_blocks(c("text", "```{r}", "```")), list())
    expect_identical(extract_blocks("plain text"), list())
})

test_that("small text helpers cover boundaries and throttling", {
    calls <- 0L
    throttled <- throttle(function(value) {
        calls <<- calls + 1L
        value
    }, t = 60)
    expect_equal(throttled("first"), "first")
    expect_null(throttled("second"))
    expect_equal(calls, 1L)

    expect_equal(look_forward("alpha.beta + rest")$token, "alpha.beta")
    expect_equal(look_forward("+")$token, "")
    expect_equal(look_backward("pkg:::fun"), list(
        full_token = "pkg:::fun", package = "pkg",
        accessor = ":::", token = "fun"
    ))
    expect_equal(look_backward("object$member")$full_token, "")
    expect_equal(look_backward("member")$full_token, "member")
    expect_equal(na_to_empty_string(NA_character_), "")
    expect_null(empty_string_to_null(""))
    expect_equal(empty_string_to_null("value"), "value")
    expect_equal(str_trunc("abcdefgh", 6L), "abc...")
    expect_equal(str_trunc("abc", 6L), "abc")
    expect_true(is.na(str_trunc(NA_character_, 6L)))
})

test_that("documentation helpers render roxygen and Rd structures", {
    documentation <- convert_comment_to_documentation(c(
        "#' Add two values",
        "#'",
        "#' A longer description.",
        "#' @param x First value.",
        "#' @param y Second value.",
        "#' @examples add(1, 2)"
    ))
    expect_equal(documentation$title, "Add two values")
    expect_match(documentation$description, "longer description")
    expect_setequal(names(documentation$arguments), c("x", "y"))
    expect_match(documentation$markdown, "```r", fixed = TRUE)

    fallback <- convert_comment_to_documentation("# ordinary comment")
    expect_identical(fallback, "ordinary comment")

    rd <- tools::parse_Rd(
        textConnection("\\code{x} \\R{} \\dots{}"),
        fragment = TRUE
    )
    markdown <- convert_doc_string(rd)
    expect_match(markdown, "`x`", fixed = TRUE)
    expect_match(markdown, "**R**", fixed = TRUE)
    expect_match(markdown, "...", fixed = TRUE)
})

test_that("help and file probes handle real text and binary data", {
    help_file <- utils::help("mean", package = "base")
    expect_match(get_help(help_file, "text"), "Generic function")
    expect_match(get_help(help_file, "html"), "Arithmetic Mean")
    expect_null(get_help(structure(character(), class = "help_files_with_topic")))

    text_path <- withr::local_tempfile()
    writeLines("plain UTF-8 text", text_path, useBytes = TRUE)
    expect_true(is_text_file(text_path))

    binary_path <- withr::local_tempfile()
    connection <- file(binary_path, open = "wb")
    writeBin(as.raw(c(
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0xff
    )), connection)
    close(connection)
    expect_false(is_text_file(binary_path))

    expect_match(format_file_size(0), "0")
    expect_match(format_file_size(1024), "1")
})

test_that("XML lookup returns a missing node outside parsed source", {
    xdoc <- parse_xdoc("value <- 1")
    token <- xdoc_find_token(xdoc, line = 20L, col = 1L)
    expect_s3_class(token, "xml_missing")

    scopes <- xdoc_find_enclosing_scopes(xdoc, line = 20L, col = 1L)
    expect_length(scopes, 0L)
})
