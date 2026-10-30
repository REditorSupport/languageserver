test_that("Default value extraction handles qualified and incomplete calls safely", {
    expect_identical(extract_default_values(quote(c(base::I("first"), "second"))),
        c("first", "second"))
    expect_identical(extract_default_values(quote(base::c("first", base::I("second")))),
        c("first", "second"))
    expect_identical(extract_default_values(quote(c(, I(), I(, "ignored"), "last"))), "last")
    expect_identical(extract_default_values(quote(c(other::I("ignored"), "last"))), "last")
    expect_identical(extract_default_values(quote(c((function() "ignored")(), "last"))), "last")
    expect_null(extract_default_values(quote(other::c("ignored"))))

    withr::local_options(languageserver.default_value_evaluated = FALSE)
    expr <- quote(c(options(languageserver.default_value_evaluated = TRUE), "literal"))
    expect_identical(extract_default_values(expr), "literal")
    expect_false(getOption("languageserver.default_value_evaluated"))
})

test_that("Default value completion inserts strings that preserve literal values", {
    expressions <- c('"a\\\"b"', '"c\\\\d"', '"line\\nend"', '"tab\\tend"',
        '""', '"漢字😀"')
    if (getRversion() >= "4.0.0") expressions <- c(expressions, 'r"(raw\\path"value)"')
    defaults <- list(mode = str2lang(paste0("c(", paste(expressions, collapse = ", "), ")")))
    workspace <- list(get_formals = function(...) defaults)
    items <- argument_value_completion(workspace, "f", NULL, "mode", "")
    expect_length(items, length(expressions))
    expected <- vapply(expressions, str2lang, "", USE.NAMES = FALSE)
    expect_identical(vapply(items, function(item) str2lang(item$insertText), ""), expected)
    expect_identical(vapply(items, `[[`, "", "label"), expected)
    expect_true(all(vapply(items, `[[`, numeric(1L), "insertTextFormat") == InsertTextFormat$PlainText))
})

test_that("Argument value completion extracts each default once and keeps parameter order", {
    defaults <- alist(first = c("one", "two"), missing = , last = base::c("three", base::I("four")))
    calls <- 0L
    workspace <- list(guess_namespace = function(...) "example", get_formals = function(...) {
        calls <<- calls + 1L
        defaults
    })
    items <- arg_value_completion(NULL, workspace, NULL, NULL, "", "f")
    expect_equal(calls, 1L)
    expect_identical(vapply(items, `[[`, "", "label"), c("one", "two", "three", "four"))
    expect_identical(vapply(items, function(item) item$data$argument, ""),
        c("first", "first", "last", "last"))

    extractor <- mockery::mock(c("one", "two"), NULL, c("three", "four"))
    complete <- arg_value_completion
    mockery::stub(complete, "extract_default_values", extractor)
    expect_identical(complete(NULL, workspace, NULL, NULL, "", "f"), items)
    expect_equal(length(mockery::mock_args(extractor)), length(defaults))

    workspace$get_formals <- function(...) alist(value = 1L)
    expect_identical(arg_value_completion(NULL, workspace, NULL, NULL, "", "f"), list())
})
