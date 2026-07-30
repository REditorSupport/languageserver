test_that("native string matching is case-insensitive and NA-safe", {
    values <- c("LanguageServer", "server", "client", NA_character_)

    expect_identical(
        match_with(values, "SERVER"),
        c(TRUE, TRUE, FALSE, FALSE)
    )
    expect_identical(match_with(values, ""), c(TRUE, TRUE, TRUE, FALSE))
    expect_identical(match_with("short", "a much longer pattern"), FALSE)
    expect_identical(match_with(values, NA_character_), rep(NA, 4L))
    expect_identical(
        .Call("match_with_c", values, "server", PACKAGE = "languageserver"),
        c(TRUE, TRUE, FALSE, NA)
    )

    expect_identical(
        fuzzy_find(values, "LS"),
        c(TRUE, FALSE, FALSE, FALSE)
    )
    expect_identical(fuzzy_find(values, ""), c(TRUE, TRUE, TRUE, FALSE))
    expect_identical(fuzzy_find(values, NA_character_), rep(NA, 4L))
    expect_identical(
        .Call("fuzzy_find_c", values, "ls", PACKAGE = "languageserver"),
        c(TRUE, FALSE, FALSE, NA)
    )
})

test_that("native string matching validates its low-level inputs", {
    expect_error(
        .Call("match_with_c", 1:3, "x", PACKAGE = "languageserver"),
        "x must be a character vector"
    )
    expect_error(
        .Call("match_with_c", "x", c("x", "y"), PACKAGE = "languageserver"),
        "token must be a single character string"
    )
    expect_error(
        .Call("fuzzy_find_c", 1:3, "x", PACKAGE = "languageserver"),
        "x must be a character vector"
    )
    expect_error(
        .Call("fuzzy_find_c", "x", 1L, PACKAGE = "languageserver"),
        "pattern must be a single character string"
    )
})

test_that("token scanning recognizes namespace and identifier boundaries", {
    expect_equal(
        scan_token("pkg::", 5L),
        list(
            full_token = "pkg::", right_token = "", package = "pkg",
            accessor = "::", token = ""
        )
    )
    expect_equal(
        scan_token("pkg:::hidden", 6L),
        list(
            full_token = "pkg:::hidden", right_token = "hidden",
            package = "pkg", accessor = ":::", token = "hidden"
        )
    )
    expect_equal(scan_token("pkg::fun", 8L, forward = FALSE)$token, "fun")
    expect_equal(scan_token("pkg::fun", 5L)$right_token, "fun")
    unicode_name <- paste0(intToUtf8(0xe9), "clair")
    expect_equal(
        scan_token(unicode_name, 6L, forward = FALSE)$token,
        unicode_name
    )

    expect_equal(scan_token("x$member", 8L, forward = FALSE)$token, "")
    expect_equal(scan_token("1abc", 4L, forward = FALSE)$token, "")
    expect_equal(scan_token("a::", 3L)$package, "")
    expect_equal(scan_token("abc", -2L)$right_token, "abc")
    expect_equal(scan_token("pkg::", 99L)$accessor, "::")
})

test_that("native token scanning validates scalar input types", {
    expect_error(
        .Call("scan_token_c", c("x", "y"), 0L, TRUE,
            PACKAGE = "languageserver"),
        "line must be a single character string"
    )
    expect_error(
        .Call("scan_token_c", "x", 0, TRUE, PACKAGE = "languageserver"),
        "col must be a single integer"
    )
})

test_that("UTF-16 conversion handles every UTF-8 width and boundary", {
    text <- intToUtf8(c(0x61, 0xe9, 0x4f62, 0x10400, 0x7a))

    expect_equal(
        code_point_to_unit(text, c(-1, 0:6, Inf)),
        c(0, 0, 1, 2, 3, 5, 6, 6, 6)
    )
    expect_equal(
        code_point_from_unit(text, c(-1, 0:7, Inf)),
        c(0, 0, 1, 2, 3, NA, 4, 5, 5, 5)
    )
    expect_identical(code_point_to_unit("", integer()), integer())
    expect_identical(code_point_from_unit("", integer()), integer())
})

test_that("native UTF-16 conversion rejects malformed argument types", {
    expect_error(
        .Call("code_point_to_unit_c", c("x", "y"), 0L,
            PACKAGE = "languageserver"),
        "line must be a single character string"
    )
    expect_error(
        .Call("code_point_to_unit_c", "x", 0,
            PACKAGE = "languageserver"),
        "points must be an integer vector"
    )
    expect_error(
        .Call("code_point_from_unit_c", 1L, 0L,
            PACKAGE = "languageserver"),
        "line must be a single character string"
    )
    expect_error(
        .Call("code_point_from_unit_c", "x", 0,
            PACKAGE = "languageserver"),
        "units must be an integer vector"
    )
})

test_that("quote detection handles backticks and raw-string delimiters", {
    enclosed <- function(text, col) {
        .Call("enclosed_by_quotes", text, col, PACKAGE = "languageserver")
    }

    expect_true(enclosed("R'[raw text", 5L))
    expect_true(enclosed("R\"[raw text", 5L))
    expect_true(enclosed("R'{raw text", 5L))
    expect_false(enclosed("R'[raw]' + value", 12L))
    expect_false(enclosed("R\"[raw]\" + value", 12L))
    expect_false(enclosed("R'{raw}' + value", 12L))
})
