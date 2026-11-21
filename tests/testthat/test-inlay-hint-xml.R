test_that("XML-only snapshots match indexed hints for positional and piped arguments", {
    lookups <- character()
    fixture <- provider_fixture(c(
        "target(outside, range)",
        "target(", "    10, # first argument", "    second = named, 30)",
        "pkg::target(40, 50)",
        "x %>% target(60, 70)",
        "x %>% target(., 80)",
        "target(first, 90, , extra)",
        "target(100)", "unknown(1, 2)", "target(outside, range)"
    ), function(name, package, uri) {
        expect_identical(uri, fixture$uri)
        lookups <<- c(lookups, paste(package %s3_or% "", name, sep = "::"))
        if (name == "unknown") stop("formals unavailable")
        if (!is.null(package)) return(alist(left = , right = ))
        alist(first = , second = , third = , ... = )
    })
    minimum <- lsp_settings$get("inlay_hints_minimum_arguments")
    minimum_length <- lsp_settings$get("inlay_hints_minimum_argument_length")
    withr::defer({
        lsp_settings$set("inlay_hints_minimum_arguments", minimum)
        lsp_settings$set("inlay_hints_minimum_argument_length", minimum_length)
    })
    lsp_settings$set("inlay_hints_minimum_arguments", NA_real_)
    lsp_settings$set("inlay_hints_minimum_argument_length", -1L)
    request <- range(position(2L, 4L), position(10L, 0L))
    hints <- function() {
        inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
    }
    indexed <- hints()
    lookups <- character()
    fixture$document$parse_data$range_data <- NULL
    fixture$document$parse_data$syntax_data <- NULL
    legacy <- hints()

    expect_identical(vapply(legacy, `[[`, "", "label"),
        c("first =", "third =", "left =", "right =", "second =", "third =",
            "first =", "second =", "second ="))
    expect_identical(legacy, indexed)
    expect_setequal(lookups, c("::target", "pkg::target", "::unknown"))
    expect_length(lookups, 3L)
    request <- range(position(10L, 0L), position(10L, 1L))
    expect_length(hints(), 0L)
})

test_that("indexed and XML-only inlay hints stop at the same argument in a large viewport", {
    fixture <- provider_fixture(rep("target(1, 2, 3)", 68L),
        function(...) alist(first = , second = , third = ))
    request <- range(position(0L, 0L), position(68L, 0L))
    indexed <- inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
    fixture$document$parse_data$range_data <- NULL
    fixture$document$parse_data$syntax_data <- NULL
    result <- inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
    expect_identical(result, indexed)
    expect_length(result, 200L)
    expect_identical(result[[200L]]$position, position(66L, 10L))
    expect_identical(result[[200L]]$label, "second =")
})

test_that("XML-only snapshots recognize explicit native pipe placeholders", {
    skip_if(getRversion() < "4.2.0", "Native pipe placeholders require R 4.2")
    fixture <- provider_fixture(c("x |> target(1, 2)", "x |> target(1, third=_, 2)"),
        function(...) alist(first = , second = , third = ))
    fixture$document$parse_data$range_data <- NULL
    fixture$document$parse_data$syntax_data <- NULL
    request <- range(position(0L, 0L), position(2L, 0L))
    result <- inlay_hint_reply(1L, fixture$uri, fixture$workspace, fixture$document, request)$result
    expect_identical(vapply(result, `[[`, "", "label"), c("second =", "third =", "first =", "second ="))
})
