test_that("type candidate scans follow description starts across physical lines", {
    candidates <- list(
        c("#' @param x [`Box`] A value."),
        c("#' @param x", "#'   \\[`Box`] A value."),
        c("#' @param", "#'   x [\\code{Box}] A value."),
        c("#' @param x An", "#'   object of class [Box]."),
        c("#' @param x An object of", "#'   class `Box`."),
        c("#' @param x The", "#'   [`Box`] object to use."),
        c("#' @param x (`Box", "#'   | Other`) A value.")
    )
    for (lines in candidates) {
        expect_true(roxygen_has_type_span(lines))
        # Some parser-recovery calls supply one multiline string.
        expect_true(roxygen_has_type_span(paste(lines, collapse = "\n")))
        hints <- roxygen_type_block(lines, "x")$params$x
        expect_true("Box" %in% hints)
        code <- c(lines, "f <- function(x) x")
        parsed <- parse(text = code, keep.source = TRUE)
        index <- member_roxygen_type_index(code, utils::getParseData(parsed), uri = "file:///types.R")
        expect_identical(index[[1L]]$params$x, hints)
    }
    ordinary <- c("x <- '[`Box`]'", "code() # 'An object of class [Box]'",
        "#' @param x See [`Box`] and [foo()].", "#' @param x <[`data-masking`][mask]>",
        "#' @return A [guide](https://example.org).", "#' Ordinary prose mentions (`Box`).")
    expect_false(roxygen_has_type_span(ordinary))
})

test_that("batched R6 fields preserve the full grammar's evidence and positions", {
    specs <- c(rep(c("(`pkg::Box`)", "[`Box`]", "\\[`Box`]", "[\\code{Box}]"), 3L),
        "[\\code{any}]", "\\[`atomic`]", "(`listish`)", "[`expressions`]",
        "(`Box`) or [`Other`]", "[`Box`], [`Other`]", "[`Box`] (url)", "[\\code{Box}] and [Other]",
        "[`Box | Other`]", "An object of class [Box].", "See [`Box`].")
    names <- paste0("value", seq_along(specs))
    code <- c('R <- R6::R6Class("R", public = list(',
        as.vector(rbind(paste("  #' @field", names, specs), paste0("  ", names, " = NULL,"))),
        "  last = 1))")
    syntax <- syntax_parse_data(utils::getParseData(parse(text = code, keep.source = TRUE)))
    get <- function() member_roxygen_type_index(code, syntax, uri = "file:///fields.R")
    fast <- get()
    testthat::local_mocked_bindings(roxygen_field_batches = function(...) NULL, .package = "languageserver")
    reference <- get()
    expect_identical(attr(fast, "classes")[[1L]]$hints, attr(reference, "classes")[[1L]]$hints)
    expect_equal(unname(as.matrix(attr(fast, "occurrences"))), unname(as.matrix(attr(reference, "occurrences"))))
})
