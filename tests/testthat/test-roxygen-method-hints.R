method_hint_probe <- function(lines, s7 = FALSE) {
    f <- provider_fixture(lines)
    if (s7) {
        skip_if_not_installed("S7")
        metadata <- MemberMetadataCache$new(16 * 1024^2)
        metadata$set("S7", member_prepare_package("S7"))
        f$workspace$member_metadata <- metadata
    }
    function(line) {
        row <- match(line, lines)
        col <- regexpr("$field", line, fixed = TRUE)[[1L]]
        stopifnot(!is.na(row), col > 0L)
        values <- member_completion(f$uri, f$workspace, f$document, list(row = row - 1L, col = col), TRUE, 200L)
        vapply(values, `[[`, "", "label")
    }
}

test_that("S4 roxygen belongs to the matched inline generic or method definition", {
    lines <- c('Box <- R6::R6Class("Box", public = list(value = 1L))',
        "#' @param x (`Box`) Input.", "#' @returns (`Box`) Output.",
        'methods::setGeneric("render", def = function(x) { x$field; standardGeneric("render") })',
        "#' @param x (`Box`) Input.", 'setGroupGeneric("OpsX", function(x, y) x$field)',
        "#' @param extra (`Box`) Options.", 'setMethod("render", "ANY", function(x, extra) extra$field)',
        "#' @param value (`Box`) Replacement.",
        'methods::setReplaceMethod(signature = "ANY", f = "render", definition = function(x, value) value$field)',
        "#' @param from (`Box`) Source.", "#' @param to (`Unrelated`) Coercion target.",
        'methods::setAs("From", "To",", "  replace = function(from, value) from$field,", "  def = (function(from) from$field))',
        "setMethod <- function(f, signature, definition) NULL", "#' @param x (`Box`) Unrelated callback.",
        'setMethod("render", "ANY", function(x) x$field)', "#' @param x (`Box`) Wrong namespace.",
        'unrelated::setGeneric("render", function(x) x$field)')
    labels <- method_hint_probe(lines)
    for (n in c(4L, 6L, 8L, 10L, 15L)) expect_setequal(labels(lines[[n]]), c("clone", "value"))
    for (n in c(14L, 18L, 20L)) expect_length(labels(lines[[n]]), 0L)
    data <- parse_document("file:///method-declarations.R", lines)$member_data
    expect_identical(data$type_hints[[1L]]$returns, "Box")
    expect_identical(names(data$type_hints[[length(data$type_hints)]]$params), "from")
})

test_that("S7 method hints and emitted generic results preserve exact dispatch evidence", {
    lines <- c('Box <- R6::R6Class("Box", public = list(value = 1L))", "library(S7)',
        'A <- new_class("A")", "B <- new_class("B")', "#' @returns (`Box`) A result.",
        'read <- new_generic("read", "x")", "read(opaque())$field',
        "method(read, A) <- function(x) list(actual = TRUE)", "a <- read(A())$field",
        "read(B())$field", "#' @returns (`Box`) Method result.", "method(read, A) <- function(x) opaque()",
        "b <- read(A())$field", "method(read, A) <- function(x) if (opaque()) list(different = TRUE) else opaque()",
        "uncertain <- read(A())$field", "read(A(), nonexistent = 1L)$field", "read()$field",
        'other <- new_generic("other", "x")', "#' @param extra (`Box`) Configuration.",
        "#' @returns (`Box`) The result.", "method(other, B) <- function(x, extra = NULL) { extra$field; opaque() }",
        "other(B())$field", "other(A())$field", "#' @param x (`Box`) Default-method input.",
        "method(other, class_any) <- function(x, ...) x$field",
        "#' @param extra (`Box`) Custom generic input.",
        'custom <- S7::new_generic("custom", "x", fun = function(x, extra) { extra$field; S7::S7_dispatch() })')
    labels <- method_hint_probe(lines, s7 = TRUE)
    for (pass in 1:2) {
        for (n in c(7L, 10L, 13L, 21L, 22L, 25L, 27L)) expect_setequal(labels(lines[[n]]), c("clone", "value"))
        expect_identical(labels(lines[[9L]]), "actual")
        for (n in c(15L, 16L, 17L, 23L)) expect_length(labels(lines[[n]]), 0L)
    }
})
