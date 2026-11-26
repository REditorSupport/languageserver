test_that("batched R6 field tags retain the exact declaration and rename coordinates", {
    fields <- function(prefix, type, n) {
        names <- paste0(prefix, seq_len(n))
        tags <- rep(type, length.out = n)
        as.vector(rbind(paste0("  #' @field ", names, " ", tags),
                paste0("  ", names, " = NULL,")))
    }
    lines <- c("#' @field a1 (`Different`) Conflicting overview.",
        'Box <- R6::R6Class("Box", public = list(',
        fields("a", c("(`A`). Typed.", "Described field."), 10L),
        "  #' @field with_space", "  #'   (`pkg::Field | NULL`) Multiline union.",
        "  with_space = NULL,",
        fields("b", "(`Ω`) A Unicode name.", 8L),
        "  #' @field mismatch (`Ignored`) Spelling must match.",
        "  end = NULL), private = list(",
        fields("private", "(`pkg::B`), An alternative section.", 8L), "  marker = NULL))",
        "unrelated <- list(", fields("ignored", "(`Nope`) Other list.", 8L), "  marker = NULL)")
    syntax <- syntax_parse_data(utils::getParseData(parse(text = lines, keep.source = TRUE)))
    index <- function() member_roxygen_type_index(lines, syntax, row_offset = 2L, col_offset = 3L, uri = "file:///R/box.R")
    batch <- index()
    testthat::local_mocked_bindings(roxygen_field_batches = function(...) NULL)
    expect_identical(batch, index())
    record <- attr(batch, "classes")[[1L]]$hints
    expect_identical(record$field$a1, character())
    expect_identical(record$field$b8, "Ω")
    expect_false("end" %in% names(record$field))
    expect_false("ignored1" %in% names(record$field))
})

test_that("shadowed class factories do not publish type references", {
    lines <- c("#' @slot field (`Real`) Preceding declaration.",
        'setClass("Original", slots = list(field = "ANY"))',
        "setClass <- function(...) NULL", "#' @slot field (`Ignored`) Shadowed global.",
        'setClass("Fake", slots = list(field = "ANY"))',
        "other <- function(new_class) {", "  #' @prop field (`Ignored`) Shadowing formal.",
        '  new_class("Fake", properties = list(field = NULL))', "}",
        "use <- function() {", "  R6Class <- function(...) NULL", "  nested <- function() {",
        "    #' @field field (`Ignored`) Shadow in enclosing function.",
        '    R6Class("Fake", public = list(field = NULL))', "  }", "}",
        "#' @field field (`Real`) Sibling function shadow is out of scope.",
        'R6Class("Visible", public = list(field = NULL))',
        "#' @slot field (`Real`) Explicit qualification.",
        'methods::setClass("Qualified", slots = list(field = "ANY"))')
    data <- parse_document("file:///factory.R", lines)$member_data
    sites <- attr(data$type_hints, "occurrences", exact = TRUE)
    expect_identical(sites$type, rep("Real", 3L))
    expect_identical(length(attr(data$type_hints, "classes")), 3L)
})

test_that("qualified inline descriptions follow S7 bindings and literal S4 declarations", {
    lines <- c("library(S7)", "Bar := new_class(properties = list(",
        "  #' @prop Bar@item (`S7Value`)", "  item = class_any,",
        "  #' @prop Other@foreign (`Ignored`)", "  foreign = class_any))",
        'methods::setClass("Stored", slots = base::list(',
        "  #' @slot data (`StoredValue`)", '  data = "ANY"))',
        'methods::setClass("Legacy", representation = methods::representation(',
        "  #' @slot raw (`LegacyValue`)", '  raw = "ANY"))',
        'methods::setClass("Other", slots = other::list(',
        "  #' @slot data (`Ignored`)", '  data = "ANY"))')
    hints <- parse_document("file:///qualified-class.R", lines)$member_data$type_hints
    classes <- unname(attr(hints, "classes", exact = TRUE))
    expect_identical(classes[[1L]]$hints$prop, list(item = "S7Value"))
    expect_identical(classes[[2L]]$hints$slot, list(data = "StoredValue"))
    expect_identical(classes[[3L]]$hints$slot, list(raw = "LegacyValue"))
    expect_identical(attr(hints, "occurrences")$type, c("S7Value", "StoredValue", "LegacyValue"))
})
