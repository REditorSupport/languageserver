test_that("open and shallow S4 indexes retain each source declaration in UTF-16", {
    root <- withr::local_tempdir()
    path <- file.path(root, "declarations.R")
    content <- c(
        '#line 70 "virtual.R"',
        "{", '\t{ "\U0001f600";\tmethods::setGeneric(def = function(x) x, name = "analyze")',
        '\t  setMethod(definition = function(x) x, f = "analyze", signature = "First")',
        "  }",
        "}",
        'methods::setMethod("an\\u0061lyze", "Second",',
        "    function(x) x)",
        'setGroupGeneric(name = "groupOp")',
        'setGeneric("value<-", function(x, value) standardGeneric("value<-"))',
        'methods::setReplaceMethod(definition = fun, signature = "First", f = "value")',
        'setMethod("value<-", "Second", fun)'
    )
    doc <- parse_document(path_to_uri(path), content)
    shallow <- index_shallow_summary(path, content, root)
    opened <- index_shallow_summary(path, content, root, parse_data = doc)
    expect_identical(shallow$s4_declarations, doc$s4_declarations)
    expect_identical(opened$s4_declarations, doc$s4_declarations)
    groups <- doc$s4_declarations
    expect_setequal(names(groups), c("analyze", "groupOp", "value<-"))
    expect_identical(vapply(groups$analyze, `[[`, character(1L), "kind"), c("generic", "method", "method"))
    expect_identical(groups$groupOp[[1L]]$kind, "generic")
    expect_identical(vapply(groups[["value<-"]], `[[`, character(1L), "kind"), c("generic", "method", "method"))
    expect_equal(groups$analyze[[1L]]$range, range(position(2L, 9L), position(2L, nchar(content[[3L]]) + 1L)))
    expect_equal(groups$analyze[[2L]]$range, range(position(3L, 3L), position(3L, nchar(content[[4L]]))))
    expect_equal(groups$analyze[[3L]]$range, range(position(6L, 0L), position(7L, nchar(content[[8L]]))))
})

test_that("S4 registration indexing inspects only literal top-level declarations", {
    content <- c(
        'quote(setGeneric("quoted"))',
        'later <- function() setMethod("inside", "A", fun)',
        'if (FALSE) setGeneric("conditional")',
        'other::setMethod("unrelated", "A", fun)',
        'setGeneric(paste0("not", "literal"))',
        'setGeneric("")',
        "setGeneric(name = , def = generic)",
        "setMethod(f = , definition = fun)",
        'setReplaceMethod("missingSetter", "A", )',
        'setMethod("missingDefinition", "A")',
        'setMethod("nullDefinition", "A", NULL)',
        'setMethod("notCallable", "A", 42)',
        'setGeneric("external", where = unknown)',
        'setMethod("externalPositional", "A", fun, somewhere)',
        'setGeneric(name = "duplicate", name = "other")',
        'setGeneric("predict", package = "stats")',
        'setGeneric("before")',
        "{ setGeneric <- function(...) NULL }",
        'setGeneric("shadowed")',
        'methods::setGeneric("after")'
    )
    doc <- parse_document("untitled:registrations", content)
    expect_setequal(names(doc$s4_declarations), c("predict", "before", "after"))
})

test_that("recovering an incomplete document keeps verified S4 declarations", {
    root <- withr::local_tempdir()
    path <- file.path(root, "partial.R")
    content <- c('setMethod("known", "First", function(x) x)',
        'setMethod("damaged", "Second", function(x) {', "  x$", "})",
        'setGeneric("after")')
    parsed <- parse_document(path_to_uri(path), content)
    expect_true(parsed$parse_error)
    expect_setequal(names(parsed$s4_declarations), c("known", "after"))
    expect_identical(index_shallow_summary(path, content, root)$s4_declarations, parsed$s4_declarations)
    literate <- c('Prose mentioning setGeneric("example")', "```{r}", content, "```")
    chunks <- parse_document("untitled:partial.Rmd", literate, is_rmarkdown = TRUE)
    expect_setequal(names(chunks$s4_declarations), c("known", "after"))
    expect_equal(chunks$s4_declarations$known[[1L]]$range$start, position(2L, 0L))
})
