member_call_test_items <- function(lines, metadata = NULL) {
    fixture <- provider_fixture(lines)
    workspace <- fixture$workspace
    workspace$loaded_packages <- character()
    workspace$imported_objects <- collections::dict()
    workspace$get_namespace <- function(...) NULL
    workspace$guess_namespace <- function(...) NULL
    workspace$member_metadata <- metadata
    point <- list(row = length(lines) - 1L, col = nchar(tail(lines, 1L)))
    items <- completion_reply(1L, fixture$uri, workspace, fixture$document, point, list())$result$items
    Filter(function(item) !is.null(item$data$parameter) || identical(item$data$type, "parameter"), items)
}

test_that("Member calls complete named arguments from their signature", {
    code <- "obj <- list(fun = function(x, ..., y = 1, `two words` = NULL) NULL)"
    for (call in c("obj$fun(", "obj$fun(x")) {
        items <- member_call_test_items(c(code, call))
        labels <- vapply(items, `[[`, character(1L), "label")
        expected <- if (endsWith(call, "(")) c("x", "y", "two words") else "x"
        expect_identical(labels, expected)
        expect_identical(items[[1L]]$insertText, "x = ")
        expect_identical(items[[1L]]$detail, "fun(x, ..., y = 1, `two words` = NULL)")
        if (length(items) == 3L) expect_identical(items[[3L]]$insertText, "`two words` = ")
    }
})

test_that("Package member argument completion uses the exposed function formals", {
    index <- member_generic_index("make_fun <- function(internal) function(exposed, flag = FALSE) NULL")
    index$package <- "callfixture"
    index$exports <- "obj"
    index$roots$obj <- member_value(type = "fixture_object", member_package = "callfixture")
    index$members <- list(fixture_object = c(fun = "make_fun"))
    index$native_factories <- "make_fun"
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("callfixture", member_index_freeze(index))
    items <- member_call_test_items(c("library(callfixture)", "obj$fun("), metadata)
    expect_identical(vapply(items, `[[`, character(1L), "label"), c("exposed", "flag"))
    expect_identical(items[[1L]]$detail, "fun(exposed, flag = FALSE)")
})

test_that("Unknown and non-callable members never borrow bare function arguments", {
    for (declaration in c("obj <- opaque()", "obj <- list(fun = 1)", "obj <- list(fun = function() NULL)")) {
        items <- member_call_test_items(c("fun <- function(unrelated) NULL", declaration, "obj$fun("))
        expect_length(items, 0L)
    }
})
