test_that("source documentation accepts only a supported roxygen2 namespace", {
    namespace <- roxygen2_namespace
    stub(namespace, "requireNamespace", FALSE)
    stub(namespace, "getNamespaceVersion", function(...) stop("Checked an absent package"))
    stub(namespace, "asNamespace", function(...) stop("Loaded an absent package"))
    expect_null(namespace())

    supported <- new.env(parent = emptyenv())
    namespace <- roxygen2_namespace
    stub(namespace, "requireNamespace", TRUE)
    stub(namespace, "getNamespaceVersion", package_version("6.9.9"))
    stub(namespace, "asNamespace", supported)
    expect_null(namespace())

    namespace <- roxygen2_namespace
    stub(namespace, "requireNamespace", TRUE)
    stub(namespace, "getNamespaceVersion", package_version("7.0.0"))
    stub(namespace, "asNamespace", supported)
    expect_identical(namespace(), supported)
})

test_that("source comments fall back to text without roxygen2 or after parser errors", {
    comments <- c("#' A source function.", "#' @param value The input.")
    expected <- "A source function.  \n@param value The input."
    testthat::local_mocked_bindings(roxygen2_namespace = function() NULL, .package = "languageserver")
    expect_identical(convert_comment_to_documentation(comments), expected)
    expect_identical(convert_comment_to_documentation("# Ordinary comment."), "Ordinary comment.")

    namespace <- new.env(parent = emptyenv())
    namespace$parse_text <- function(...) stop("Parser failed")
    testthat::local_mocked_bindings(roxygen2_namespace = function() namespace, .package = "languageserver")
    expect_identical(convert_comment_to_documentation(comments), expected)
})

test_that("empty source documentation remains safe without roxygen2", {
    testthat::local_mocked_bindings(
        roxygen2_namespace = function() NULL,
        convert_comment_to_documentation = function(...) stop("Parsed empty comments"),
        .package = "languageserver")
    owner <- new.env(parent = emptyenv())
    expect_null(analysis_documentation(owner, character()))
    expect_null(analysis_documentation(owner, character()))
})

test_that("source documentation caches follow roxygen2 availability changes", {
    state <- new.env(parent = emptyenv())
    state$namespace <- NULL
    state$parses <- 0L
    testthat::local_mocked_bindings(roxygen2_namespace = function() state$namespace, .package = "languageserver")
    owner <- new.env(parent = emptyenv())
    comments <- c("#' A source function.", "#' @param value The input.")
    fallback <- "A source function.  \n@param value The input."
    expect_identical(analysis_documentation(owner, comments), fallback)
    unavailable_cache <- owner$analysis_caches[["source-documentation"]]$values

    namespace <- new.env(parent = emptyenv())
    namespace$parse_text <- function(...) {
        state$parses <- state$parses + 1L
        list(list(tags = list(
            list(tag = "title", val = "A source function.", raw = "A source function."),
            list(tag = "param", val = list(name = "value", description = "The input."), raw = "value The input.")
        )))
    }
    state$namespace <- namespace
    structured <- analysis_documentation(owner, comments)
    expect_identical(structured$title, "A source function.")
    expect_identical(structured$arguments$value, "The input.")
    expect_identical(analysis_documentation(owner, comments), structured)
    expect_identical(state$parses, 1L)
    available_cache <- owner$analysis_caches[["source-documentation"]]$values
    expect_false(identical(available_cache, unavailable_cache))

    state$namespace <- NULL
    expect_identical(analysis_documentation(owner, comments), fallback)
    expect_false(identical(owner$analysis_caches[["source-documentation"]]$values, available_cache))
    expect_identical(state$parses, 1L)
})
