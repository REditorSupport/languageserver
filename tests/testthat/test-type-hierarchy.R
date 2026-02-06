test_that("Type hierarchy works with R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))"
    ), single_file)

    client %>% did_save(single_file)

    # Test prepare type hierarchy for Animal
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 1), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Animal")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
    expect_equal(result[[1]]$uri, path_to_uri(single_file))
    expect_true(!is.null(result[[1]]$data$classType))
    expect_equal(result[[1]]$data$classType, "R6")

    # Test prepare type hierarchy for Dog
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(4, 1), retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Dog")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
    expect_equal(result[[1]]$data$classType, "R6")
})

test_that("Type hierarchy returns supertypes for R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))"
    ), single_file)

    client %>% did_save(single_file)

    # Prepare Dog
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(4, 1), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)

    # Get supertypes
    result <- client %>% respond_type_hierarchy_supertypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_length(result, 1)
    expect_equal(result[[1]]$name, "Animal")
    expect_equal(result[[1]]$kind, SymbolKind$Class)
})

test_that("Type hierarchy returns subtypes for R6Class", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(R6)",
        "Animal <- R6::R6Class('Animal', public = list(",
        "  initialize = function(name) { self$name <- name }",
        "))",
        "Dog <- R6::R6Class('Dog', inherit = Animal, public = list(",
        "  bark = function() { print('Woof!') }",
        "))",
        "Cat <- R6::R6Class('Cat', inherit = Animal, public = list(",
        "  meow = function() { print('Meow!') }",
        "))"
    ), single_file)

    client %>% did_save(single_file)

    # Prepare Animal
    item <- client %>% respond_prepare_type_hierarchy(
        single_file, c(1, 1), retry_when = function(result) length(result) == 0)

    expect_length(item, 1)

    # Get subtypes
    result <- client %>% respond_type_hierarchy_subtypes(
        item[[1]], retry_when = function(result) length(result) == 0)

    expect_gte(length(result), 2)
    names <- vapply(result, function(x) x$name, character(1))
    expect_setequal(names, c("Dog", "Cat"))
})

test_that("Type hierarchy returns empty for non-class definitions", {
    skip_on_cran()
    client <- language_client()

    single_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) { x + 1 }",
        "bar <- 42"
    ), single_file)

    client %>% did_save(single_file)

    # Try to prepare type hierarchy on a regular function
    result <- client %>% respond_prepare_type_hierarchy(
        single_file, c(0, 1), retry_when = function(result) TRUE)

    expect_null(result)
})
