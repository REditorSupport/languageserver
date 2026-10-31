test_that("Namespaces without registry closures skip identity buckets", {
    ns <- new.env(parent = baseenv())
    ns$first <- function(value = 1L) list(done = value)
    ns$second <- function(flag = TRUE) if (flag) 1L else 0L
    reference <- member_namespace_input(ns, "functionfixture", exports = c("first", "second"))
    builds <- 0L
    build <- member_function_identity_index
    testthat::local_mocked_bindings(member_function_identity_index = function(functions) {
        builds <<- builds + 1L
        build(functions)
    }, .package = "languageserver")
    input <- member_namespace_input(ns, "functionfixture", exports = c("first", "second"))
    expect_identical(builds, 0L)
    expect_identical(input, reference)
})

test_that("Registry identity matching retains equivalent bindings and distinct closure scopes", {
    scope <- new.env(parent = baseenv())
    syntax <- quote(function(value = 1L) list(done = value))
    first <- eval(syntax, scope)
    copied <- compiler::cmpfun(eval(syntax, scope))
    other <- eval(syntax, new.env(parent = baseenv()))
    expect_identical(first, copied)
    expect_false(identical(first, other))
    ns <- new.env(parent = baseenv())
    ns$a_first <- first
    ns$z_copied <- copied
    ns$other <- other
    ns$registry <- list(first = first, copied = copied, other = other,
        anonymous = function(value = 1L) list(separate = value))
    input <- member_namespace_input(ns, "preparationfixture", exports = "registry")
    expect_identical(input$registries$registry, c(first = "a_first", copied = "a_first",
            other = "other", anonymous = "registry$anonymous"))
    index <- member_package_index(input)
    expect_identical(names(member_infer(quote(registry$copied()), index, index$roots)$fields), "done")
    expect_identical(names(member_infer(quote(registry$anonymous()), index, index$roots)$fields), "separate")
    testthat::local_mocked_bindings(member_function_identity_index = function(functions) {
        function(fn) {
            matches <- which(vapply(functions, identical, logical(1L), fn))
            if (length(matches)) names(functions)[[matches[[1L]]]] else NULL
        }
    }, .package = "languageserver")
    reference <- member_namespace_input(ns, "preparationfixture", exports = "registry")
    expect_identical(input, reference)
    expect_identical(member_index_freeze(index), member_index_freeze(member_package_index(reference)))
})

test_that("Registry identity buckets still verify closures when syntax keys collide", {
    first <- function(value) list(first = value)
    second <- function(value) list(second = value)
    testthat::local_mocked_bindings(member_function_syntax = function(fn) quote(function() NULL),
        .package = "languageserver")
    match <- member_function_identity_index(list(first = first, second = second))
    expect_identical(match(first), "first")
    expect_identical(match(second), "second")
    expect_null(match(function(value) list(unknown = value)))
})
