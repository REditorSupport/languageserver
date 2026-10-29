test_that("Package preparation reuses unchanged models and refreshes changed dependencies", {
    revisions <- c(cachefixture = 1L, dependencyfixture = 1L)
    prepared <- character()
    identity <- function(package, ...) list(package = package, revision = revisions[[package]])
    testthat::local_mocked_bindings(
        resolve_attached_packages = function(...) names(revisions),
        package_metadata_context = function(...) list(cacheable = TRUE),
        package_metadata_live_identity = identity,
        member_prepare_package = function(package, ...) {
            prepared <<- c(prepared, package)
            list(identity = identity(package))
        }, .package = "languageserver")
    known <- stats::setNames(lapply(names(revisions), identity), names(revisions))
    result <- member_resolve_packages("cachefixture", .libPaths(), prepare_s3 = FALSE, known_members = known)
    expect_length(result$members, 0L)
    expect_length(prepared, 0L)
    expect_identical(result$packages, names(revisions))
    revisions[["dependencyfixture"]] <- 2L
    result <- member_resolve_packages("cachefixture", .libPaths(), prepare_s3 = FALSE, known_members = known)
    expect_identical(names(result$members), "dependencyfixture")
    expect_identical(prepared, "dependencyfixture")
    expect_identical(result$members$dependencyfixture$identity$revision, 2L)
})

test_that("Member catalogs retain identities for reuse by package workers", {
    index <- member_generic_index("")
    index$package <- "cachefixture"
    index$identity <- list(package = "cachefixture", revision = 1L)
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("cachefixture", member_index_freeze(index))
    expect_identical(metadata$catalog("cachefixture")$identity, index$identity)
})

test_that("Timed-out member receivers retry with more time and cache successful results", {
    metadata <- MemberMetadataCache$new(1024^2)
    workspace <- list(member_metadata = metadata)
    content <- c("x <- list(alpha = 1)", "x$")
    document <- Document$new("file:///adaptive-members.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    point <- list(row = 1L, col = 2L)
    limits <- numeric()
    budget_factory <- member_request_budget
    infer <- member_cursor_value
    testthat::local_mocked_bindings(
        member_request_budget = function() {
            budget <- budget_factory()
            budget$time_limit <- 0.25
            budget
        },
        member_cursor_value = function(parsed, sentinel, index, bindings, budget, ...) {
            limits <<- c(limits, budget$time_limit)
            if (budget$time_limit < 0.5) {
                budget$exhausted <- budget$timed_out <- TRUE
                return(member_value(reason = "budget"))
            }
            infer(parsed, sentinel, index, bindings, budget, ...)
        }, .package = "languageserver")
    resolve <- function() member_resolve_cursor(document$uri, workspace, document, point, member_cursor(document, point))
    expect_identical(names(resolve()$value$fields), "alpha")
    expect_identical(limits, c(0.25, 0.5))
    expect_identical(names(resolve()$value$fields), "alpha")
    expect_length(limits, 2L)
    content[[1L]] <- "x <- list(beta = 1)"
    document$set_content(2L, content)
    document$parse_data <- parse_document(document$uri, content)
    expect_identical(names(resolve()$value$fields), "beta")
    expect_identical(tail(limits, 1L), 0.5)
    index <- member_generic_index("")
    metadata$set("changed", member_index_freeze(index))
    expect_identical(names(resolve()$value$fields), "beta")
    expect_identical(tail(limits, 2L), c(0.25, 0.5))
})

test_that("Adaptive retries share the original node allowance", {
    metadata <- MemberMetadataCache$new(1024^2)
    content <- c("x <- list(alpha = TRUE)", "x$")
    document <- Document$new("file:///adaptive-nodes.R", language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(document$uri, content)
    budget_factory <- member_request_budget
    calls <- 0L
    allowances <- integer()
    testthat::local_mocked_bindings(
        member_request_budget = function() {
            budget <- budget_factory()
            budget$time_limit <- 0.25
            budget$remaining <- 100L
            budget
        },
        member_cursor_value = function(parsed, sentinel, index, bindings, budget, ...) {
            calls <<- calls + 1L
            allowances <<- c(allowances, budget$remaining)
            if (calls == 1L) {
                budget$remaining <- 7L
                budget$exhausted <- budget$timed_out <- TRUE
                member_value(reason = "budget")
            } else {
                member_value(type = "list", fields = list(alpha = member_literal(TRUE)))
            }
        }, .package = "languageserver")
    point <- list(row = 1L, col = 2L)
    result <- member_resolve_cursor(document$uri, list(member_metadata = metadata), document, point, member_cursor(document, point))
    expect_identical(allowances, c(100L, 7L))
    expect_identical(result$value$type, "list")
})
