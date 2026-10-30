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

test_that("Cold inference extends in place while accounting for CPU contention", {
    withr::local_envvar(c(TESTTHAT = "", R_COVR = ""))
    timing <- new.env(parent = emptyenv())
    timing$calls <- 0L
    clock <- function() {
        timing$calls <- timing$calls + 1L
        c((timing$calls - 1L) * 0.18, 0, (timing$calls - 1L) * 0.35)
    }
    infer <- member_infer
    environment(infer) <- list2env(list(proc.time = clock), parent = environment(infer))
    assign("member_infer", infer, environment(infer))
    budget_factory <- member_request_budget
    environment(budget_factory) <- list2env(list(proc.time = clock), parent = environment(budget_factory))
    expr <- as.call(c(list(as.name("list")), rep(list(1L), 1024L)))
    budget <- budget_factory()
    result <- infer(expr, member_generic_index(""), budget = budget)
    expect_false(budget$exhausted)
    expect_identical(result$type, "list")
    expect_true(result$open)
    expect_gt((timing$calls - 1L) * 0.35, 1)
    expect_gt(budget$time_limit, 0.25)
    expect_identical(budget$remaining, 20000L - 1025L)
})

test_that("Adaptive inference still stops at CPU and elapsed limits", {
    withr::local_envvar(c(TESTTHAT = "", R_COVR = ""))
    for (clock_step in list(c(0.4, 0, 0.4), c(0.01, 0, 1.1))) {
        timing <- new.env(parent = emptyenv())
        timing$calls <- 0L
        clock <- function() {
            timing$calls <- timing$calls + 1L
            clock_step * (timing$calls - 1L)
        }
        infer <- member_infer
        environment(infer) <- list2env(list(proc.time = clock), parent = environment(infer))
        assign("member_infer", infer, environment(infer))
        budget_factory <- member_request_budget
        environment(budget_factory) <- list2env(list(proc.time = clock), parent = environment(budget_factory))
        budget <- budget_factory()
        expr <- as.call(c(list(as.name("list")), rep(list(1L), 1024L)))
        infer(expr, member_generic_index(""), budget = budget)
        expect_true(budget$exhausted)
        expect_true(budget$timed_out)
        expect_gt(budget$remaining, 0L)
        expect_lte(budget$deadline, budget$wall_deadline)
    }
})

test_that("S4 inheritance uses the same adaptive allowance and honors explicit deadlines", {
    withr::local_envvar(c(TESTTHAT = "", R_COVR = ""))
    timing <- new.env(parent = emptyenv())
    timing$calls <- 0L
    clock <- function() {
        timing$calls <- timing$calls + 1L
        c(timing$calls * 0.05, 0, timing$calls * 0.3)
    }
    shape <- member_s4_shape
    environment(shape) <- list2env(list(proc.time = clock), parent = environment(shape))
    assign("member_s4_shape", shape, environment(shape))
    budget_factory <- member_request_budget
    environment(budget_factory) <- list2env(list(proc.time = clock), parent = environment(budget_factory))
    index <- member_generic_index("")
    index$s4_classes$Leaf <- list(complete = TRUE, slots = list(value = "numeric"))
    index$s4_classes$Branch <- list(complete = TRUE, contains = "Leaf", slots = list(label = "character"))
    budget <- budget_factory()
    result <- shape("Branch", index, list(), budget = budget)
    expect_identical(result$slot_types, list(value = "numeric", label = "character"))
    expect_false(budget$exhausted)
    expect_gt(budget$time_limit, 0.25)
    budget$deadline <- -1
    expect_identical(shape("Branch", index, list(), budget = budget)$reason, "budget")
    expect_true(budget$timed_out)
})

test_that("Completed uncertain receivers are reused across providers and invalidated", {
    index <- member_generic_index("step <- function(label = NULL) self")
    index$package <- "chainfixture"
    index$exports <- "seed"
    index$members <- list(box = c(step = "step"))
    index$roots$seed <- member_value(reason = "open_dispatch", possible_returns = list(
        member_value(type = "box", member_package = "chainfixture")))
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("chainfixture", member_index_freeze(index))
    workspace <- list(member_metadata = metadata)
    content <- c("library(chainfixture)", "object <- seed",
        rep("object <- object$step()", 18L), "object$step()")
    document <- Document$new("file:///uncertain-chain.R", language = "r", version = 1L, content = content)
    update <- function(version) {
        document$set_content(version, content)
        data <- parse_document(document$uri, content)
        data$version <- version
        document$update_parse_data(data)
    }
    update(1L)
    calls <- 0L
    infer <- member_cursor_value
    testthat::local_mocked_bindings(member_cursor_value = function(...) {
        calls <<- calls + 1L
        infer(...)
    }, .package = "languageserver")
    row <- length(content) - 1L
    resolve <- function() {
        member_resolve_cursor(document$uri, workspace, document,
            list(row = row, col = 7L), member_cursor(document, list(row = row, col = 7L)))
    }
    result <- resolve()
    expect_false(result$budget$exhausted)
    expect_null(result$value$type)
    expect_length(result$value$possible_returns, 1L)
    expect_identical(document$member_receivers$size(), 1L)
    items <- member_completion(document$uri, workspace, document, list(row = row, col = 7L), TRUE, 200L)
    expect_identical(vapply(items, `[[`, character(1L), "label"), "step")
    expect_match(items[[1L]]$detail, "[possible receiver]", fixed = TRUE)
    signature <- signature_reply(1L, document$uri, workspace, document, list(row = row, col = 12L))$result
    expect_identical(signature$signatures[[1L]]$label, "step(label = NULL)")
    hover <- hover_reply(1L, document$uri, workspace, document, list(row = row, col = 9L))$result
    expect_match(paste(hover$contents, collapse = "\n"), "Receiver type is uncertain", fixed = TRUE)
    expect_identical(calls, 1L)
    content[[2L]] <- "object <- list(other = TRUE)"
    update(2L)
    expect_length(resolve()$value$possible_returns, 0L)
    expect_identical(calls, 2L)
    content[[2L]] <- "object <- seed"
    update(3L)
    expect_length(resolve()$value$possible_returns, 1L)
    index$roots$seed <- member_value(type = "list", fields = list(other = member_literal(TRUE)))
    metadata$set("chainfixture", member_index_freeze(index))
    expect_length(resolve()$value$possible_returns, 0L)
    expect_identical(calls, 4L)
    budget <- member_request_budget()
    budget$transient <- TRUE
    expect_false(member_receiver_cacheable(result$value, result$index, budget))
    budget$transient <- FALSE
    budget$exhausted <- TRUE
    expect_false(member_receiver_cacheable(result$value, result$index, budget))
})
