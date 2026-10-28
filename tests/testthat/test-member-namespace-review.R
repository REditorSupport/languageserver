namespace_review_items <- function(snapshot, code) {
    uri <- "file:///namespace-review.R"
    document <- Document$new(uri, language = "r", version = 1L, content = code)
    data <- parse_document(uri, code)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    metadata$set(snapshot$package, member_index_thaw(snapshot))
    member_completion(uri, list(member_metadata = metadata), document,
        list(row = length(code) - 1L, col = nchar(tail(code, 1L))), TRUE, 200L)
}

namespace_review_labels <- function(snapshot, code) {
    vapply(namespace_review_items(snapshot, code), `[[`, character(1L), "label")
}

test_that("Static namespace methods retain captured values and namespace helpers", {
    ns <- new.env(parent = baseenv())
    ns$helper <- eval(quote(function(value) list(done = value)), ns)
    ns$api <- eval(quote(local({
        flag <- TRUE
        list(run = function(option = FALSE) {
            if (flag) helper(option) else list(other = TRUE)
        }, select = function() if (flag) list(selected = TRUE) else list(other = TRUE))
    })), ns)
    ns$environment <- new.env(parent = emptyenv())
    ns$environment$run <- ns$api$run
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "closurefixture", exports = c("api", "environment")
    )))
    expect_identical(namespace_review_labels(snapshot, "closurefixture::api$select()$"), "selected")
    expect_identical(namespace_review_labels(snapshot, "closurefixture::api$run()$"), "done")
    expect_identical(namespace_review_labels(snapshot, "closurefixture::environment$run()$"), "done")
    expect_identical(namespace_review_labels(snapshot, c(
        "helper <- function(...) list(wrong = TRUE)", "closurefixture::api$run()$"
    )), "done")
    item <- namespace_review_items(snapshot, "closurefixture::api$ru")[[1L]]
    expect_identical(item$detail, "run(option = FALSE)")
    expect_identical(item$data$function_id, "api$run")
})

test_that("Static list matrices and arrays never dispatch coercion hooks", {
    marker <- withr::local_tempfile()
    methods <- get(".__S3MethodsTable__.", asNamespace("base"))
    keys <- c("as.list.matrix", "as.list.array")
    previous <- lapply(keys, function(key) get0(key, methods, inherits = FALSE))
    withr::defer({
        for (i in seq_along(keys)) {
            if (is.null(previous[[i]])) rm(list = keys[[i]], envir = methods) else
                assign(keys[[i]], previous[[i]], methods)
        }
    })
    hook <- function(x, ...) {
        writeLines("coercion", marker)
        stop("coercion must not run")
    }
    for (key in keys) assign(key, hook, methods)
    ns <- new.env(parent = baseenv())
    ns$matrix <- matrix(list(list(done = TRUE), 2L), nrow = 1L)
    names(ns$matrix) <- c("alpha", "beta")
    ns$array <- array(list(list(done = TRUE), 2L), dim = c(1L, 1L, 2L))
    names(ns$array) <- c("alpha", "beta")
    before <- list(ns$matrix, ns$array)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "matrixfixture", exports = c("matrix", "array")
    )))
    for (name in c("matrix", "array")) {
        expect_identical(namespace_review_labels(snapshot, paste0("matrixfixture::", name, "$")), c("alpha", "beta"))
        expect_identical(namespace_review_labels(snapshot, paste0("matrixfixture::", name, "$alpha$")), "done")
    }
    expect_identical(list(ns$matrix, ns$array), before)
    expect_false(file.exists(marker))
})
