namespace_member_fixture <- function(code, snapshot, point = NULL) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///namespace-members.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    data <- parse_document(uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    metadata$set(snapshot$package, member_index_thaw(snapshot))
    if (is.null(point)) point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    list(uri = uri, document = document, point = point,
        workspace = list(member_metadata = metadata, get_documentation = function(...) NULL))
}

namespace_member_items <- function(code, snapshot) {
    fixture <- namespace_member_fixture(code, snapshot)
    member_completion(fixture$uri, fixture$workspace, fixture$document, fixture$point, TRUE, 200L)
}

namespace_member_labels <- function(code, snapshot) {
    vapply(namespace_member_items(code, snapshot), `[[`, character(1L), "label")
}

test_that("Static namespace lists and data frames expose names and nested structures", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$table <- data.frame(value = 1L, `a b` = TRUE, check.names = FALSE)
    ns$bundle <- list(alpha = list(done = TRUE), table = ns$table,
        run = function(key, limit = 2L) {
            writeLines("method", marker)
            list(result = key)
        })
    ns$hidden <- list(secret = TRUE)
    ns$vector <- c(alpha = 1, beta = 2)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "namespacefixture", exports = c("bundle", "table", "vector")
    )))
    for (code in c("namespacefixture::bundle$", "library(namespacefixture)\nbundle$",
            "api <- namespacefixture::bundle\ncopy <- api\ncopy$")) {
        expect_identical(namespace_member_labels(code, snapshot), c("alpha", "run", "table"), info = code)
    }
    expect_identical(namespace_member_labels("namespacefixture::table$", snapshot), c("a b", "value"))
    expect_identical(namespace_member_labels("namespacefixture::bundle$table$", snapshot), c("a b", "value"))
    expect_identical(namespace_member_labels("namespacefixture::bundle$alpha$", snapshot), "done")
    expect_identical(namespace_member_labels("namespacefixture::bundle[[1L]]$", snapshot), "done")
    expect_identical(namespace_member_labels("lapply(namespacefixture::bundle, function(x) x)[[1L]]$", snapshot), "done")
    expect_identical(namespace_member_labels(paste0(
        "choose <- function(flag) if (flag) namespacefixture::bundle$alpha$done else TRUE\n",
        "result <- function() if (choose(unknown)) list(selected = TRUE) else list(other = TRUE)\nresult()$"
    ), snapshot), "selected")
    expect_identical(namespace_member_labels("namespacefixture::bundle$run(stop(\"argument\"))$", snapshot), "result")
    item <- namespace_member_items("namespacefixture::bundle$ru", snapshot)[[1L]]
    expect_identical(item$detail, "run(key, limit = 2L)")
    expect_identical(item$kind, CompletionItemKind$Method)
    expect_identical(namespace_member_items("namespacefixture::table$`a", snapshot)[[1L]]$textEdit$newText, "`a b`")
    expect_length(namespace_member_labels("namespacefixture::hidden$", snapshot), 0L)
    expect_identical(namespace_member_labels("namespacefixture:::hidden$", snapshot), "secret")
    expect_length(namespace_member_labels("library(namespacefixture)\nhidden$", snapshot), 0L)
    expect_length(namespace_member_labels("namespacefixture::vector$", snapshot), 0L)
    expect_length(namespace_member_labels("namespacefixture::bundle@", snapshot), 0L)
    expect_false(file.exists(marker))
})

test_that("Static classed lists retain distinct fields with ordinary dollar access", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$as.list.namespace_record <- function(x, ...) {
        writeLines("coercion", marker)
        stop("coercion")
    }
    ns$first <- structure(list(alpha = list(done = TRUE)), class = "namespace_record")
    ns$second <- structure(list(beta = 1L), class = "namespace_record")
    ns$box <- list(first = ns$first)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "namespacefixture", exports = c("first", "second", "box")
    )))
    expect_identical(namespace_member_labels("namespacefixture::first$", snapshot), "alpha")
    expect_identical(namespace_member_labels("namespacefixture::second$", snapshot), "beta")
    expect_identical(namespace_member_labels("namespacefixture::box$first$alpha$", snapshot), "done")
    expect_false(file.exists(marker))
})

test_that("Static namespace pairlists expose named fields without forcing missing arguments", {
    ns <- new.env(parent = baseenv())
    ns$args <- pairlist(alpha = list(done = TRUE), run = function(value = 1L) list(result = value))
    ns$args <- as.pairlist(c(as.list(ns$args), alist(absent = )))
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "namespacefixture", exports = "args"
    )))
    expect_identical(namespace_member_labels("namespacefixture::args$", snapshot), c("absent", "alpha", "run"))
    expect_identical(namespace_member_labels("namespacefixture::args$alpha$", snapshot), "done")
    expect_identical(namespace_member_labels("namespacefixture::args$run()$", snapshot), "result")
    expect_length(namespace_member_labels("namespacefixture::args$absent$", snapshot), 0L)
})

test_that("Static R6 instances expose public current fields and bound fluent methods", {
    marker <- withr::local_tempfile()
    Parent <- R6::R6Class("StaticParent", public = list(base = function() self))
    Client <- R6::R6Class("StaticClient", inherit = Parent, public = list(
        value = 1L,
        initialize = function() writeLines("initialize", marker),
        step = function(amount = 1L) {
            writeLines("method", marker)
            self
        },
        read = function() private$payload
    ), private = list(payload = list(done = TRUE)), active = list(active = function(value) {
        writeLines("getter", marker)
        stop("getter")
    }))
    ns <- new.env(parent = baseenv())
    ns$client <- Client$new()
    ns$api <- list(client = ns$client)
    ns$private_client <- ns$client
    unlink(marker)
    snapshot <- member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "namespacefixture", exports = c("client", "api")
    )))
    expected <- c("active", "base", "clone", "initialize", "read", "step", "value")
    expect_identical(namespace_member_labels("namespacefixture::client$", snapshot), expected)
    expect_identical(namespace_member_labels("library(namespacefixture)\nclient$", snapshot), expected)
    expect_length(namespace_member_labels("namespacefixture::private_client$", snapshot), 0L)
    expect_identical(namespace_member_labels("namespacefixture:::private_client$", snapshot), expected)
    expect_identical(namespace_member_labels("namespacefixture::client$step(stop(\"argument\"))$", snapshot), expected)
    expect_identical(namespace_member_labels("namespacefixture::client$base()$", snapshot), expected)
    expect_identical(namespace_member_labels("namespacefixture::client$read()$", snapshot), "done")
    expect_identical(namespace_member_labels("namespacefixture::api$client$", snapshot), expected)
    expect_identical(namespace_member_labels("namespacefixture::api$client$step()$", snapshot), expected)
    expect_length(namespace_member_labels("namespacefixture::client$active$", snapshot), 0L)
    expect_length(namespace_member_labels("namespacefixture::client$.__enclos_env__$", snapshot), 0L)
    item <- namespace_member_items("namespacefixture::client$ste", snapshot)[[1L]]
    expect_identical(item$detail, "step(amount = 1L)")
    expect_identical(item$kind, CompletionItemKind$Method)
    expect_false(file.exists(marker))
})
