inference_review_labels <- function(code, snapshots = list(), point = NULL) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    document <- Document$new("file:///inference-review.R", language = "r", version = 1L, content = content)
    data <- parse_document(document$uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    if (is.null(point)) point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    items <- member_completion(document$uri, list(member_metadata = metadata), document,
        point, FALSE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

test_that("Removing class attributes preserves source and static namespace members", {
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(alpha = list(done = TRUE)), class = "review_record")
    snapshots <- list(reviewfixture = member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "reviewfixture", exports = "record"
    ))))
    for (class in c("NULL", "character()", "base::character()")) {
        expect_identical(inference_review_labels(sprintf("structure(list(alpha = 1), class = %s)$", class)), "alpha")
        expect_identical(inference_review_labels(sprintf("structure(reviewfixture::record, class = %s)$alpha$", class),
                snapshots), "done")
    }
    expect_length(inference_review_labels(paste0(
        "character <- function() stop(\"unknown class\")\n",
        "structure(reviewfixture::record, class = character())$"
    ), snapshots), 0L)
})

test_that("Named namespace elements survive class changes and custom dollar dispatch", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$record <- list(alpha = list(done = TRUE))
    ns$custom <- structure(list(alpha = list(done = TRUE)), class = "review_custom_dollar")
    ns$`$.review_custom_dollar` <- function(x, name) {
        writeLines("dispatch", marker)
        stop("custom dollar")
    }
    snapshots <- list(reviewfixture = member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "reviewfixture", exports = c("record", "custom")
    ))))
    for (class in c("NULL", "character()", "\"list\"")) {
        expect_identical(inference_review_labels(sprintf("structure(reviewfixture::record, class = %s)[[1L]]$", class),
                snapshots), "done")
    }
    expect_identical(inference_review_labels("reviewfixture::custom[[1L]]$", snapshots), "done")
    expect_identical(inference_review_labels(paste0(
        "choose <- function() if(length(structure(reviewfixture::record, class = \"list\")) == 1L) ",
        "list(selected = TRUE) else list(other = TRUE)\nchoose()$"
    ), snapshots), "selected")
    expect_false(file.exists(marker))
})

test_that("Local dollar overrides invalidate static members inside function scopes", {
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(alpha = TRUE), class = "review_record")
    snapshots <- list(reviewfixture = member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "reviewfixture", exports = "record"
    ))))
    for (method in c("$.review_record", "$.default")) {
        code <- paste(c("f <- function() {",
                sprintf("`%s` <- function(x, name) list(other = TRUE)", method),
                "reviewfixture::record$al", "}"), collapse = "\n")
        expect_length(inference_review_labels(code, snapshots,
                point = list(row = 2L, col = nchar("reviewfixture::record$al"))), 0L)
    }
})

test_that("Static namespace indexing respects known subset methods", {
    marker <- withr::local_tempfile()
    ns <- new.env(parent = baseenv())
    ns$record <- structure(list(alpha = list(done = TRUE)), class = "review_subset")
    ns$`[[.review_subset` <- function(x, i, j = "default", ...) {
        writeLines("method", marker)
        if (j == "other") list(other = TRUE) else list(actual = TRUE)
    }
    snapshots <- list(reviewfixture = member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "reviewfixture", exports = "record"
    ))))
    expect_identical(inference_review_labels("reviewfixture::record[[1L]]$", snapshots), "actual")
    expect_identical(inference_review_labels('reviewfixture::record[[1L, j = "other"]]$', snapshots), "other")
    expect_false(file.exists(marker))
})
