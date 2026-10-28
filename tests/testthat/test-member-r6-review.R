r6_review_snapshot <- function(object) {
    ns <- new.env(parent = baseenv())
    ns$object <- object
    member_index_freeze(member_package_index(member_namespace_input(
        ns, package = "r6reviewfixture", exports = "object"
    )))
}

r6_review_labels <- function(code, snapshot) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///r6-review.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    data <- parse_document(uri, content)
    data$version <- 1L
    document$update_parse_data(data)
    metadata <- collections::dict()
    metadata$set("r6reviewfixture", member_index_thaw(snapshot))
    workspace <- list(member_metadata = metadata)
    point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    items <- member_completion(uri, workspace, document, point, TRUE, 200L)
    vapply(items, `[[`, character(1L), "label")
}

test_that("Stored R6 function fields keep their original lexical bindings", {
    marker <- withr::local_tempfile()
    Class <- R6::R6Class("StoredCallbacks", public = list(
        callback = NULL, unrelated = NULL, mimic = NULL,
        run = function() self,
        read = function() private$first()
    ), private = list(payload = list(done = TRUE),
        first = function() private$second(), second = function() private$payload))
    object <- Class$new()
    object$callback <- local({
        self <- 42L
        function() list(value = self)
    })
    object$unrelated <- function() {
        writeLines("method", marker)
        private$payload
    }
    object$mimic <- local({
        self <- object
        private <- NULL
        function() {
            self
            private$payload
        }
    })
    snapshot <- r6_review_snapshot(object)
    index <- member_index_thaw(snapshot)
    value <- member_infer(str2lang("r6reviewfixture::object$callback()$value"), index, index$roots)
    expect_identical(value$literal, 42L)
    expect_identical(r6_review_labels("r6reviewfixture::object$callback()$", snapshot), "value")
    expect_length(r6_review_labels("r6reviewfixture::object$unrelated()$", snapshot), 0L)
    expect_length(r6_review_labels("r6reviewfixture::object$mimic()$", snapshot), 0L)
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "done")
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot),
        c("callback", "clone", "mimic", "read", "run", "unrelated"))
    expect_false(file.exists(marker))
})

test_that("Stored inherited R6 methods retain each lexical super environment", {
    Grand <- R6::R6Class("StoredGrand", public = list(run = function() list(grand = TRUE)))
    Parent <- R6::R6Class("StoredParent", inherit = Grand, public = list(run = function() super$run()))
    Child <- R6::R6Class("StoredChild", inherit = Parent, public = list(run = function() super$run()))
    snapshot <- r6_review_snapshot(Child$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$run()$", snapshot), "grand")
})

test_that("Stored non-portable R6 instances conceal their internal bindings", {
    Class <- R6::R6Class("StoredNonPortable", portable = FALSE,
        public = list(read = function() private$payload),
        private = list(payload = list(done = TRUE)))
    snapshot <- r6_review_snapshot(Class$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$", snapshot), c("clone", "read"))
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "done")
    expect_length(r6_review_labels("r6reviewfixture::object$private$", snapshot), 0L)
})

test_that("Stored R6 locked data bindings survive invalid replacement attempts", {
    Class <- R6::R6Class("StoredLockedData", public = list(payload = list(done = TRUE)))
    object <- Class$new()
    lockBinding("payload", object)
    snapshot <- r6_review_snapshot(object)
    expect_identical(r6_review_labels(paste0(
        "f <- function() { object <- r6reviewfixture::object\nobject$payload <- unknown()\nobject$payload$"
    ), snapshot), "done")
})

test_that("Stored R6 method contexts preserve private method binding locks", {
    Class <- R6::R6Class("StoredPrivateLocks", public = list(read = function() {
        private$helper <- function() list(fake = TRUE)
        private$helper()
    }), private = list(helper = function() list(real = TRUE)))
    snapshot <- r6_review_snapshot(Class$new())
    expect_identical(r6_review_labels("r6reviewfixture::object$read()$", snapshot), "real")
})
