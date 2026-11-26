test_that("documented links require class evidence from the named package", {
    linked <- function(types, proof = types) structure(types, roxygen_class_links = proof)
    index <- member_generic_index("")
    index$hint_packages <- c("base", "stats", "registered")
    index$namespace_indices <- list(registered = list(classes = list(receiver = c("receiver", "parent"))))
    infer <- function(types, scope = list()) member_hinted_value(types, index, scope, member_request_budget())

    expect_null(infer(linked("ordinary")))
    expect_identical(infer("ordinary")$type, "ordinary")
    expect_identical(infer(linked("base::integer"))$type, "integer")
    expect_null(infer(linked("other::integer")))
    expect_identical(infer(linked("stats::lm"))$classes, "lm")
    expect_identical(infer(linked("Date"))$classes, "Date")
    expect_null(infer(linked("Date"), list(Date = member_literal(1L))))
    expect_null(infer(linked("utils::data.frame")))
    expect_identical(infer(linked("registered::receiver"))$member_package, "registered")
    expect_null(infer(linked("other::receiver")))
    expect_identical(infer(linked("receiver"))$member_package, "registered")
    index$attached_roots <- list(
        Date = member_value(function_key = "ordinary", metadata = "base"),
        integer = member_literal(1L),
        receiver = member_value(function_key = "receiver", metadata = "maskpkg"),
        glm = member_value(function_key = "glm", metadata = "stats"))
    expect_null(infer(linked("Date")))
    expect_null(infer(linked("integer")))
    expect_null(infer(linked("receiver")))
    expect_identical(infer(linked("base::Date"))$classes, "Date")
    expect_identical(infer(linked("base::integer"))$type, "integer")
    expect_identical(infer(linked("glm"))$classes, c("glm", "lm"))
    index$attached_roots <- list()
    index$namespace_indices$other <- list(classes = list(receiver = "receiver"))
    index$hint_packages <- c(index$hint_packages, "other")
    expect_null(infer(linked(c("receiver", "base::integer"))))
    expect_null(infer(linked(c("base::integer", "ordinary"), "ordinary")))
    options <- infer(linked(c("ordinary", "base::integer"), "base::integer"))$possible_returns
    expect_identical(vapply(options, `[[`, "", "type"), c("ordinary", "integer"))
})

test_that("linked formal, return and field types reach the member and dispatch providers", {
    definitions <- c('Box <- R6::R6Class("Box", public = list(item = 1L, step = function() self))',
        'paint <- function(x, ...) UseMethod("paint")',
        "paint.Box <- function(x, stroke = TRUE) x", "paint.article <- function(x, bad = TRUE) x",
        "paint.lm <- function(x, model = TRUE) x",
        "paint.default <- function(x, fallback = TRUE) x")
    r6 <- s3_prepare_namespace("R6")
    check <- function(documentation, body, expected = NULL) {
        code <- c(definitions, documentation, body)
        f <- provider_fixture(code)
        row <- length(code) - 1L
        point <- list(row = row, col = nchar(code[[row + 1L]]) - 1L)
        f$workspace$s3_metadata <- S3MetadataCache$new()
        for (snapshot in provider_startup_s3()) f$workspace$s3_metadata$set(snapshot$package, snapshot)
        f$workspace$s3_metadata$set("R6", r6)
        call <- resolve_s3_call(f$uri, f$workspace, f$document, point)
        expect_identical(call$method$name, expected)
    }
    check("#' @param x [`Box`] Input.", "use <- function(x) paint(x, )", "paint.Box")
    check("#' @param x [`stats::glm`] Fitted model.", "use <- function(x) paint(x, )", "paint.lm")
    check("#' @param x [`other::glm`] A topic.", "use <- function(x) paint(x, )")
    check("#' @param x [`article`] Read the topic.", "use <- function(x) paint(x, )")
    check("#' @param x [`Box`] or [`article`] Input.", "use <- function(x) paint(x, )")
    check("#' @param x (`article`) Explicit nominal type.", "use <- function(x) paint(x, )", "paint.article")
    check(c("#' @returns [`Box`] Produced instance.", "make <- function() opaque()"), "paint(make(), )", "paint.Box")
    check(c("#' @returns [`article`] Read the topic.", "make <- function() opaque()"), "paint(make(), )")
    check(c("#' @slot value [`article`] A topic.",
            'Holder <- methods::setClass("Holder", slots = list(value = "ANY"))'), "paint(Holder()@value, )")
    check(c("#' @slot value [`Box`] A receiver.",
            'Holder <- methods::setClass("Holder", slots = list(value = "ANY"))'), "paint(Holder()@value, )", "paint.Box")
    check(c("#' @field value [`article`] A topic.",
            'Holder <- R6::R6Class("Holder", public = list(value = NULL))'), "paint(Holder$new()$value, )")
    check(c("#' @field value [`Box`] A receiver.",
            'Holder <- R6::R6Class("Holder", public = list(value = NULL))'), "paint(Holder$new()$value, )", "paint.Box")
    check(c("#' @param x [`article`] Donor.", "donor <- function(x) NULL", "#' @inheritParams donor"),
        "use <- function(x) paint(x, )")
    check(c("#' @param x [`Box`] Donor.", "donor <- function(x) NULL", "#' @inheritParams donor"),
        "use <- function(x) paint(x, )", "paint.Box")

    labels <- function(annotation) {
        f <- provider_fixture(c(definitions, annotation, "use <- function(x) x$step()$probe"))
        point <- list(row = f$document$nline - 1L, col = nchar("use <- function(x) x$step()$"))
        vapply(member_completion(f$uri, f$workspace, f$document, point, TRUE, 100L), `[[`, "", "label")
    }
    expect_setequal(labels("#' @param x [`Box`] A receiver."), c("clone", "item", "step"))
    expect_length(labels("#' @param x [`Box`] or [`article`] Alternatives."), 0L)
})

test_that("donor caches distinguish linked topics and explicit nominal types in the same declaration", {
    f <- provider_fixture(c("article <- function() NULL", "donor <- function(left, right) NULL"))
    direct <- list(types = "article", uri = f$uri, row = 1L, col = 0L)
    linked <- direct
    attr(linked$types, "roxygen_class_links") <- "article"
    for (docs in list(list(direct, linked), list(linked, direct))) {
        budget <- member_request_budget()
        results <- lapply(docs, member_inherited_hint_value, workspace = f$workspace, budget = budget)
        for (i in seq_along(docs)) {
            if (is.null(attr(docs[[i]]$types, "roxygen_class_links", exact = TRUE))) {
                expect_identical(results[[i]]$type, "article")
            } else {
                expect_null(results[[i]]$type)
            }
        }
    }
})

test_that("qualified Polars class links retain the returned group-by methods", {
    skip_if_not_installed("polars")
    f <- provider_fixture(c("#' @param data [`polars::polars_data_frame`] Input.",
            "aggregate <- function(data, grp) data$group_by(grp)$agg()"))
    f$workspace$member_metadata <- MemberMetadataCache$new(32 * 1024^2)
    f$workspace$member_metadata$set("polars", member_prepare_package("polars"))
    for (query in list(c("aggregate <- function(data, grp) data$", "group_by"),
            c("aggregate <- function(data, grp) data$group_by(grp)$", "agg"))) {
        point <- list(row = 1L, col = nchar(query[[1L]]))
        items <- member_completion(f$uri, f$workspace, f$document, point, TRUE, 200L)
        expect_true(query[[2L]] %in% vapply(items, `[[`, "", "label"))
    }
})
