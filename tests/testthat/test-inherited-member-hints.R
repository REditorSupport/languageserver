inherited_member_fixture <- function(lines, uri = "file:///inherited-members.R", workspace = NULL) {
    if (is.null(workspace)) workspace <- Workspace$new(NULL)
    doc <- Document$new(uri, language = "r", version = 1L, content = lines)
    workspace$documents$set(uri, doc)
    parsed <- parse_document(uri, lines)
    parsed$version <- doc$version
    workspace$update_parse_data(uri, parsed)
    if (isTRUE(workspace$index$enabled)) workspace$index$update_content(uri, lines, parse_data = parsed)
    list(uri = uri, document = doc, workspace = workspace)
}

inherited_member_labels <- function(f, row, prefix) {
    point <- list(row = as.integer(row - 1L), col = nchar(prefix))
    items <- member_completion(f$uri, f$workspace, f$document, point, FALSE, 200L)
    vapply(items, `[[`, "", "label")
}

test_that("inherited argument types reach the cursor, fluent calls and wrapper results", {
    f <- inherited_member_fixture(c(
        'Box <- R6::R6Class("Box", public = list(step = function(n) self, collect = function() list(payload = 1)))',
        "#' @param item (`Box`) A box.", "#' @returns (`Box`) A box.",
        "donor <- function(item) opaque()", "#' @inheritParams donor",
        "relay <- function(item, n) {", "    item$step(n)$collect()", "}",
        "alias <- relay", "alias(opaque(), 1)$payload", "#' @inheritParams donor",
        "restore <- function(item) opaque()", "restore(opaque())$nothing"))
    expect_setequal(inherited_member_labels(f, 7L, "    item$"), c("clone", "collect", "step"))
    expect_setequal(inherited_member_labels(f, 7L, "    item$step(n)$"), c("clone", "collect", "step"))
    expect_identical(inherited_member_labels(f, 10L, "alias(opaque(), 1)$"), "payload")
    expect_length(inherited_member_labels(f, 13L, "restore(opaque())$"), 0L)
    point <- list(row = 6L, col = nchar("    item$step(n)$col"))
    definition <- member_definition_location(f$uri, f$workspace, f$document, point)$result
    expect_equal(definition$range$start, position(0L, regexpr("collect", f$document$content[[1L]], fixed = TRUE)[[1L]] - 1L))
})

test_that("consumers resolve inherited types in the donor scope and preserve direct documentation", {
    f <- inherited_member_fixture(c(
        'Box <- R6::R6Class("Outside", public = list(from_donor = TRUE))',
        "#' @param x (`Box`) First box.", "#' @param y (`Box`) Second box.", "described <- function(x, y) x",
        "factory <- function() {", '    Box <- R6::R6Class("Inside", public = list(from_recipient = TRUE))',
        "    #' @inheritParams described", "    #' @param y Local prose with no type.",
        "    function(x, y) {", "        x$from_donor", "        y$local_prose", "        x", "    }", "}",
        "factory()(opaque(), 1)$from_donor", "factory()(list(actual = TRUE), 1)$actual"))
    expect_setequal(inherited_member_labels(f, 10L, "        x$"), c("clone", "from_donor"))
    expect_length(inherited_member_labels(f, 11L, "        y$"), 0L)
    expect_setequal(inherited_member_labels(f, 15L, "factory()(opaque(), 1)$"), c("clone", "from_donor"))
    expect_identical(inherited_member_labels(f, 16L, "factory()(list(actual = TRUE), 1)$"), "actual")
})

test_that("concrete, missing and unused arguments do not look up donor documentation", {
    f <- inherited_member_fixture(c(
        'Box <- R6::R6Class("Box", public = list(wrong = TRUE))',
        "#' @param item (`Box`) A box.", "donor <- function(item) item",
        "#' @inheritParams donor", "known <- function(item) list(real = 1)",
        "#' @inheritParams donor", "relay <- function(item, result = item) result",
        "known(opaque())$real", "relay(list(supplied = 1))$supplied", "relay()$probe"))
    testthat::local_mocked_bindings(roxygen_effective_params = function(...) stop("Forced unused inheritance"))
    expect_identical(inherited_member_labels(f, 8L, "known(opaque())$"), "real")
    expect_identical(inherited_member_labels(f, 9L, "relay(list(supplied = 1))$"), "supplied")
    expect_length(inherited_member_labels(f, 10L, "relay()$"), 0L)
})

test_that("an S3 method can return a receiver documented by another source function", {
    f <- inherited_member_fixture(c(
        'Input <- R6::R6Class("Input", public = list(done = TRUE))',
        "#' @param payload (`Input`) An input.", "document_input <- function(payload) payload",
        'draw <- function(x, ...) UseMethod("draw")', "#' @inheritParams document_input",
        "draw.widget <- function(x, payload) payload", 'widget <- structure(1, class = "widget")',
        "draw(widget, opaque())$done"))
    expect_setequal(inherited_member_labels(f, 8L, "draw(widget, opaque())$"), c("clone", "done"))
})

test_that("a source callback retains its inherited formals through a package call", {
    index <- member_generic_index("apply_one <- function(fn, value) fn(value)")
    index$package <- "mapfixture"
    index$exports <- "apply_one"
    index$roots$apply_one <- member_value(function_key = "apply_one", metadata = "mapfixture")
    metadata <- MemberMetadataCache$new(1024^2)
    metadata$set("mapfixture", member_index_freeze(index))
    f <- inherited_member_fixture(c(
        'Box <- R6::R6Class("Box", public = list(answer = 42))', "#' @param item (`Box`) A box.",
        "donor <- function(item) item", "#' @inheritParams donor", "relay <- function(item) item",
        "mapfixture::apply_one(relay, unknown())$answer"))
    f$workspace$member_metadata <- metadata
    expect_setequal(inherited_member_labels(f, 6L, "mapfixture::apply_one(relay, unknown())$"), c("answer", "clone"))
})

test_that("inherited unions keep shared members and a concrete argument narrows them", {
    f <- inherited_member_fixture(c(
        'A <- R6::R6Class("A", public = list(common = 1, only_a = 1))',
        'B <- R6::R6Class("B", public = list(common = 2, only_b = 2))',
        "#' @param item (`A | B`) Either input.", "describe <- function(item) item",
        "#' @inheritParams describe", "relay <- function(item) item",
        "relay(opaque())$common", "relay(B$new())$only_b"))
    expect_setequal(inherited_member_labels(f, 7L, "relay(opaque())$"), c("clone", "common"))
    expect_setequal(inherited_member_labels(f, 8L, "relay(B$new())$"), c("clone", "common", "only_b"))
})

test_that("one request resolves a shared donor class once and caches only complete work", {
    f <- inherited_member_fixture(c(
        'Box <- R6::R6Class("Box", public = list(item = 1))',
        "#' @param a,b (`Box`) Two boxes.", "donor <- function(a, b) a",
        "#' @inheritParams donor", "use <- function(a, b, flag) {",
        "  chosen <- if (flag) a else b", "  chosen$item", "}"))
    original <- member_hinted_value
    calls <- 0L
    flag <- NULL
    testthat::local_mocked_bindings(member_hinted_value = function(types, index, bindings, budget) {
        calls <<- calls + 1L
        value <- original(types, index, bindings, budget)
        if (!is.null(flag)) budget[[flag]] <- TRUE
        value
    })
    expect_setequal(inherited_member_labels(f, 7L, "  chosen$"), c("clone", "item"))
    expect_identical(calls, 1L)

    record <- list(types = "Box", uri = f$uri, row = 2L, col = nchar("donor <- "))
    for (flag in c("exhausted", "transient")) {
        budget <- member_request_budget()
        member_inherited_hint_value(record, f$workspace, budget)
        expect_true(budget[[flag]])
        expect_length(budget$inherited_values, 0L)
    }
})

test_that("an inherited S4 instance description supplies slots", {
    f <- inherited_member_fixture(c(
        'methods::setClass("Record", slots = c(payload = "numeric"))',
        "#' @param x (`Record`) A record.", "describe <- function(x) x",
        "#' @inheritParams describe", "consume <- function(x) x@payload"))
    expect_identical(inherited_member_labels(f, 5L, "consume <- function(x) x@"), "payload")
})

test_that("Polars group_by keeps its aggregation methods through an inherited parameter hint", {
    skip_if_not_installed("polars")
    f <- inherited_member_fixture(c(
        "library(polars)", "#' @param data (`polars_data_frame`) A frame.",
        "describe_frame <- function(data) data", "#' @inheritParams describe_frame",
        "summarize <- function(data, grp) data$group_by(grp)$agg()"))
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    metadata$set("polars", member_prepare_package("polars"))
    f$workspace$member_metadata <- metadata
    expect_true("group_by" %in% inherited_member_labels(f, 5L, "summarize <- function(data, grp) data$"))
    expect_true("agg" %in% inherited_member_labels(f, 5L, "summarize <- function(data, grp) data$group_by(grp)$"))
})

test_that("an edit to a donor refreshes an unchanged recipient and its caller", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: inheritedfixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)
    open <- function(file, lines) {
        path <- file.path(root, "R", file)
        writeLines(lines, path)
        inherited_member_fixture(lines, path_to_uri(path), workspace)
    }
    source <- c('First <- R6::R6Class("First", public = list(first = TRUE))',
        'Second <- R6::R6Class("Second", public = list(second = TRUE))',
        "#' @param item (`First`) A record.", "described <- function(item) item")
    donor <- open("donor.R", source)
    recipient <- open("recipient.R", c("#' @inheritParams described", "relay <- function(item) {", "  item$first", "  item", "}"))
    caller <- open("caller.R", c("cached <- relay(opaque())", "cached$probe"))
    for (i in 1:2) {
        expect_setequal(inherited_member_labels(recipient, 3L, "  item$"), c("clone", "first"))
        expect_setequal(inherited_member_labels(caller, 2L, "cached$"), c("clone", "first"))
    }
    definition <- member_definition_location(recipient$uri, workspace, recipient$document, list(row = 2L, col = 8L))$result
    expect_identical(definition$uri, donor$uri)
    expect_equal(definition$range$start$line, 0L)
    source[[3L]] <- "#' @param item (`Second`) A record."
    doc <- donor$document
    doc$set_content(doc$version + 1L, source)
    parsed <- parse_document(doc$uri, source)
    parsed$version <- doc$version
    workspace$update_parse_data(doc$uri, parsed)
    workspace$index$update_content(doc$uri, source, parse_data = parsed)
    expect_setequal(inherited_member_labels(recipient, 3L, "  item$"), c("clone", "second"))
    expect_setequal(inherited_member_labels(caller, 2L, "cached$"), c("clone", "second"))
})

test_that("an assignment cached before a forward donor follows documentation-only edits", {
    lines <- c('A <- R6::R6Class("A", public = list(read = function() list(first = 1)))',
        'B <- R6::R6Class("B", public = list(read = function() list(second = 2)))',
        "#' @inheritParams described", "read <- function(item) item$read()",
        "cached <- read(opaque())", "cached$probe",
        "#' @param item (`A`) An input.", "described <- function(item) item")
    f <- inherited_member_fixture(lines)
    for (i in 1:2) expect_identical(inherited_member_labels(f, 6L, "cached$"), "first")
    lines[[7L]] <- "#' @param item (`B`) An input."
    doc <- f$document
    doc$set_content(doc$version + 1L, lines)
    parsed <- parse_document(doc$uri, lines)
    parsed$version <- doc$version
    f$workspace$update_parse_data(doc$uri, parsed)
    expect_identical(inherited_member_labels(f, 6L, "cached$"), "second")
})
