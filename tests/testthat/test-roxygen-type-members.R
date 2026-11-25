hinted_members <- function(fixture, row, prefix) {
    point <- list(row = row, col = nchar(prefix))
    items <- member_completion(fixture$uri, fixture$workspace, fixture$document, point, TRUE, 200L)
    vapply(items, `[[`, "", "label")
}

test_that("hinted R6 instances preserve member identities across editor providers", {
    f <- provider_fixture(c(
        'Parent <- R6::R6Class("Parent", public = list(filter = function(predicate) self))',
        'Pipeline <- R6::R6Class("Pipeline", inherit = Parent, public = list(',
        '  limit = function(n = 10L) self, collect = function(format = "list") list(data = NULL)),',
        "  private = list(remember = function(n) self))",
        "#' @param pipeline (`Pipeline`) Pipeline to run.",
        "fun2 <- function(pipeline, n) {",
        "  alias <- pipeline$limit(n)",
        '  alias$collect("wide")',
        "  pipeline <- list(replaced = TRUE)",
        "  pipeline$replaced", "}"))
    f$workspace$get_documentation <- function(...) NULL
    expect_setequal(hinted_members(f, 6L, "  alias <- pipeline$"), c("limit", "collect", "filter", "clone"))
    expect_setequal(hinted_members(f, 7L, "  alias$"), c("limit", "collect", "filter", "clone"))
    expect_identical(hinted_members(f, 9L, "  pipeline$"), "replaced")
    point <- list(row = 7L, col = 10L)
    hover <- hover_reply(1L, f$uri, f$workspace, f$document, point)$result
    expect_match(hover$contents[[1L]], 'collect(format = "list")', fixed = TRUE)
    signature <- signature_reply(1L, f$uri, f$workspace, f$document, list(row = 7L, col = 16L))$result
    expect_identical(signature$signatures[[1L]]$label, 'collect(format = "list")')
    definition <- member_definition_location(f$uri, f$workspace, f$document, point)$result
    expect_identical(definition$uri, f$uri)
    expect_equal(definition$range$start$line, 2L)
    type <- type_definition_reply(1L, f$uri, f$workspace, f$document, list(row = 6L, col = 14L))$result
    expect_equal(type$range$start, position(1L, 0L))
    workspace <- Workspace$new(NULL)
    workspace$documents$set(f$uri, f$document)
    workspace$update_parse_data(f$uri, f$document$parse_data)
    minimum <- lsp_settings$get("inlay_hints_minimum_arguments")
    withr::defer(lsp_settings$set("inlay_hints_minimum_arguments", minimum))
    lsp_settings$set("inlay_hints_minimum_arguments", 1L)
    hints <- inlay_hint_reply(1L, f$uri, workspace, f$document, range(position(7L, 0L), position(8L, 0L)))$result
    expect_identical(vapply(hints, `[[`, "", "label"), "format =")
    refs <- references_reply(1L, f$uri, workspace, f$document, point)$result
    expect_setequal(vapply(refs, function(loc) loc$range$start$line, integer(1L)), c(2L, 7L))
    rename <- rename_reply(1L, f$uri, workspace, f$document, point, "fetch")$result
    expect_setequal(vapply(rename$changes[[f$uri]], function(edit) edit$range$start$line, integer(1L)), c(2L, 7L))
})

test_that("package class hints resolve Polars methods through group_by with an unknown key", {
    skip_if_not_installed("polars")
    metadata <- MemberMetadataCache$new(32 * 1024^2)
    metadata$set("polars", member_prepare_package("polars"))
    for (qualified in c(FALSE, TRUE)) {
        f <- provider_fixture(c(
            if (qualified) "" else "library(polars)",
            if (qualified) "#' @param data (`polars::polars_data_frame`)" else "#' @param data (`polars_data_frame`)",
            "fun1 <- function(data, grp) {", "  data$group_by(grp)$agg()", "}"))
        f$workspace$member_metadata <- metadata
        expect_true("group_by" %in% hinted_members(f, 3L, "  data$"))
        expect_true("agg" %in% hinted_members(f, 3L, "  data$group_by(grp)$"))
    }
})

test_that("Go to Type Definition offers both source classes of a declared union", {
    f <- provider_fixture(c('Parent <- R6::R6Class("Parent")',
            'Pipeline <- R6::R6Class("Pipeline", inherit = Parent)',
            "#' @param pipeline (`Pipeline | Parent`)", "f <- function(pipeline) pipeline"))
    targets <- type_definition_reply(1L, f$uri, f$workspace, f$document, list(row = 3L, col = 26L))$result
    expect_setequal(vapply(targets, function(loc) loc$range$start$line, integer(1L)), c(0L, 1L))
})

test_that("filtered and limited union completion only inspects returned slots with one budget", {
    f <- provider_fixture(c('methods::setClass("Fields", slots = c(alpha = "integer", beta = "integer", gamma = "integer"))',
            'methods::setClass("Left", contains = "Fields")', 'methods::setClass("Right", contains = "Fields")',
            "#' @param x (`Left | Right`)", "f <- function(x) x@gamma"))
    slot <- member_slot
    observed <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(member_slot = function(value, name, index, bindings, budget = NULL) {
        stopifnot(is.environment(budget))
        observed$names <- c(observed$names, name)
        if (!is.null(observed$budget)) stopifnot(identical(budget, observed$budget))
        observed$budget <- budget
        slot(value, name, index, bindings, budget)
    })
    for (prefix in c("gamma", "")) {
        observed$names <- observed$budget <- NULL
        point <- list(row = 4L, col = nchar(paste0("f <- function(x) x@", prefix)))
        items <- member_completion(f$uri, f$workspace, f$document, point, TRUE, 1L)
        want <- if (nzchar(prefix)) "gamma" else "alpha"
        expect_identical(vapply(items, `[[`, "", "label"), want)
        expect_identical(unique(observed$names), want)
    }
})

test_that("a member request waits for the attached package of a documented receiver", {
    f <- provider_fixture(c("library(polars)", "#' @param data (`polars_data_frame`)",
            "f <- function(data) data$group_by()", "outside <- list(ready = TRUE)", "outside$ready"))
    f$document$requested_packages <- member_package_request(f$document$parse_data)
    server <- list(member_task_manager = list(has = function(uri) TRUE), get_workspace = function(uri) f$workspace)
    expect_true(member_metadata_waiting(server, f$uri, f$document, list(row = 2L, col = 25L)))
    expect_false(member_metadata_waiting(server, f$uri, f$document, list(row = 4L, col = 8L)))
})

test_that("formal hints use reference-class, S4 and S7 instance declarations", {
    f <- provider_fixture(c(
        'Frame <- methods::setRefClass("Frame", fields = list(value = "numeric"))',
        'methods::setClass("Record", slots = c(payload = "numeric"))',
        "#' @param x (`Frame`) Input.", "#' @param y (`Record`) Output.",
        "f <- function(x, y) {", "  x$value", "  y@payload", "}"))
    expect_true("value" %in% hinted_members(f, 5L, "  x$"))
    expect_identical(hinted_members(f, 6L, "  y@"), "payload")
    skip_if_not_installed("S7")
    f <- provider_fixture(c('Box <- S7::new_class("Box", properties = list(value = S7::class_numeric))',
            "#' @param x (`Box`)", "f <- function(x) x@value"))
    expect_true("value" %in% hinted_members(f, 2L, "f <- function(x) x@"))
})

test_that("package files share return hints and invalidate providers on a documentation-only edit", {
    root <- index_normalize_path(withr::local_tempdir())
    dir.create(file.path(root, "R"))
    writeLines("Package: hintfixture", file.path(root, "DESCRIPTION"))
    writeLines("", file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)
    open <- function(name, lines) {
        path <- file.path(root, "R", name)
        writeLines(lines, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, version = 1L, content = lines)
        workspace$documents$set(uri, doc)
        update(doc, lines)
        list(uri = uri, workspace = workspace, document = doc)
    }
    update <- function(doc, lines) {
        doc$set_content(doc$version + 1L, lines)
        parsed <- parse_document(doc$uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(doc$uri, parsed)
        workspace$index$update_content(doc$uri, lines, parse_data = parsed)
    }
    open("classes.R", c('First <- R6::R6Class("First", public = list(first = 1L))',
            'Second <- R6::R6Class("Second", public = list(second = 1L))'))
    factory <- open("factory.R", c("#' @return (`First`)", "factory <- function() opaque()"))
    open("wrappers.R", c("restored <- factory", "wrapper <- function() restored()"))
    caller <- open("caller.R", "wrapper()$field")
    expect_setequal(hinted_members(caller, 0L, "wrapper()$"), c("first", "clone"))
    update(factory$document, c("#' @return (`Second`)", "factory <- function() opaque()"))
    expect_setequal(hinted_members(caller, 0L, "wrapper()$"), c("second", "clone"))
    update(factory$document, c("#' @return A record.", "factory <- function() opaque()"))
    expect_length(hinted_members(caller, 0L, "wrapper()$"), 0L)
})
