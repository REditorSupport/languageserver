type_hint_fixture <- function(lines) {
    row <- grep("@@", lines, fixed = TRUE)
    col <- regexpr("@@", lines[[row]], fixed = TRUE)[[1L]] - 1L
    lines[[row]] <- sub("@@", "", lines[[row]], fixed = TRUE)
    c(provider_fixture(lines), list(point = list(row = row - 1L, col = col)))
}

type_hint_labels <- function(f) {
    items <- member_completion(f$uri, f$workspace, f$document, f$point, TRUE, 200L)
    sort(vapply(items, `[[`, "", "label"))
}

type_hint_box <- c('Base <- R6::R6Class("Base", public = list(filter = function(x) self))',
    'Box <- R6::R6Class("Box", inherit = Base, public = list(limit = function(n) self, collect = function() list(data = 1)),',
    "    private = list(secret = TRUE))")

test_that("documented arguments complete the public instance and follow fluent calls", {
    for (expr in c("box$@@limit(n)", "box$limit(n)$@@collect()", "box$@@")) {
        f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) An instance.",
                "use <- function(box, n) {", paste0("  ", expr), "}"))
        expect_identical(type_hint_labels(f), c("clone", "collect", "filter", "limit"))
    }
    f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) An instance.",
            "use <- function(box) {", "  box <- list(replaced = TRUE)", "  box$@@replaced", "}"))
    expect_identical(type_hint_labels(f), "replaced")
})

test_that("explicit return hints fill unknown results while retaining concrete evidence", {
    for (tag in c("@return", "@returns")) {
        f <- type_hint_fixture(c(type_hint_box, paste("#'", tag, "(`Box`) A box."),
                "restore <- function(path) readRDS(path)", "restore(path)$limit(3)$@@collect()"))
        expect_true("collect" %in% type_hint_labels(f))
    }
    f <- type_hint_fixture(c(type_hint_box, "#' @return (`Box`) A box.",
            "known <- function() list(real = 1)", "known()$@@real"))
    expect_identical(type_hint_labels(f), "real")
    f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) A box.",
            "wrapper <- function(box) box", "wrapper(list(actual = TRUE))$@@actual"))
    expect_identical(type_hint_labels(f), "actual")
})

test_that("the opt-out changes an already cached completion without reparsing", {
    previous <- lsp_settings$get("roxygen_type_hints")
    withr::defer(lsp_settings$set("roxygen_type_hints", previous))
    f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) A box.", "use <- function(box) box$@@collect"))
    for (enabled in c(TRUE, FALSE, TRUE)) {
        lsp_settings$set("roxygen_type_hints", enabled)
        expect_identical("collect" %in% type_hint_labels(f), enabled)
    }
})

test_that("type hints use only explicit markers in actual roxygen comments", {
    for (doc in c("#' @param box See `Box`.", "#' @param box (`Box$new()`) Dynamic.",
            "#' @param box (`Box`) or (`Other`) An alternative.", "#' @param box (`Box | Other`) A union.",
            "# @param box (`Box`) An ordinary comment.", "#' @param other (`Box`) A different argument.")) {
        f <- type_hint_fixture(c(type_hint_box, doc, "use <- function(box) box$@@collect"))
        expect_false("collect" %in% type_hint_labels(f))
    }
    f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) Outer box.",
            "use <- function(box) {", "  inner <- function(box) box$@@collect", "}"))
    expect_false("collect" %in% type_hint_labels(f))
    f <- type_hint_fixture(c(type_hint_box, "#' @param box (`Box`) (Required).",
            "use <- function(box) box$@@collect"))
    expect_true("collect" %in% type_hint_labels(f))
})

test_that("a nested function uses its locally visible generator", {
    f <- type_hint_fixture(c(type_hint_box, "outer <- function() {",
            '  Box <- R6::R6Class("Nested", public = list(local = TRUE))',
            "  #' @param value (`Box`) A local box.", "  inner <- function(value) value$@@local", "}"))
    expect_identical(type_hint_labels(f), c("clone", "local"))
})

test_that("qualified and attached non-exported classes use their package member table", {
    model <- member_generic_index("")
    model$package <- "framefixture"
    model$classes$frame_type <- c("frame_type", "frame_object")
    model$members$frame_type <- c(group_by = NA_character_)
    for (type in c("frame_type", "framefixture::frame_type")) {
        f <- type_hint_fixture(c("library(framefixture)", paste0("#' @param data (`", type, "`) Rows."),
                "f <- function(data) data$@@group_by"))
        f$workspace$member_metadata <- MemberMetadataCache$new(4 * 1024^2)
        f$workspace$member_metadata$set("framefixture", member_index_freeze(model))
        expect_identical(type_hint_labels(f), "group_by")
    }
})

test_that("a package hint waits for its model only when its receiver is used", {
    f <- type_hint_fixture(c("#' @param data (`framefixture::frame_type`) Input.",
            "use <- function(data) data$@@group_by", "other <- list(ready = TRUE)", "other$ready"))
    previous <- lsp_settings$get("roxygen_type_hints")
    withr::defer(lsp_settings$set("roxygen_type_hints", previous))
    server <- list(member_task_manager = list(has = function(...) TRUE), get_workspace = function(...) f$workspace)
    f$document$requested_packages <- member_package_request(f$document$parse_data)
    expect_identical(f$document$requested_packages$namespace_packages, "framefixture")
    expect_true(member_metadata_waiting(server, f$uri, f$document, f$point, type_hints = TRUE))
    expect_false(member_metadata_waiting(server, f$uri, f$document, list(row = 3L, col = 6L), type_hints = TRUE))
    lsp_settings$set("roxygen_type_hints", FALSE)
    expect_length(member_package_request(f$document$parse_data)$namespace_packages, 0L)
    expect_false(member_metadata_waiting(server, f$uri, f$document, f$point, type_hints = TRUE))
})

test_that("the foundation only uses hints for member completion", {
    f <- type_hint_fixture(c(type_hint_box, "#' @return (`Box`) A box.",
            "restore <- function() opaque()", "box <- restore()", "box$@@collect()"))
    expect_true("collect" %in% type_hint_labels(f))
    field <- list(row = f$point$row, col = f$point$col + 2L)
    expect_null(member_definition_location(f$uri, f$workspace, f$document, field)$result)
    expect_null(prepare_rename_reply(1L, f$uri, f$workspace, f$document, field)$result)
    cursor <- member_cursor(f$document, f$point)
    expect_null(member_resolve_cursor(f$uri, f$workspace, f$document, f$point, cursor)$value$type)
    expect_true("collect" %in% type_hint_labels(f))
})
