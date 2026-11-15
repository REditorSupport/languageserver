member_tip_fixture <- function(code, snapshots = list()) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///member-tips.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    document$parse_data <- parse_document(uri, content)
    metadata <- collections::dict()
    for (package in names(snapshots)) metadata$set(package, member_index_thaw(snapshots[[package]]))
    list(uri = uri, document = document,
        workspace = list(member_metadata = metadata, get_documentation = function(...) NULL),
        point = list(row = length(content) - 1L, col = nchar(tail(content, 1L))))
}

member_tip_items <- function(code, snapshots = list(), label_details_support = FALSE) {
    fixture <- member_tip_fixture(code, snapshots)
    member_completion(fixture$uri, fixture$workspace, fixture$document, fixture$point,
        TRUE, 200L, label_details_support = label_details_support)
}

test_that("Member tips show known scalar and short vector values", {
    cases <- list(
        "CompletionItemKind <- list(Function = 3)\nCompletionItemKind$" = "3 [known value]",
        "SemanticTokenTypes <- list(namespace = 0L)\nSemanticTokenTypes$" = "0L [known value]",
        "SemanticTokenModifiers <- list(readonly = 2L)\nSemanticTokenModifiers$" = "2L [known value]",
        "list(value = \"yes\")$" = "\"yes\" [known value]",
        "list(value = TRUE)$" = "TRUE [known value]",
        "list(value = NULL)$" = "NULL [known value]",
        "list(value = c(1L, 3L))$" = "c(1L, 3L) [known value]"
    )
    for (code in names(cases)) {
        item <- member_tip_items(code)[[1L]]
        expect_identical(item$detail, cases[[code]], info = code)
        expect_null(item$labelDetails, info = code)
        expect_null(item$documentation, info = code)
    }
})

test_that("Member tips distinguish inferred types and shapes from known values", {
    cases <- list(
        "list(value = 1:3)$" = "integer [inferred]",
        "x <- if (flag) list(value = 1L) else list(value = 2L)\nx$" = "integer [inferred]",
        "list(config = list(host = \"localhost\", port = 123L))$" = "list(host, port) [inferred]",
        "list(config = list(a = 1, b = 2, c = 3, d = 4))$" = "list(a, b, c, ...) [inferred]",
        "list(value = structure(1, class = \"Date\"))$" = "Date [inferred]"
    )
    for (code in names(cases)) {
        expect_identical(member_tip_items(code)[[1L]]$detail, cases[[code]], info = code)
    }
    item <- member_tip_items("list(value = opaque())$")[[1L]]
    expect_identical(item$detail, "[static member]")
})

test_that("Member previews remain short for long strings and collections", {
    codes <- c(
        paste0("list(value = \"", strrep("x", 10000L), "\")$"),
        paste0("list(value = c(", paste(seq_len(100L), collapse = ","), "))$"),
        paste0("list(value = list(`", strrep("x", 1000L), "` = 1))$")
    )
    for (code in codes) {
        detail <- member_tip_items(code)[[1L]]$detail
        expect_true(nchar(sub(" \\[.*\\]$", "", detail)) <= 80L, info = code)
        expect_match(detail, "...", fixed = TRUE)
        expect_false(grepl("10000|1000", detail))
    }
})

test_that("Completion label details are immediate and survive resolution", {
    fixture <- member_tip_fixture("list(value = 3)$")
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE))
    item <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        fixture$point, capabilities)$result$items[[1L]]
    expect_identical(item$label, "value")
    expect_identical(item$textEdit$newText, "value")
    expect_identical(item$filterText, "value")
    expect_identical(item$labelDetails, list(detail = " = 3", description = "known value"))
    expect_identical(completion_item_resolve_reply(2L, fixture$workspace, item,
            capabilities)$result$labelDetails, item$labelDetails)

    item <- member_tip_items("list(value = 1:3)$", label_details_support = TRUE)[[1L]]
    expect_identical(item$labelDetails, list(detail = ": integer", description = "inferred"))

    fixture <- member_tip_fixture("list(run = function(argument = 1) NULL)$")
    item <- completion_reply(1L, fixture$uri, fixture$workspace, fixture$document,
        fixture$point, capabilities)$result$items[[1L]]
    expect_identical(item$detail, "run(argument = 1)")
    expect_identical(item$textEdit$newText, "run")
    expect_identical(item$labelDetails, list(detail = "(argument = 1)", description = "static"))
    expect_identical(completion_item_resolve_reply(2L, fixture$workspace, item,
            capabilities)$result$labelDetails, item$labelDetails)
})

test_that("Package member tips use snapshot values without invoking active bindings", {
    scope <- new.env(parent = baseenv())
    scope$api <- new.env(parent = emptyenv())
    scope$api$value <- 42L
    calls <- 0L
    makeActiveBinding("pending", function() {
        calls <<- calls + 1L
        stop("Completion must not invoke this binding")
    }, scope$api)
    index <- member_package_index(member_namespace_input(scope, package = "tipfixture", exports = "api"))
    snapshot <- member_index_freeze(index)
    items <- member_tip_items("library(tipfixture)\napi$", list(tipfixture = snapshot))
    details <- stats::setNames(vapply(items, `[[`, character(1L), "detail"),
        vapply(items, `[[`, character(1L), "label"))
    expect_identical(details[["value"]], "42L [known value]")
    expect_identical(details[["pending"]], "[static member]")
    expect_identical(calls, 0L)
})

test_that("Possible member tips do not expose a conflicting candidate's value", {
    index <- member_generic_index("")
    index$package <- "tipfixture"
    index$exports <- "candidate"
    index$roots$candidate <- member_value(reason = "open_dispatch", possible_returns = list(
        member_value(type = "list", fields = list(value = member_literal(1L)), member_package = "tipfixture"),
        member_value(type = "list", fields = list(value = member_literal(2L)), member_package = "tipfixture")))
    items <- member_tip_items("library(tipfixture)\ncandidate$",
        list(tipfixture = member_index_freeze(index)), label_details_support = TRUE)
    expect_identical(vapply(items, `[[`, character(1L), "label"), "value")
    expect_identical(items[[1L]]$detail, "[possible member]")
    expect_false(grepl("known value", paste(unlist(items[[1L]]$labelDetails), collapse = " ")))

    index$roots$candidate$possible_returns[[2L]]$fields$value <- member_literal(1L)
    item <- member_tip_items("library(tipfixture)\ncandidate$",
        list(tipfixture = member_index_freeze(index)), label_details_support = TRUE)[[1L]]
    expect_identical(item$detail, "1L [possible receiver]")
    expect_identical(item$labelDetails, list(detail = " = 1L", description = "possible receiver"))
})

test_that("Data-frame tips do not claim underlying column values", {
    code <- paste(c(
        "`$.custom` <- function(x, name) 99L",
        "x <- structure(list(value = 1L), class = c(\"custom\", \"data.frame\"))",
        "x$"
    ), collapse = "\n")
    expect_identical(member_tip_items(code)[[1L]]$detail, "[static member]")
})

test_that("Member shape tips do not resolve deferred children", {
    fixture <- member_tip_fixture("list(config = list(host = 1, pending = 2))$")
    resolved <- member_resolve_cursor(fixture$uri, fixture$workspace, fixture$document,
        fixture$point, member_cursor(fixture$document, fixture$point))
    resolved$value$fields$config$fields$pending <- member_local_binding(quote(stop("Never infer")), list())
    testthat::local_mocked_bindings(member_resolve_cursor = function(...) resolved,
        member_infer = function(...) stop("Completion tips must not infer children"), .package = "languageserver")
    item <- member_completion(fixture$uri, fixture$workspace, fixture$document,
        fixture$point, FALSE, 200L)[[1L]]
    expect_identical(item$detail, "list(host, pending) [inferred]")
})
