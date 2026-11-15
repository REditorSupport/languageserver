inline_info_setting <- function(enabled) {
    previous <- lsp_settings$get("completion_label_details")
    withr::defer(lsp_settings$set("completion_label_details", previous), envir = parent.frame())
    lsp_settings$set("completion_label_details", enabled)
}

inline_info_fixture <- function(code, packages = character(), point = NULL) {
    content <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    uri <- "file:///inline-info.R"
    document <- Document$new(uri, language = "r", version = 1L, content = content)
    parsed <- parse_document(uri, content)
    parsed$version <- document$version
    document$update_parse_data(parsed)
    documents <- collections::dict()
    documents$set(uri, document)
    namespaces <- collections::dict()
    for (package in packages) namespaces$set(package, PackageNamespace$new(package))
    globals <- GlobalEnv$new(documents)
    workspace <- new.env(parent = baseenv())
    workspace$documents <- documents
    workspace$global_env <- globals
    workspace$namespaces <- namespaces
    workspace$member_metadata <- MemberMetadataCache$new(1024^2)
    workspace$loaded_packages <- workspace$startup_packages <- packages
    workspace$imported_objects <- collections::dict()
    workspace$get_parse_data <- function(...) document$parse_data
    workspace$get_namespace <- function(package, uri = NULL) {
        if (identical(package, WORKSPACE)) globals else namespaces$get(package, NULL)
    }
    workspace$get_signature <- function(name, package = NULL, exported_only = TRUE, uri = NULL) {
        ns <- if (is.null(package) || identical(package, WORKSPACE)) globals else namespaces$get(package, NULL)
        if (!is.null(ns)) ns$get_signature(name, exported_only = exported_only)
    }
    workspace$get_formals <- function(...) NULL
    workspace$get_documentation <- function(...) NULL
    workspace$get_help <- function(...) NULL
    workspace$guess_namespace <- function(...) NULL
    if (is.null(point)) point <- list(row = length(content) - 1L, col = nchar(tail(content, 1L)))
    list(uri = uri, document = document, workspace = workspace, point = point)
}

inline_info_reply <- function(fixture, support = TRUE) {
    completion_reply(1L, fixture$uri, fixture$workspace, fixture$document, fixture$point,
        list(completionItem = list(labelDetailsSupport = support, snippetSupport = TRUE)))$result
}

inline_info_item <- function(reply, name) {
    labels <- vapply(reply$items, `[[`, character(1L), "label")
    reply$items[[match(name, labels)]]
}

test_that("Inline completion info is hidden by default and requires client support", {
    expect_false(Settings$new()$get("completion_label_details"))
    inline_info_setting(FALSE)
    fixtures <- list(
        inline_info_fixture("value <- 3\nval"),
        inline_info_fixture("list(value = 3)$val"),
        inline_info_fixture("basen", packages = "base")
    )
    for (fixture in fixtures) {
        expect_true(all(vapply(inline_info_reply(fixture)$items,
                    function(item) is.null(item$labelDetails), logical(1L))))
    }
    inline_info_setting(TRUE)
    for (fixture in fixtures) {
        expect_true(all(vapply(inline_info_reply(fixture, support = FALSE)$items,
                    function(item) is.null(item$labelDetails), logical(1L))))
    }
})

test_that("Enabled inline tips agree for ordinary symbols and members", {
    inline_info_setting(TRUE)
    cases <- list(
        list(expression = "3", detail = " = 3", description = "known value"),
        list(expression = "1:3", detail = ": integer", description = "inferred"),
        list(expression = "function(argument = 1) NULL", detail = "(argument = 1)", description = "static")
    )
    for (case in cases) {
        symbol <- inline_info_fixture(paste0("value <- ", case$expression, "\nval"))
        member <- inline_info_fixture(paste0("box <- list(value = ", case$expression, ")\nbox$val"))
        symbol_item <- inline_info_item(inline_info_reply(symbol), "value")
        member_item <- inline_info_item(inline_info_reply(member), "value")
        expected <- list(detail = case$detail, description = case$description)
        expect_identical(symbol_item$labelDetails, expected, info = case$expression)
        expect_identical(member_item$labelDetails, expected, info = case$expression)
    }
})

test_that("Inline signatures respect local scope and formal masking", {
    inline_info_setting(TRUE)
    fixture <- inline_info_fixture(paste(c(
        "run <- function(old = 1) NULL",
        "outer <- function() {",
        "run <- function(current = 2) NULL",
        "ru",
        "}"
    ), collapse = "\n"), point = list(row = 3L, col = 2L))
    item <- inline_info_item(inline_info_reply(fixture), "run")
    expect_identical(item$labelDetails$detail, "(current = 2)")
    expect_identical(item$insertText, "run($0)")

    fixture <- inline_info_fixture("value <- 3\nouter <- function(value) {\nval\n}",
        point = list(row = 2L, col = 3L))
    item <- inline_info_item(inline_info_reply(fixture), "value")
    expect_false(grepl("known value| = 3", paste(unlist(item$labelDetails), collapse = " ")))
})

test_that("Cached package signatures appear inline without altering completions", {
    inline_info_setting(TRUE)
    fixture <- inline_info_fixture("utils::head", packages = c("base", "utils"))
    item <- inline_info_item(inline_info_reply(fixture), "head")
    expect_identical(item$labelDetails$detail, "(x, ...)")
    expect_identical(item$insertText, "head($0)")
    expect_match(item$detail, "head(x, ...)", fixed = TRUE)

    fixture <- inline_info_fixture("basename", packages = "base")
    item <- inline_info_item(inline_info_reply(fixture), "basename")
    expect_identical(item$labelDetails$detail, "(path)")
})

test_that("Hidden inline info remains hidden after resolve while selected tips remain", {
    inline_info_setting(FALSE)
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE))
    fixture <- inline_info_fixture("list(value = 3)$val")
    item <- inline_info_item(inline_info_reply(fixture), "value")
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item, capabilities)$result
    expect_null(resolved$labelDetails)
    expect_identical(resolved$detail, "3 [known value]")

    fixture <- inline_info_fixture("value <- 3\nval")
    item <- inline_info_item(inline_info_reply(fixture), "value")
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item, capabilities)$result
    expect_null(resolved$labelDetails)
    expect_match(resolved$detail, "3 [known value]", fixed = TRUE)

    fixture <- inline_info_fixture("basename", packages = "base")
    item <- inline_info_item(inline_info_reply(fixture), "basename")
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item, capabilities)$result
    expect_null(resolved$labelDetails)
    expect_match(resolved$detail, "basename(path)", fixed = TRUE)

    inline_info_setting(TRUE)
    fixture <- inline_info_fixture("value <- 3\nval")
    item <- inline_info_item(inline_info_reply(fixture), "value")
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item, capabilities)$result
    expect_identical(resolved$labelDetails, item$labelDetails)
})

test_that("Enabling inline info preserves pruning, ranking and inserted text", {
    previous_limit <- lsp_settings$get("max_completions")
    withr::defer(lsp_settings$set("max_completions", previous_limit))
    lsp_settings$set("max_completions", 5L)
    fixture <- inline_info_fixture(paste(c(
        sprintf("value_%02d <- %d", seq_len(20L), seq_len(20L)),
        "value_function <- function(argument = 1) NULL",
        "value_"
    ), collapse = "\n"))
    inline_info_setting(FALSE)
    hidden <- inline_info_reply(fixture)
    inline_info_setting(TRUE)
    enabled <- inline_info_reply(fixture)
    expect_length(enabled$items, 5L)
    expect_identical(enabled$isIncomplete, hidden$isIncomplete)
    unchanged_fields <- function(items) {
        lapply(items, function(item) {
            item$detail <- item$labelDetails <- NULL
            item$data$signature <- item$data$tip <- NULL
            item
        })
    }
    expect_identical(unchanged_fields(enabled$items), unchanged_fields(hidden$items))
})

test_that("The outline fast path follows the inline display setting", {
    inline_info_setting(FALSE)
    fixture <- inline_info_fixture("value <- 3\nval")
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE))
    ready <- testthat::with_mocked_bindings({
        expect_no_error(inline_info_reply(fixture))
        basic_name_completion_reply(1L, fixture$workspace, fixture$document, fixture$point, capabilities)
    }, completion_symbol_info = function(...) stop("Unnecessary inline enrichment"), .package = "languageserver")
    expect_true(all(vapply(ready$result$items, function(item) is.null(item$labelDetails), logical(1L))))
    inline_info_setting(TRUE)
    ready <- basic_name_completion_reply(1L, fixture$workspace, fixture$document, fixture$point, capabilities)
    expect_identical(inline_info_item(ready$result, "value")$labelDetails,
        list(detail = " = 3", description = "known value"))
})

test_that("Outline inline facts agree with accepted source facts when already known", {
    inline_info_setting(TRUE)
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE))
    codes <- c("value <- 3\nval", "run <- function(argument = 1) NULL\nru",
        "`run unusual` <- function(argument = 1) NULL\nru")
    for (code in codes) {
        fixture <- inline_info_fixture(code)
        expected <- inline_info_reply(fixture)$items
        fixture$document$parse_data <- NULL
        actual <- basic_name_completion_reply(1L, fixture$workspace, fixture$document,
            fixture$point, capabilities)$result$items
        name <- if (startsWith(code, "value")) "value" else if (startsWith(code, "`")) "run unusual" else "run"
        matching <- function(item) identical(completion_source_name(item$label), name)
        item <- Filter(matching, actual)[[1L]]
        full <- Filter(matching, expected)[[1L]]
        expect_identical(item$labelDetails, full$labelDetails, info = code)
    }
})

test_that("Outline tips do not guess conditional, future or masked values", {
    inline_info_setting(TRUE)
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE))
    cases <- list(
        list(code = "value <- 1\nif (flag) value <- 2\nval", point = NULL),
        list(code = "value <- 1\nval\nvalue <- 2", point = list(row = 1L, col = 3L)),
        list(code = "run <- function(old = 1) NULL\nouter <- function(run) {\nru\n}",
            point = list(row = 2L, col = 2L)),
        list(code = "run <- function(old = 1) NULL\nrun <- opaque()\nru", point = NULL),
        list(code = "run <- function(old = 1) NULL\nif (flag) run <- function(new = 2) NULL\nru", point = NULL),
        list(code = "outer <- function() {\nrun <- function(old = 1) NULL\nif (flag) run <- function(new = 2) NULL\nru\n}",
            point = list(row = 3L, col = 2L))
    )
    for (case in cases) {
        fixture <- inline_info_fixture(case$code, point = case$point)
        fixture$document$parse_data <- NULL
        items <- basic_name_completion_reply(1L, fixture$workspace, fixture$document,
            fixture$point, capabilities)$result$items
        expect_true(all(vapply(items, function(item) is.null(item$labelDetails), logical(1L))), info = case$code)
    }
})

test_that("Inline enrichment does not load an uncached package namespace", {
    inline_info_setting(TRUE)
    fixture <- inline_info_fixture("fixture_fun")
    fixture$workspace$get_namespace <- function(...) stop("Cold package namespace lookup")
    fixture$workspace$get_signature <- function(...) stop("Cold package signature lookup")
    items <- list(list(label = "fixture_fun", kind = CompletionItemKind$Function,
            detail = "{coldfixture}", sortText = "4-fixture_fun",
            data = list(type = "function", package = "coldfixture")))
    expect_identical(completion_symbol_info(fixture$uri, fixture$workspace, fixture$document,
            fixture$point, completion_token(fixture$document, fixture$point), items), items)
    fixture$workspace$namespaces$set("coldfixture", list(get_signature = function(...) stop("Reloaded namespace")))
    expect_identical(completion_symbol_info(fixture$uri, fixture$workspace, fixture$document,
            fixture$point, completion_token(fixture$document, fixture$point), items), items)
    item <- completion_context_data(items, fixture$uri, fixture$document, fixture$point)[[1L]]
    expect_null(completion_item_resolve_reply(2L, fixture$workspace, item,
            list(completionItem = list(labelDetailsSupport = TRUE)))$result$labelDetails)
})

test_that("Resolve does not guess a signature for a masked source function", {
    inline_info_setting(TRUE)
    fixture <- inline_info_fixture("run <- function(old = 1) NULL\nouter <- function(run) {\nru\n}",
        point = list(row = 2L, col = 2L))
    items <- list(list(label = "run", kind = CompletionItemKind$Function, detail = "[workspace]",
            data = list(type = "function", package = WORKSPACE)))
    item <- completion_context_data(items, fixture$uri, fixture$document, fixture$point)[[1L]]
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item,
        list(completionItem = list(labelDetailsSupport = TRUE)))$result
    expect_null(resolved$labelDetails)
    expect_identical(resolved$detail, "[workspace]")
})

test_that("Resolve preserves labels unless the client explicitly supports resolving them", {
    inline_info_setting(TRUE)
    expect_true(CompletionOptions$completionItem$labelDetailsSupport)
    fixture <- inline_info_fixture("value <- 3\nval")
    item <- inline_info_item(inline_info_reply(fixture), "value")
    capabilities <- list(completionItem = list(labelDetailsSupport = TRUE,
            resolveSupport = list(properties = c("documentation", "detail", "additionalTextEdits"))))
    late_labels <- list(detail = " = 999", description = "late")
    testthat::local_mocked_bindings(completion_symbol_info = function(uri, workspace, document, point, token, items, ...) {
        items[[1L]]$labelDetails <- late_labels
        items[[1L]]$detail <- "Selected-item details"
        items
    }, .package = "languageserver")
    resolved <- completion_item_resolve_reply(2L, fixture$workspace, item, capabilities)$result
    expect_identical(resolved$labelDetails, item$labelDetails)
    expect_identical(resolved$detail, "Selected-item details")
    plain <- item
    plain$labelDetails <- NULL
    expect_null(completion_item_resolve_reply(2L, fixture$workspace, plain, capabilities)$result$labelDetails)
    capabilities$completionItem$resolveSupport$properties <- "labelDetails"
    expect_identical(completion_item_resolve_reply(2L, fixture$workspace, item,
            capabilities)$result$labelDetails, late_labels)
})
