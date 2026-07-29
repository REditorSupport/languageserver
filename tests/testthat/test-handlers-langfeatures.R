langfeature_handler_fixture <- function(content = "value <- 1") {
    fixture <- provider_fixture(content)
    self <- new.env(parent = baseenv())
    self$deliveries <- list()
    self$get_workspace <- function(...) fixture$workspace
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
        invisible(message)
    }
    self$rootPath <- tempdir()
    self$ClientCapabilities <- list(textDocument = list(
        completion = list(),
        documentSymbol = list(),
        codeLens = list()
    ))
    self$pending_replies <- collections::dict()
    self$pending_replies$set(fixture$uri, list(
        `textDocument/documentSymbol` = collections::queue(),
        `textDocument/codeLens` = collections::queue(),
        `textDocument/documentLink` = collections::queue(),
        `textDocument/documentColor` = collections::queue(),
        `textDocument/foldingRange` = collections::queue(),
        `textDocument/linkedEditingRange` = collections::queue(),
        `textDocument/inlineValue` = collections::queue(),
        `textDocument/inlayHint` = collections::queue(),
        `textDocument/semanticTokens/full` = collections::queue(),
        `textDocument/semanticTokens/full/delta` = collections::queue(),
        `textDocument/semanticTokens/range` = collections::queue()
    ))
    c(fixture, list(self = self))
}

test_that("language feature handlers deliver successful provider replies", {
    fixture <- langfeature_handler_fixture()
    point <- list(line = 0L, character = 0L)
    request_range <- list(start = point, end = point)
    params <- list(
        textDocument = list(uri = fixture$uri),
        position = point,
        positions = list(point),
        range = request_range,
        ranges = list(request_range, request_range),
        context = list(diagnostics = list()),
        color = list(red = 1, green = 0, blue = 0, alpha = 1),
        options = list(tabSize = 2L, insertSpaces = TRUE),
        ch = ")",
        newName = "renamed",
        previousResultId = "previous"
    )
    cases <- list(
        list(text_document_completion, "completion_reply"),
        list(text_document_hover, "hover_reply"),
        list(text_document_signature_help, "signature_reply"),
        list(text_document_definition, "definition_reply"),
        list(text_document_references, "references_reply"),
        list(text_document_document_highlight, "document_highlight_reply"),
        list(text_document_document_symbol, "document_symbol_reply"),
        list(text_document_code_action, "document_code_action_reply"),
        list(text_document_code_lens, "code_lens_reply"),
        list(text_document_document_link, "document_link_reply"),
        list(text_document_document_color, "document_color_reply"),
        list(text_document_color_presentation, "color_presentation_reply"),
        list(text_document_formatting, "formatting_reply"),
        list(text_document_range_formatting, "range_formatting_reply"),
        list(text_document_ranges_formatting, "ranges_formatting_reply"),
        list(text_document_on_type_formatting, "on_type_formatting_reply"),
        list(text_document_rename, "rename_reply"),
        list(text_document_prepare_rename, "prepare_rename_reply"),
        list(text_document_folding_range, "document_folding_range_reply"),
        list(text_document_selection_range, "selection_range_reply"),
        list(text_document_prepare_call_hierarchy, "prepare_call_hierarchy_reply"),
        list(text_document_prepare_type_hierarchy, "prepare_type_hierarchy_reply"),
        list(text_document_linked_editing_range, "linked_editing_range_reply"),
        list(text_document_inline_value, "inline_value_reply"),
        list(text_document_inlay_hint, "inlay_hint_reply"),
        list(text_document_semantic_tokens_full, "semantic_tokens_full_reply"),
        list(text_document_semantic_tokens_delta, "semantic_tokens_delta_reply"),
        list(text_document_semantic_tokens_range, "semantic_tokens_range_reply")
    )

    for (case in cases) {
        handler <- case[[1L]]
        stub(handler, case[[2L]], function(id, ...) {
            Response$new(id = id, result = list(provider = case[[2L]]))
        })
        before <- length(fixture$self$deliveries)
        handler(fixture$self, before + 1L, params)
        expect_length(fixture$self$deliveries, before + 1L)
        expect_null(fixture$self$deliveries[[before + 1L]]$error)
    }
})

test_that("language feature handlers return null for unknown documents", {
    fixture <- langfeature_handler_fixture()
    fixture$workspace$documents <- list(get = function(...) NULL)
    point <- list(line = 0L, character = 0L)
    request_range <- list(start = point, end = point)
    params <- list(
        textDocument = list(uri = fixture$uri),
        position = point,
        positions = list(point),
        range = request_range,
        ranges = list(request_range),
        context = list(diagnostics = list()),
        color = list(red = 1, green = 0, blue = 0, alpha = 1),
        options = list(tabSize = 2L, insertSpaces = TRUE),
        ch = ")",
        newName = "renamed",
        previousResultId = "previous"
    )
    handlers <- list(
        text_document_completion,
        text_document_hover,
        text_document_signature_help,
        text_document_definition,
        text_document_references,
        text_document_document_highlight,
        text_document_document_symbol,
        text_document_code_action,
        text_document_code_lens,
        text_document_document_link,
        text_document_document_color,
        text_document_color_presentation,
        text_document_formatting,
        text_document_range_formatting,
        text_document_ranges_formatting,
        text_document_on_type_formatting,
        text_document_rename,
        text_document_prepare_rename,
        text_document_folding_range,
        text_document_selection_range,
        text_document_prepare_call_hierarchy,
        text_document_prepare_type_hierarchy,
        text_document_linked_editing_range,
        text_document_inline_value,
        text_document_inlay_hint,
        text_document_semantic_tokens_full,
        text_document_semantic_tokens_delta,
        text_document_semantic_tokens_range
    )

    for (handler in handlers) {
        before <- length(fixture$self$deliveries)
        handler(fixture$self, before + 1L, params)
        expect_length(fixture$self$deliveries, before + 1L)
        expect_null(fixture$self$deliveries[[before + 1L]]$result)
    }
})

test_that("latest queued replies supersede only the same document version", {
    fixture <- langfeature_handler_fixture()
    queue <- fixture$self$pending_replies$get(fixture$uri)[["textDocument/inlayHint"]]
    queue$push(list(id = 1L, version = 3L))
    queue$push(list(id = 2L, version = 4L))

    enqueue_latest_reply(
        fixture$self,
        fixture$uri,
        "textDocument/inlayHint",
        list(id = 3L, version = 3L)
    )

    expect_length(fixture$self$deliveries, 1L)
    expect_equal(fixture$self$deliveries[[1L]]$id, 1L)
    expect_equal(queue$size(), 2L)
    expect_equal(queue$pop()$id, 2L)
    expect_equal(queue$pop()$id, 3L)
})

test_that("document link resolve reports provider errors to the user", {
    fixture <- langfeature_handler_fixture()
    handler <- document_link_resolve
    stub(handler, "document_link_resolve_reply", function(id, ...) {
        ResponseErrorMessage$new(id, "InternalError", "cannot resolve link")
    })

    handler(fixture$self, 1L, list(data = list(uri = fixture$uri)))

    expect_length(fixture$self$deliveries, 2L)
    expect_equal(fixture$self$deliveries[[1L]]$error$message, "cannot resolve link")
    expect_equal(fixture$self$deliveries[[2L]]$method, "window/showMessage")
    expect_equal(
        fixture$self$deliveries[[2L]]$params$message,
        "cannot resolve link"
    )
})

test_that("initialization handles trace and multiple workspace folders", {
    old_trace <- lsp_settings$get("trace")
    old_log_file <- lsp_settings$get("log_file")
    withr::defer({
        lsp_settings$set("trace", old_trace)
        lsp_settings$set("log_file", old_log_file)
    })
    lsp_settings$set("log_file", withr::local_tempfile())
    self <- new.env(parent = baseenv())
    self$workspaces <- collections::dict()
    self$added <- character()
    self$deliveries <- list()
    self$add_workspace <- function(uri) {
        self$added <- c(self$added, uri)
        self$workspaces$set(uri, uri)
    }
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    root <- path_to_uri(tempdir())
    second <- path_to_uri(withr::local_tempdir())

    on_initialize(self, 7L, list(
        trace = "messages",
        processId = 42L,
        rootUri = root,
        workspaceFolders = list(
            list(uri = root, name = "root"),
            list(uri = second, name = "second")
        ),
        initializationOptions = list(test = TRUE),
        capabilities = list()
    ))

    expect_true(lsp_settings$get("trace"))
    expect_equal(self$processId, 42L)
    expect_equal(self$added, c(root, second))
    expect_length(self$deliveries, 1L)
    expect_false(is.null(self$deliveries[[1L]]$result$capabilities))
})

test_that("exit, cancellation, and trace notifications update server state", {
    old_trace <- lsp_settings$get("trace")
    withr::defer(lsp_settings$set("trace", old_trace))
    self <- new.env(parent = baseenv())
    self$exit_flag <- FALSE
    self$deliveries <- list()
    self$deliver <- function(message) {
        self$deliveries[[length(self$deliveries) + 1L]] <- message
    }
    self$pending_replies <- collections::dict()
    first <- collections::queue()
    second <- collections::queue()
    first$push(list(id = 11L))
    first$push(list(id = 12L))
    second$push(list(id = "11"))
    self$pending_replies$set("file:///one.R", list(first, second))

    cancel_request(self, list(id = 11L))
    expect_equal(
        vapply(self$deliveries, function(x) as.character(x$id), character(1L)),
        c("11", "11")
    )
    expect_equal(first$size(), 1L)
    expect_equal(first$peek()$id, 12L)
    expect_equal(second$size(), 0L)

    on_exit(self, NULL)
    expect_true(self$exit_flag)
    protocol_set_trace(self, list(value = "off"))
    expect_false(lsp_settings$get("trace"))
    protocol_set_trace(self, list(value = "verbose"))
    expect_true(lsp_settings$get("trace"))
})
