formatting_client_capabilities <- function(formatting = TRUE, range = TRUE, on_type = TRUE) {
    list(textDocument = list(
        formatting = list(dynamicRegistration = formatting),
        rangeFormatting = list(dynamicRegistration = range, rangesSupport = TRUE),
        onTypeFormatting = list(dynamicRegistration = on_type)))
}

local_formatting_capability_settings <- function(.env = parent.frame()) {
    keys <- c("formatting_backend", "diagnostics", "backend_options", "server_capabilities")
    previous <- stats::setNames(lapply(keys, lsp_settings$get), keys)
    withr::defer(for (key in keys) lsp_settings$set(key, previous[[key]]), envir = .env)
    withr::local_options(list(languageserver.formatting_backend = NULL), .local_envir = .env)
    lsp_settings$set("formatting_backend", "styler")
    lsp_settings$set("diagnostics", FALSE)
    lsp_settings$set("backend_options", list())
    lsp_settings$set("server_capabilities", list())
}

formatting_registration_fixture <- function() {
    self <- new.env(parent = baseenv())
    self$ClientCapabilities <- formatting_client_capabilities()
    self$workspaces <- collections::dict()
    self$messages <- self$callbacks <- list()
    self$request <- function(method, params) list(method = method, params = params)
    self$deliver <- function(message, callback = NULL) {
        index <- length(self$messages) + 1L
        self$messages[[index]] <- message
        self$callbacks[index] <- list(callback)
    }
    self$acknowledge <- function(index, error = NULL) self$callbacks[[index]](self, NULL, error)
    self
}

test_that("dynamic formatting is not also advertised statically, including partial client support", {
    local_formatting_capability_settings()
    capabilities <- update_server_capabilities(ServerCapabilities, formatting_client_capabilities())
    expect_false(capabilities$documentFormattingProvider)
    expect_false(capabilities$documentRangeFormattingProvider)
    expect_null(capabilities$documentOnTypeFormattingProvider)
    partial <- formatting_client_capabilities(range = FALSE, on_type = FALSE)
    capabilities <- update_server_capabilities(ServerCapabilities, partial)
    expect_false(capabilities$documentFormattingProvider)
    expect_true(capabilities$documentRangeFormattingProvider$rangesSupport)
    expect_equal(capabilities$documentOnTypeFormattingProvider, DocumentOnTypeFormattingOptions)
    registrations <- formatting_registrations(partial)
    expect_equal(vapply(registrations, `[[`, character(1L), "method"),
        c(documentFormattingProvider = "textDocument/formatting"))
    lsp_settings$set("server_capabilities", list(documentFormattingProvider = FALSE))
    expect_length(formatting_registrations(partial), 0L)
})

test_that("late workspace settings replace styler registrations with Air modes", {
    local_formatting_capability_settings()
    self <- formatting_registration_fixture()
    sync_formatting_capabilities(self)
    expect_length(self$messages, 0L)
    sync_formatting_capabilities(self, initialized = TRUE)
    expect_length(self$messages[[1L]]$params$registrations, 3L)
    # VS Code sends r.lsp settings after initialization, possibly before the ack.
    workspace_did_change_configuration(self, list(settings = list(r = list(lsp = list(formatting_backend = "air")))))
    expect_length(self$messages, 1L)
    self$acknowledge(1L)
    expect_equal(self$messages[[2L]]$method, "client/unregisterCapability")
    expect_setequal(vapply(self$messages[[2L]]$params$unregisterations, `[[`, character(1L), "method"),
        c("textDocument/rangeFormatting", "textDocument/onTypeFormatting"))
    self$acknowledge(2L)
    expect_equal(self$messages[[3L]]$params$registrations[[1L]]$registerOptions$firstTriggerCharacter, "\n")
    expect_null(self$messages[[3L]]$params$registrations[[1L]]$registerOptions$moreTriggerCharacter)
    self$acknowledge(3L)
    expect_setequal(names(self$formatting_capability_state$registered),
        c("documentFormattingProvider", "documentOnTypeFormattingProvider"))
    expect_false(self$formatting_capability_state$pending)
})

test_that("backend switches during registration updates converge to the latest modes", {
    local_formatting_capability_settings()
    self <- formatting_registration_fixture()
    sync_formatting_capabilities(self, initialized = TRUE)
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "air")))
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "styler")))
    self$acknowledge(1L)
    expect_length(self$messages, 1L)
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "air")))
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "styler")))
    expect_length(self$messages, 2L)
    self$acknowledge(2L)
    expect_length(self$messages[[3L]]$params$registrations, 2L)
    self$acknowledge(3L)
    registered <- self$formatting_capability_state$registered
    expect_true(registered$documentRangeFormattingProvider$registerOptions$rangesSupport)
    expect_equal(registered$documentOnTypeFormattingProvider$registerOptions$moreTriggerCharacter,
        DocumentOnTypeFormattingOptions$moreTriggerCharacter)
    expect_false(self$formatting_capability_state$pending)
})

test_that("registration errors preserve state and do not retry indefinitely", {
    local_formatting_capability_settings()
    self <- formatting_registration_fixture()
    sync_formatting_capabilities(self, initialized = TRUE)
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "air")))
    self$acknowledge(1L, list(message = "Rejected"))
    expect_length(self$messages, 2L)
    expect_length(self$messages[[2L]]$params$registrations, 2L)
    self$acknowledge(2L, list(message = "Rejected"))
    expect_length(self$messages, 2L)
    expect_length(self$formatting_capability_state$registered, 0L)
    expect_false(self$formatting_capability_state$pending)
    workspace_did_change_configuration(self, list(settings = list(formatting_backend = "styler")))
    self$acknowledge(3L)
    expect_length(self$formatting_capability_state$registered, 3L)
})

test_that("LSP formatting registration follows VS Code workspace settings without restarting", {
    skip_on_cran()
    local_formatting_capability_settings()
    client <- language_client(working_dir = withr::local_tempdir(), capabilities = formatting_client_capabilities())
    expect_false(client$ServerCapabilities$documentFormattingProvider)
    expect_false(client$ServerCapabilities$documentRangeFormattingProvider)
    expect_null(client$ServerCapabilities$documentOnTypeFormattingProvider)
    state <- new.env(parent = emptyenv())
    state$registered <- list()
    client$request_handlers[["client/registerCapability"]] <- function(self, id, params) {
        for (registration in params$registrations) state$registered[[registration$method]] <- registration$registerOptions
        self$deliver(Response$new(id, NULL))
    }
    client$request_handlers[["client/unregisterCapability"]] <- function(self, id, params) {
        for (registration in params$unregisterations) state$registered[[registration$method]] <- NULL
        self$deliver(Response$new(id, NULL))
    }
    wait <- function(done) {
        deadline <- Sys.time() + 60
        while (!done() && Sys.time() < deadline) {
            data <- client$fetch(blocking = TRUE, timeout = 1)
            if (!is.null(data)) client$handle_raw(data)
        }
        expect_true(done())
    }
    notify(client, "workspace/didChangeConfiguration", list(settings = list(r = list(lsp = list(formatting_backend = "air")))))
    wait(function() {
        !is.null(state$registered[["textDocument/formatting"]]) &&
            !is.null(state$registered[["textDocument/onTypeFormatting"]]) &&
            is.null(state$registered[["textDocument/rangeFormatting"]]) &&
            is.null(state$registered[["textDocument/onTypeFormatting"]]$moreTriggerCharacter)
    })
    expect_equal(state$registered[["textDocument/onTypeFormatting"]]$firstTriggerCharacter, "\n")
    notify(client, "workspace/didChangeConfiguration", list(settings = list(r = list(lsp = list(formatting_backend = "styler")))))
    wait(function() {
        isTRUE(state$registered[["textDocument/rangeFormatting"]]$rangesSupport) &&
            ")" %in% state$registered[["textDocument/onTypeFormatting"]]$moreTriggerCharacter
    })
})
