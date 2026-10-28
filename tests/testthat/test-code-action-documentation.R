documentation_client_capabilities <- function(documentation = TRUE, show_document = TRUE) {
    list(textDocument = list(codeAction = list(documentationSupport = documentation)),
        window = list(showDocument = list(support = show_document)))
}

test_that("code action documentation requires a client that can open it", {
    for (capabilities in list(list(), documentation_client_capabilities(FALSE),
            documentation_client_capabilities(show_document = FALSE))) {
        result <- add_code_action_documentation(ServerCapabilities, capabilities)
        expect_null(result$codeActionProvider$documentation)
        expect_null(result$executeCommandProvider)
    }
    capabilities <- documentation_client_capabilities()
    result <- add_code_action_documentation(ServerCapabilities, capabilities)
    expect_equal(vapply(result$codeActionProvider$documentation, `[[`, character(1L), "kind"),
        c("refactor.extract", "refactor.inline"))
    expect_equal(result$executeCommandProvider$commands, list(refactoring_documentation_command))
    expect_identical(add_code_action_documentation(result, capabilities), result)

    disabled <- ServerCapabilities
    disabled$codeActionProvider <- FALSE
    expect_identical(add_code_action_documentation(disabled, capabilities), disabled)
    disabled <- ServerCapabilities
    disabled$executeCommandProvider <- FALSE
    expect_identical(add_code_action_documentation(disabled, capabilities), disabled)
})

documentation_command_fixture <- function() {
    self <- new.env(parent = baseenv())
    self$ClientCapabilities <- documentation_client_capabilities()
    self$ServerCapabilities <- add_code_action_documentation(ServerCapabilities, self$ClientCapabilities)
    self$documentation_command <- refactoring_documentation_command
    self$messages <- self$callbacks <- list()
    self$request <- function(method, params) Request$new(99L, method, params)
    self$deliver <- function(message, callback = NULL) {
        i <- length(self$messages) + 1L
        self$messages[[i]] <- message
        self$callbacks[i] <- list(callback)
    }
    self
}

test_that("documentation command opens only the documented URL and reports client failures", {
    self <- documentation_command_fixture()
    workspace_execute_command(self, 1L, list(command = refactoring_documentation_command))
    expect_equal(self$messages[[1L]]$method, "window/showDocument")
    expect_equal(self$messages[[1L]]$params, list(uri = refactoring_documentation_uri, external = TRUE))
    self$callbacks[[1L]](self, list(success = TRUE))
    expect_equal(self$messages[[2L]]$id, 1L)
    expect_null(self$messages[[2L]]$error)

    for (response in list(list(success = FALSE), NULL)) {
        self <- documentation_command_fixture()
        workspace_execute_command(self, 1L, list(command = refactoring_documentation_command))
        self$callbacks[[1L]](self, response)
        expect_equal(self$messages[[2L]]$error$code, ErrorCodes$RequestFailed)
    }
    self <- documentation_command_fixture()
    workspace_execute_command(self, 1L, list(command = refactoring_documentation_command))
    self$callbacks[[1L]](self, NULL, list(message = "client error"))
    expect_equal(self$messages[[2L]]$error$code, ErrorCodes$RequestFailed)
})

test_that("documentation command rejects unexpected commands, arguments, and unsupported clients", {
    for (params in list(list(command = "other"), list(), "invalid",
            list(command = refactoring_documentation_command, arguments = list("file:///other")))) {
        self <- documentation_command_fixture()
        workspace_execute_command(self, 1L, params)
        expect_length(self$messages, 1L)
        expect_equal(self$messages[[1L]]$error$code, ErrorCodes$InvalidParams)
    }
    self <- documentation_command_fixture()
    self$ClientCapabilities <- list()
    workspace_execute_command(self, 1L, list(command = refactoring_documentation_command))
    expect_equal(self$messages[[1L]]$error$code, ErrorCodes$RequestFailed)
})

test_that("code action documentation is advertised and opened through LSP", {
    skip_on_cran()
    client <- language_client(working_dir = withr::local_tempdir(),
        capabilities = documentation_client_capabilities())
    docs <- client$ServerCapabilities$codeActionProvider$documentation
    expect_length(docs, 2L)
    opened <- NULL
    client$request_handlers[["window/showDocument"]] <- function(self, id, params) {
        opened <<- params
        self$deliver(Response$new(id, list(success = TRUE)))
    }
    result <- respond(client, "workspace/executeCommand", docs[[1L]]$command, retry = FALSE)
    expect_null(result)
    expect_equal(opened, list(uri = refactoring_documentation_uri, external = TRUE))
})

documentation_initialize_fixture <- function() {
    self <- new.env(parent = baseenv())
    self$workspaces <- collections::dict()
    self$add_workspace <- function(uri) self$workspaces$set(uri, list())
    self$deliver <- function(message) self$response <- message
    self
}

test_that("initialization applies documentation overrides before negotiation", {
    withr::local_options(list(languageserver.server_capabilities = NULL))
    old <- lsp_settings$get("server_capabilities")
    withr::defer(lsp_settings$set("server_capabilities", old))
    for (override in list(list(executeCommandProvider = FALSE), list(codeActionProvider = FALSE),
            list(codeActionProvider = list(codeActionKinds = list("quickfix"))))) {
        lsp_settings$set("server_capabilities", override)
        self <- documentation_initialize_fixture()
        on_initialize(self, 1L, list(rootUri = "file:///documentation-test",
                capabilities = documentation_client_capabilities()))
        options <- self$ServerCapabilities$codeActionProvider
        expect_null(if (is.list(options)) options$documentation)
        expect_null(self$documentation_command)
    }
    options <- CodeActionOptions
    options$documentation <- list(list(kind = "refactor.extract", command = list(
        title = "Custom documentation", command = "client.openHelp"
    )))
    lsp_settings$set("server_capabilities", list(codeActionProvider = options))
    self <- documentation_initialize_fixture()
    on_initialize(self, 1L, list(rootUri = "file:///documentation-test",
            capabilities = documentation_client_capabilities()))
    expect_equal(self$ServerCapabilities$codeActionProvider$documentation, options$documentation)
    expect_null(self$documentation_command)
})

test_that("documentation commands belong to individual R clients", {
    withr::local_options(list(languageserver.server_capabilities = NULL))
    old <- lsp_settings$get("server_capabilities")
    withr::defer(lsp_settings$set("server_capabilities", old))
    lsp_settings$set("server_capabilities", list())
    clients <- replicate(2L, documentation_initialize_fixture(), simplify = FALSE)
    for (client in clients) on_initialize(client, 1L, list(rootUri = "file:///documentation-test",
            capabilities = documentation_client_capabilities()))
    expect_false(identical(clients[[1L]]$documentation_command, clients[[2L]]$documentation_command))
    for (client in clients) {
        expect_equal(client$ServerCapabilities$executeCommandProvider$commands,
            list(client$documentation_command))
    }
    workspace_execute_command(clients[[1L]], 2L, list(command = clients[[2L]]$documentation_command))
    expect_equal(clients[[1L]]$response$error$code, ErrorCodes$InvalidParams)
})
