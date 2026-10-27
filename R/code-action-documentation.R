refactoring_documentation_command <- "languageserver.showRefactoringDocumentation"
refactoring_documentation_uri <- "https://github.com/REditorSupport/languageserver/blob/master/README.md#refactoring"

code_action_documentation_supported <- function(client_capabilities) {
    isTRUE(client_capabilities$textDocument$codeAction$documentationSupport) &&
        isTRUE(client_capabilities$window$showDocument$support)
}

add_code_action_documentation <- function(server_capabilities, client_capabilities,
    command = refactoring_documentation_command) {
    if (!code_action_documentation_supported(client_capabilities) ||
            isFALSE(server_capabilities$executeCommandProvider)) return(server_capabilities)
    options <- server_capabilities$codeActionProvider
    if (isTRUE(options)) options <- CodeActionOptions
    if (!is.list(options) || !is.null(options$documentation)) return(server_capabilities)

    kinds <- unlist(options$codeActionKinds, use.names = FALSE)
    documented <- intersect(c("refactor.extract", "refactor.inline"), kinds)
    if (!length(documented)) return(server_capabilities)
    options$documentation <- lapply(documented, function(kind) {
        list(kind = kind, command = list(
            title = "Learn about R refactorings",
            command = command
        ))
    })
    server_capabilities$codeActionProvider <- options
    command_options <- server_capabilities$executeCommandProvider
    if (is.null(command_options)) command_options <- list()
    command_options$commands <- as.list(unique(c(
        unlist(command_options$commands, use.names = FALSE), command
    )))
    server_capabilities$executeCommandProvider <- command_options
    server_capabilities
}

configure_code_action_documentation <- function(self, server_capabilities) {
    # Editor commands are global, while vscode-R can run several R clients.
    command <- paste0(refactoring_documentation_command, ".", Sys.getpid(), ".",
        basename(tempfile("refactor-")))
    server_capabilities <- add_code_action_documentation(
        server_capabilities, self$ClientCapabilities, command)
    command_options <- server_capabilities$executeCommandProvider
    commands <- if (is.list(command_options)) command_options$commands
    self$documentation_command <- if (command %in% unlist(commands, use.names = FALSE)) command
    server_capabilities
}

#' `workspace/executeCommand` request handler for refactoring documentation
#' @noRd
workspace_execute_command <- function(self, id, params) {
    if (!is.list(params) || !is.character(params$command) || length(params$command) != 1L ||
            is.na(params$command) ||
            length(params$arguments)) {
        return(self$deliver(ResponseErrorMessage$new(
            id, "InvalidParams", "Unknown command or unexpected arguments"
        )))
    }
    if (!code_action_documentation_supported(self$ClientCapabilities) ||
            is.null(self$documentation_command) || !self$documentation_command %in%
            unlist(self$ServerCapabilities$executeCommandProvider$commands, use.names = FALSE)) {
        return(self$deliver(ResponseErrorMessage$new(
            id, "RequestFailed", "Refactoring documentation is not supported by this client"
        )))
    }
    if (!identical(params$command, self$documentation_command)) {
        return(self$deliver(ResponseErrorMessage$new(id, "InvalidParams", "Unknown command")))
    }
    self$deliver(self$request("window/showDocument", list(
        uri = refactoring_documentation_uri, external = TRUE
    )), callback = function(self, result, error = NULL) {
        if (!is.null(error) || !is.list(result) || !isTRUE(result$success)) {
            self$deliver(ResponseErrorMessage$new(
                id, "RequestFailed", "The client could not open refactoring documentation"
            ))
        } else {
            self$deliver(Response$new(id, result = NULL))
        }
    })
}
