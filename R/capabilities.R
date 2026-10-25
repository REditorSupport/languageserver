SaveOptions <- list(
    includeText = TRUE
)

TextDocumentSyncKind <- list(
    None = 0,
    Full = 1,
    Incremental = 2
)

TextDocumentSyncOptions <- list(
    openClose = TRUE,
    change = TextDocumentSyncKind$Incremental,
    willSave = FALSE,
    willSaveWaitUntil = FALSE,
    save = SaveOptions
)

CompletionOptions <- list(
    resolveProvider = TRUE,
    triggerCharacters = list(".", ":", "$", "@")
)

SignatureHelpOptions <- list(
    triggerCharacters = list("(", ",")
)

CodeLensOptions <- list(
    resolveProvider = TRUE
)

InlayHintOptions <- list(
    resolveProvider = TRUE
)

DocumentRangeFormattingOptions <- list(
    rangesSupport = TRUE
)

DocumentOnTypeFormattingOptions <- list(
    firstTriggerCharacter = "\n",
    moreTriggerCharacter = list(")", "]", "}")
)

DocumentLinkOptions <- list(
    resolveProvider = TRUE
)

CodeActionOptions <- list(
    codeActionKinds = list(
        "quickfix", "refactor.extract", "refactor.inline", "source.fixAll")
)

RenameOptions <- list(
    prepareProvider = TRUE
)

ExecuteCommandOptions <- list(
    commands = NULL
)

SemanticTokensOptions <- list(
    legend = list(
        tokenTypes = c(
            "namespace", "type", "class", "enum", "interface", "struct",
            "typeParameter", "parameter", "variable", "property", "enumMember",
            "event", "function", "method", "macro", "keyword", "modifier",
            "comment", "string", "number", "regexp", "operator", "decorator",
            "label"
        ),
        tokenModifiers = c(
            "declaration", "definition", "readonly", "static", "deprecated",
            "abstract", "async", "modification", "documentation", "defaultLibrary"
        )
    ),
    full = list(delta = TRUE),
    range = TRUE
)

ServerCapabilities <- list(
    positionEncoding = "utf-16",
    textDocumentSync = TextDocumentSyncOptions,
    hoverProvider = TRUE,
    completionProvider = CompletionOptions,
    signatureHelpProvider = SignatureHelpOptions,
    # typeDefinitionProvider = FALSE,
    # implementationProvider = FALSE,
    definitionProvider = TRUE,
    referencesProvider = TRUE,
    documentHighlightProvider = TRUE,
    documentSymbolProvider = TRUE,
    workspaceSymbolProvider = TRUE,
    codeActionProvider = CodeActionOptions,
    codeLensProvider = CodeLensOptions,
    documentFormattingProvider = TRUE,
    documentRangeFormattingProvider = DocumentRangeFormattingOptions,
    documentOnTypeFormattingProvider = DocumentOnTypeFormattingOptions,
    renameProvider = TRUE,
    documentLinkProvider = DocumentLinkOptions,
    colorProvider = TRUE,
    foldingRangeProvider = TRUE,
    selectionRangeProvider = TRUE,
    callHierarchyProvider = TRUE,
    typeHierarchyProvider = TRUE,
    semanticTokensProvider = SemanticTokensOptions,
    linkedEditingRangeProvider = TRUE,
    inlineValueProvider = TRUE,
    inlayHintProvider = InlayHintOptions,
    # monikerProvider = FALSE,
    # executeCommandProvider = ExecuteCommandOptions,
    workspace = list(
        workspaceFolders = list(
            supported = TRUE,
            changeNotifications = TRUE
        )
    )
)

formatting_capability_methods <- c(
    documentFormattingProvider = "formatting",
    documentRangeFormattingProvider = "rangeFormatting",
    documentOnTypeFormattingProvider = "onTypeFormatting"
)

formatting_capabilities <- function() {
    formatter <- tryCatch(backend_descriptor("formatting"), error = function(e) NULL)
    modes <- formatter$modes
    capabilities <- list(documentFormattingProvider = "document" %in% modes)
    capabilities$documentRangeFormattingProvider <- if ("range" %in% modes) {
        list(rangesSupport = "ranges" %in% modes)
    } else {
        FALSE
    }
    capabilities$documentOnTypeFormattingProvider <- if ("on_type" %in% modes) {
        DocumentOnTypeFormattingOptions
    } else if (!is.null(formatter) && identical(formatter$id, "air")) {
        list(firstTriggerCharacter = "\n")
    } else {
        NULL
    }
    capabilities <- merge_list(capabilities, lsp_settings$get("server_capabilities"))
    capabilities[intersect(names(formatting_capability_methods), names(capabilities))]
}

disable_dynamic_formatting_capabilities <- function(server_capabilities, client_capabilities) {
    for (field in names(formatting_capability_methods)) {
        method <- formatting_capability_methods[[field]]
        if (!isTRUE(client_capabilities$textDocument[[method]]$dynamicRegistration)) next
        server_capabilities[[field]] <- if (field == "documentOnTypeFormattingProvider") NULL else FALSE
    }
    server_capabilities
}

update_server_capabilities <- function(server_capabilities, client_capabilities) {
    capabilities <- formatting_capabilities()
    for (field in names(formatting_capability_methods)) server_capabilities[[field]] <- capabilities[[field]]
    server_capabilities <- disable_dynamic_formatting_capabilities(server_capabilities, client_capabilities)
    # RenameOptions may only be specified if the client states that
    # it supports prepareSupport in its initial initialize request
    if (isTRUE(server_capabilities$renameProvider) &&
            isTRUE(client_capabilities$textDocument$rename$prepareSupport)) {
        server_capabilities$renameProvider <- RenameOptions
    }

    server_capabilities
}

formatting_registrations <- function(client_capabilities) {
    capabilities <- formatting_capabilities()
    registrations <- list()
    for (field in names(formatting_capability_methods)) {
        method <- formatting_capability_methods[[field]]
        value <- capabilities[[field]]
        if (!isTRUE(client_capabilities$textDocument[[method]]$dynamicRegistration) ||
                is.null(value) || identical(value, FALSE)) next
        registrations[[field]] <- list(id = paste0("languageserver-", method),
            method = paste0("textDocument/", method),
            registerOptions = c(list(documentSelector = NULL), if (is.list(value)) value))
    }
    registrations
}

# Serialize registration changes: settings can change before the client replies.
sync_formatting_capabilities <- function(self, initialized = FALSE) {
    state <- self$formatting_capability_state
    if (is.null(state)) {
        if (!initialized) return(invisible(NULL))
        state <- new.env(parent = emptyenv())
        state$registered <- list()
        state$pending <- FALSE
        self$formatting_capability_state <- state
    }
    if (state$pending || isTRUE(self$exit_flag)) return(invisible(NULL))
    desired <- formatting_registrations(self$ClientCapabilities)
    removed <- state$registered[vapply(names(state$registered), function(field) {
        !identical(state$registered[[field]], desired[[field]])
    }, logical(1L))]
    added <- desired[setdiff(names(desired), names(state$registered))]
    if (!length(removed) && !length(added)) return(invisible(NULL))
    state$pending <- TRUE
    if (length(removed)) {
        method <- "client/unregisterCapability"
        params <- list(unregisterations = unname(lapply(removed, function(registration) {
            registration[c("id", "method")]
        })))
    } else {
        method <- "client/registerCapability"
        params <- list(registrations = unname(added))
    }
    self$deliver(self$request(method, params), callback = function(self, result, error = NULL) {
        state$pending <- FALSE
        if (!is.null(error)) {
            logger$info("Formatting capability update failed: ", error)
            if (!identical(desired, formatting_registrations(self$ClientCapabilities))) sync_formatting_capabilities(self)
            return(invisible(NULL))
        }
        if (length(removed)) {
            state$registered[names(removed)] <- NULL
        } else {
            state$registered[names(added)] <- added
        }
        sync_formatting_capabilities(self)
    })
    invisible(NULL)
}
