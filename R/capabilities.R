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
    change = TextDocumentSyncKind$Full,
    willSave = FALSE,
    willSaveWaitUntil = FALSE,
    save = SaveOptions
)

CompletionOptions <- list(
    resolveProvider = TRUE,
    triggerCharacters = list(".", ":")
)

SignatureHelpOptions <- list(
    triggerCharacters = list("(", ",")
)

CodeLensOptions <- list(
    resolveProvider = FALSE
)

DocumentOnTypeFormattingOptions <- list(
    firstTriggerCharacter = "\n",
    moreTriggerCharacter = list(")", "]", "}")
)

DocumentLinkOptions <- list(
    resolveProvider = FALSE
)

RenameOptions <- list(
    prepareProvider = TRUE
)

ExecuteCommandOptions <- list(
    commands = NULL
)

ServerCapabilities <- list(
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
    # codeActionProvider = FALSE,
    # codeLensProvider = CodeLensOptions,
    documentFormattingProvider = TRUE,
    documentRangeFormattingProvider = TRUE,
    documentOnTypeFormattingProvider = DocumentOnTypeFormattingOptions,
    renameProvider = TRUE,
    documentLinkProvider = DocumentLinkOptions,
    colorProvider = TRUE,
    foldingRangeProvider = TRUE
    # selectionRangeProvider = FALSE,
    # executeCommandProvider = ExecuteCommandOptions,
    # workspace = list()
)

update_server_capabilities <- function(server_capabilities, client_capabilities) {
    # RenameOptions may only be specified if the client states that
    # it supports prepareSupport in its initial initialize request
    if (isTRUE(server_capabilities$renameProvider) &&
        isTRUE(client_capabilities$textDocument$rename$prepareSupport)) {
        server_capabilities$renameProvider <- RenameOptions
    }

    server_capabilities
}
