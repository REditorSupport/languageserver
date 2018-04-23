SaveOptions <- list(
    includeText = FALSE
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
    resolveProvider = FALSE,
    triggerCharacters = c(".")
)

SignatureHelpOptions <- list(
    triggerCharacters = c("(", ",")
)

CodeLensOptions <- list(
    resolveProvider = FALSE
)

DocumentOnTypeFormattingOptions <- list(
    firstTriggerCharacter = NULL,
    moreTriggerCharacter = NULL
)

DocumentLinkOptions <- list(
    resolveProvider = FALSE
)

ExecuteCommandOptions <- list(
    commands = NULL
)

ServerCapabilities <- list(
    textDocumentSync = TextDocumentSyncOptions,
    hoverProvider = TRUE,
    completionProvider = CompletionOptions,
    signatureHelpProvider = SignatureHelpOptions,
    # definitionProvider = FALSE,
    # referencesProvider = FALSE
    # documentHighlightProvider = FALSE,
    # documentSymbolProvider = FALSE,
    # workspaceSymbolProvider = FALSE,
    # codeActionProvider = FALSE,
    # codeLensProvider = CodeLensOptions,
    documentFormattingProvider = TRUE,
    documentRangeFormattingProvider = TRUE
    # documentOnTypeFormattingProvider = DocumentOnTypeFormattingOptions,
    # renameProvider = FALSE,
    # documentLinkProvider = DocumentLinkOptions,
    # executeCommandProvider = ExecuteCommandOptions
)
