ParseError = -32700;
InvalidRequest = -32600;
MethodNotFound = -32601;
InvalidParams = -32602;
InternalError = -32603;
serverErrorStart = -32099;
serverErrorEnd = -32000;
ServerNotInitialized = -32002;
UnknownErrorCode = -32001;
RequestCancelled = -32800;

SaveOptions <- list(
    includeText = 0
)

TextDocumentSyncOptions <- list(
    openClose = FALSE,
    change = 0,
    willSave = FALSE,
    willSaveWaitUntil = FALSE,
    save = SaveOptions
)

CompletionOptions <- list(
    resolveProvider = FALSE,
    triggerCharacters = NULL
)

SignatureHelpOptions <- list(
    triggerCharacters = NULL
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
    # textDocumentSync = TextDocumentSyncOptions,
    # hoverProvider = FALSE,
    # completionProvider = CompletionOptions,
    # signatureHelpProvider = SignatureHelpOptions,
    definitionProvider = FALSE,
    referencesProvider = FALSE
    # documentHighlightProvider = FALSE,
    # documentSymbolProvider = FALSE,
    # workspaceSymbolProvider = FALSE,
    # codeActionProvider = FALSE,
    # codeLensProvider = CodeLensOptions,
    # documentFormattingProvider = FALSE,
    # documentRangeFormattingProvider = FALSE,
    # documentOnTypeFormattingProvider = DocumentOnTypeFormattingOptions,
    # renameProvider = FALSE,
    # documentLinkProvider = DocumentLinkOptions,
    # executeCommandProvider = ExecuteCommandOptions
)
