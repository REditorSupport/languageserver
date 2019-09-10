#' Server capabilities
#'
#' List all supported capabilities of the language server, as defined in the
#' [Language Server Protocol specification](https://microsoft.github.io/language-server-protocol/specification).
#'
#' Currently the capabilities are:
#' + textDocumentSync
#' + hoverProvider
#' + completionProvider
#' + signatureHelpProvider
#' + definitionProvider
#' + documentSymbolProvider
#' + workspaceSymbolProvider
#' + documentFormattingProvider
#' + documentRangeFormattingProvider
#' @name ServerCapabilities
#' @export
NULL

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
    triggerCharacters = list(".")
)

SignatureHelpOptions <- list(
    triggerCharacters = list("(", ",")
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
    # typeDefinitionProvider = FALSE,
    # implementationProvider = FALSE,
    definitionProvider = TRUE,
    # referencesProvider = FALSE
    # documentHighlightProvider = FALSE,
    documentSymbolProvider = TRUE,
    workspaceSymbolProvider = TRUE,
    # codeActionProvider = FALSE,
    # codeLensProvider = CodeLensOptions,
    documentFormattingProvider = TRUE,
    documentRangeFormattingProvider = TRUE
    # documentOnTypeFormattingProvider = DocumentOnTypeFormattingOptions,
    # renameProvider = FALSE,
    # documentLinkProvider = DocumentLinkOptions,
    # colorProvider = FALSE,
    # foldingRangeProvider = FALSE,
    # executeCommandProvider = ExecuteCommandOptions,
    # workspace = list()
)
