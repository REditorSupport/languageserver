on_initialize <- function(self, id, params) {
    self$logger$info("initializing")
    self$logger$info("initialization config: ", params)
    self$processId <- params$processId
    self$rootUri <- params$rootUri
    self$rootPath <- parse_uri(self$rootUri)
    self$initializationOptions <- params$initializationOptions
    self$capabilities <- params$capabilities
    self$deliver(Response$new(id = id, result = list(capabilities = ServerCapabilities)))
}

on_shutdown <- function(self, id, params) {

}

workspacesymbol <- function(self, id, params) {

}

workspaceexecuteCommand <- function(self, id, params) {

}

textDocumentwillSaveWaitUntil <- function(self, id, params) {

}

textDocumentcompletion  <- function(self, id, params) {

}

completionItemresolve  <- function(self, id, params) {

}

textDocumenthover  <- function(self, id, params) {

}

textDocumentsignatureHelp  <- function(self, id, params) {

}

textDocumentreferences  <- function(self, id, params) {

}

textDocumentdocumentHighlight  <- function(self, id, params) {

}

textDocumentdocumentSymbol  <- function(self, id, params) {

}

textDocumentformatting  <- function(self, id, params) {

}

textDocumentrangeFormatting  <- function(self, id, params) {

}

textDocumentonTypeFormatting  <- function(self, id, params) {

}

textDocumentdefinition  <- function(self, id, params) {

}

textDocumentcodeAction  <- function(self, id, params) {

}

textDocumentcodeLens  <- function(self, id, params) {

}

codeLensresolve  <- function(self, id, params) {

}

textDocumentdocumentLink  <- function(self, id, params) {

}

documentLinkresolve  <- function(self, id, params) {

}

textDocumentrename  <- function(self, id, params) {

}
