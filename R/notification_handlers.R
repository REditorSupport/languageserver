on_initialized <- function(self, params) {
    rootPath <- self$rootPath
    # TODO: result lint result of the package
    # lint_result <- lintr::lint_package(rootPath)
}

on_exit <- function(self, params) {
    self$will_exit <- TRUE
}

cancelRequest <- function(self, params) {

}

workspacedidChangeConfiguration <- function(self, params) {

}

workspacedidChangeWatchedFiles <- function(self, params){

}

textDocumentdidOpen <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

textDocumentdidChange <- function(self, params) {

}

textDocumentwillSave <- function(self, params) {

}

textDocumentdidSave <- function(self, params) {
    textDocument <- params$textDocument
    publish_diagnostics(self, textDocument$uri)
}

textDocumentdidClose <- function(self, params) {

}
