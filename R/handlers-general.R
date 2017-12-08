on_initialize <- function(self, id, params) {
    logger$info("initialization config: ", params)
    self$processId <- params$processId
    self$rootUri <- params$rootUri
    self$rootPath <- parse_uri(self$rootUri)
    self$initializationOptions <- params$initializationOptions
    self$capabilities <- params$capabilities
    self$deliver(Response$new(id = id, result = list(capabilities = ServerCapabilities)))
}

# Notification
on_initialized <- function(self, params) {
    rootPath <- self$rootPath
    # TODO: result lint result of the package
    # lint_result <- lintr::lint_package(rootPath)
}

# Request
on_shutdown <- function(self, id, params) {

}

# Notification
on_exit <- function(self, params) {
    self$will_exit <- TRUE
}

# Notification
cancel_request <- function(self, params) {

}
