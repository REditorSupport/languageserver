on_initialize <- function(self, id, params) {
    self$logger$info("initializing")
    self$logger$info("initialization config: ", params)
    self$processId = params$processId
    self$rootUri = params$rootUri
    self$initializationOptions = params$initializationOptions
    self$capabilities = params$capabilities
    Response$new(id = id, result=list(capabilities=ServerCapabilities))$deliver()
}
