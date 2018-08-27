on_initialize <- function(self, id, params) {
    logger$info("initialization config: ", params)
    self$processId <- params$processId
    self$rootUri <- params$rootUri
    self$rootPath <- path_from_uri(self$rootUri)
    self$initializationOptions <- params$initializationOptions
    self$capabilities <- params$capabilities
    self$deliver(Response$new(id = id, result = list(capabilities = ServerCapabilities)))
}

# Notification
on_initialized <- function(self, params) {
    logger$info("on_initialized")
    if (is_package(self$rootUri)) {
        logger$info("is package")

        source_dir <- file.path(path_from_uri(self$rootUri), "R")
        files <- list.files(source_dir)
        for (f in files) {
            uri <- path_to_uri(file.path(source_dir, f))
            self$sync_input_dict$set(uri, list(document = NULL, run_lintr = FALSE, parse = TRUE))
        }
    }
    # TODO: result lint result of the package
    # lint_result <- lintr::lint_package(rootPath)
}

# Request
on_shutdown <- function(self, id, params) {
    self$exit_flag <- TRUE
    self$deliver(Response$new(id = id, result = list()))
}

# Notification
on_exit <- function(self, params) {
    self$exit_flag <- TRUE
}

# Notification
cancel_request <- function(self, params) {

}
