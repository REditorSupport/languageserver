#' initialize handler
#'
#' Handler to the [initialize](https://microsoft.github.io/language-server-protocol/) [Request].
#'
#' @template self
#' @template id
#' @param params a named list, the `initialize` Request options
#'
#'  @keywords internal
on_initialize <- function(self, id, params) {
    logger$info("initialization config: ", params)
    self$processId <- params$processId
    self$rootUri <- params$rootUri
    self$rootPath <- path_from_uri(self$rootUri)
    self$initializationOptions <- params$initializationOptions
    self$ClientCapabilities <- params$capabilities
    self$deliver(Response$new(id = id, result = list(capabilities = ServerCapabilities)))
}

#' initialized handler
#'
#' Handler to the [initialized](https://microsoft.github.io/language-server-protocol/) [Notification].
#'
#' @template self
#' @param params a named list
#'
#' @keywords internal
on_initialized <- function(self, params) {
    logger$info("on_initialized")
    if (is_package(self$rootUri)) {

        project_root <- path_from_uri(self$rootUri)
        source_dir <- file.path(project_root, "R")
        files <- list.files(source_dir)
        for (f in files) {
            logger$info("load ", f)
            uri <- path_to_uri(file.path(source_dir, f))
            self$text_sync(uri, document = NULL, run_lintr = FALSE, parse = TRUE)
        }
        deps <- tryCatch(desc::desc_get_deps(project_root), error = function(e) NULL)
        if (!is.null(deps)) {
            packages <- Filter(function(x) x != "R", deps$package[deps$type == "Depends"])
            for (package in packages) {
                logger$info("load package:", package)
                self$workspace$load_package(package)
            }
        }
    }
    # TODO: result lint result of the package
    # lint_result <- lintr::lint_package(rootPath)
}

#' shutdown request handler
#'
#' Handler to the [shutdown](https://microsoft.github.io/language-server-protocol/) [Request]
#'
#' @template self
#' @template id
#' @param params unused here
#'
#' @keywords internal
on_shutdown <- function(self, id, params) {
    self$exit_flag <- TRUE
    self$deliver(Response$new(id = id, result = list()))
}


#' exit notification handler
#'
#' Hanlder to the [exit](https://microsoft.github.io/language-server-protocol/) [Notification]
#'
#' @template self
#' @param params unused here
#'
#' @keywords internal
on_exit <- function(self, params) {
    self$exit_flag <- TRUE
}

#' cancel request notification handler
#'
#' Handler to the [cancelRequest](https://microsoft.github.io/language-server-protocol/) [Notification]
#'
#' @template self
#' @param params unused here
#'
#' @keywords internal
cancel_request <- function(self, params) {

}
