#' `initialize` handler
#'
#' Handler to the `initialize` [Request].
#'
#' @keywords internal
on_initialize <- function(self, id, params) {
    trace <- params$trace
    if (!is.null(trace) && trace %in% c("messages", "verbose")) {
        lsp_settings$set("debug", TRUE)
    }

    logger$info("session: ", list(
        system = as.list(Sys.info()),
        pid = Sys.getpid(),
        wd = getwd(),
        args = commandArgs(),
        ver = unclass(R.version),
        locale = Sys.getlocale(),
        env = as.list(Sys.getenv()),
        namespaces = local({
            nss <- loadedNamespaces()
            vs <- lapply(nss, function(ns) format(utils::packageVersion(ns)))
            names(vs) <- nss
            vs
        }),
        search = search()
    ))
    logger$info("initialization config: ", params)
    self$processId <- params$processId
    self$rootUri <- uri_escape_unicode(params$rootUri)
    self$rootPath <- path_from_uri(self$rootUri)
    self$workspace <- Workspace$new(self$rootPath)
    self$initializationOptions <- params$initializationOptions
    self$ClientCapabilities <- params$capabilities
    server_capabilities <- update_server_capabilities(
        ServerCapabilities, self$ClientCapabilities)
    server_capabilities <- merge_list(
        server_capabilities,
        lsp_settings$get("server_capabilities"))
    self$ServerCapabilities <- server_capabilities
    self$deliver(Response$new(id = id, result = list(capabilities = server_capabilities)))
}

#' `initialized` handler
#'
#' Handler to the `initialized` [Notification].
#'
#' @keywords internal
on_initialized <- function(self, params) {
    logger$info("on_initialized")
    project_root <- self$rootPath
    if (length(project_root) && is_package(project_root)) {
        # a bit like devtools::load_all()
        self$workspace$load_all(self)
        # TODO: result lint result of the package
        # lint_result <- lintr::lint_package(rootPath)
    }
}

#' `shutdown` request handler
#'
#' Handler to the `shutdown` [Request].
#' @keywords internal
on_shutdown <- function(self, id, params) {
    self$exit_flag <- TRUE
    self$deliver(Response$new(id = id, result = list()))
}


#' `exit` notification handler
#'
#' Handler to the `exit` [Notification].
#' @keywords internal
on_exit <- function(self, params) {
    self$exit_flag <- TRUE
}

#' `cancel` request notification handler
#'
#' Handler to the `cancelRequest` [Notification].
#' @keywords internal
cancel_request <- function(self, params) {

}
