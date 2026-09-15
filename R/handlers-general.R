#' `initialize` handler
#'
#' Handler to the `initialize` [Request].
#'
#' @noRd
on_initialize <- function(self, id, params) {
    trace <- params$trace
    if (!is.null(trace) && trace %in% c("messages", "verbose")) {
        lsp_settings$set("trace", TRUE)
    }

    logger$info("session: ", list(
        system = as.list(Sys.info()),
        pid = Sys.getpid(),
        wd = getwd(),
        args = commandArgs(),
        ver = unclass(R.version),
        locale = Sys.getlocale(),
        env = as.list(Sys.getenv()),
        libpaths = .libPaths(),
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
    
    if (is.list(params$workspaceFolders) && length(params$workspaceFolders) > 0) {
        logger$info("workspaceFolders provided: ", length(params$workspaceFolders))
        for (folder in params$workspaceFolders) {
            self$add_workspace(uri_escape_unicode(folder$uri))
        }
    } else {
        self$add_workspace(self$rootUri)
    }
    logger$info("workspaces initialized: ", self$workspaces$size())

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
#' @noRd
on_initialized <- function(self, params) {
    logger$info("on_initialized")
    # a bit like devtools::load_all()
    self$load_workspaces()
    # TODO: result lint result of the package
    # lint_result <- lintr::lint_package(rootPath)
}

#' `shutdown` request handler
#'
#' Handler to the `shutdown` [Request].
#' @noRd
on_shutdown <- function(self, id, params) {
    self$exit_flag <- TRUE
    self$deliver(Response$new(id = id, result = list()))
}


#' `exit` notification handler
#'
#' Handler to the `exit` [Notification].
#' @noRd
on_exit <- function(self, params) {
    self$exit_flag <- TRUE
}

#' `cancel` request notification handler
#'
#' Handler to the `cancelRequest` [Notification].
#' @noRd
cancel_request <- function(self, params) {
    request_id <- params$id
    if (!is.null(request_id)) cancel_formatting_requests(self, id = request_id)
    for (uri in self$pending_replies$keys()) {
        queues <- self$pending_replies$get(uri)
        for (queue in queues) {
            retained <- list()
            while (queue$size()) {
                item <- queue$pop()
                if (identical(as.character(item$id), as.character(request_id))) {
                    self$deliver(ResponseErrorMessage$new(
                        item$id,
                        "RequestCancelled",
                        "Request cancelled by client"
                    ))
                } else {
                    retained[[length(retained) + 1L]] <- item
                }
            }
            for (item in retained) queue$push(item)
        }
    }
}
