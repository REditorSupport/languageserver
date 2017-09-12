#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"


LanguageServer <- R6::R6Class("LanguageServer",
    public = list(
        logger = NULL,
        stdout = NULL,
        will_exit = NULL,

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        capabilities = NULL,

        initialize = function(stdout) {
            if (stdout == "stdout"){
                self$stdout <- stdout()
            } else {
                self$stdout <- stdout
            }
        },

        deliver = function(message) {
            cat(message$format(), file = self$stdout)
        },

        handle_raw = function(data) {
            tryCatch({
                payload <- jsonlite::fromJSON(data)
                pl_names <- names(payload)
                logger$info("payload: ", data)

                if ("id" %in% pl_names && "method" %in% pl_names) {
                    self$handle_request(payload)
                } else if ("method" %in% pl_names) {
                    self$handle_notification(payload)
                } else {
                    logger$error("not request or notification")
                }
            },
            error = function(e){
                logger$error("error handling json: ", e)
            })
        },

        handle_request = function(request) {
            id <- request$id
            method <- request$method
            params <- request$params
            if (method %in% names(request_handlers)) {
                logger$info("handling request: ", method)
                dispatch <- request_handlers[[method]]
                dispatch(self, id, params)
            } else {
                logger$error("unknown request: ", method)
            }
        },

        handle_notification = function(notification) {
            method <- notification$method
            params <- notification$params
            if (method %in% names(notification_handlers)) {
                logger$info("handling notification: ", method)
                dispatch <- notification_handlers[[method]]
                dispatch(self, params)
            } else {
                logger$error("unknown notification: ", method)
            }
        }
    )
)
