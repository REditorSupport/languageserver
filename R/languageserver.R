#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"


languageserver <- function() {
    LanguageServer <- R6::R6Class("LanguageServer",
        public = list(
            logger = NULL,
            initialize = function() {
                self$logger = logging$get_logger()
            },
            handle_raw = function(data) {
                tryCatch({
                    payload <- jsonlite::fromJSON(data)
                    pl_names <- names(payload)
                    self$logger$info("payload: ", data)

                    if ("id" %in% pl_names && "method" %in% pl_names) {
                        self$handle_request(payload)
                    } else if ("method" %in% pl_names) {
                        self$handle_notification(payload)
                    } else {
                        self$logger$error("not request or notification")
                    }
                },
                error = function(e){
                    self$logger$error("error handling json: ", e)
                })
            },
            request_handlers = list(
                initialize = on_initialize
            ),
            handle_request = function(request) {
                id <- request$id
                method <- request$method
                params <- request$params
                if (method %in% names(self$request_handlers)) {
                    self$logger$info("handling request: ", method)
                    dispatch <- self$request_handlers[[method]]
                    dispatch(self, id, params)
                } else {
                    self$logger$error("unknown request: ", method)
                }
            },
            notification_handlers = list(
                initialized = on_initialized
            ),
            handle_notification = function(notification) {
                method <- notification$method
                params <- notification$params
                if (method %in% names(self$notification_handlers)) {
                    self$logger$info("handling notification: ", method)
                    dispatch <- self$notification_handlers[[method]]
                    dispatch(self, params)
                } else {
                    self$logger$error("unknown notification: ", method)
                }
            },
            processId = NULL,
            rootUri = NULL,
            initializationOptions = NULL,
            capabilities = NULL

        )
    )
    LanguageServer$new()
}
