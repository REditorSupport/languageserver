#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"


LanguageServer <- R6::R6Class("LanguageServer",
    public = list(
        logger = NULL,
        request_handlers = list(
            initialize = "on_initialize"
        ),
        initialize = function() {
            self$logger = logging$get_logger()
        },
        handle_raw = function(data) {
            tryCatch({
                payload <- jsonlite::fromJSON(data)
                pl_names <- names(payload)
                self$logger$info("names of payload: ", paste0(pl_names, collapse = " "))

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
        request_handler_exist = function(method) {
            method %in% names(self$request_handlers)
        },
        handle_request = function(request) {
            id <- request$id
            method <- request$method
            params <- request$params
            if (self$request_handler_exist(method)) {
                self$logger$info("handling request: ", method)
                dispatch <- self$request_handlers[[method]]
                self[[dispatch]](id, params)
            } else {
                self$logger$error("unknown request: ", method)
            }
        },
        handle_notification = function(notification) {

        },
        processId = NULL,
        rootUri = NULL,
        initializationOptions = NULL,
        capabilities = NULL,
        on_initialize = function(id, params) {
            self$logger$info("initializing")
            self$logger$info("initialization config: ", params)
            self$processId = params$processId
        }
    )
)
