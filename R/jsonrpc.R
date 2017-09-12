Message <- R6::R6Class("Message",
    public = list(
        jsonrpc = 2,
        to_json = function() {
        },
        deliver = function() {
            data <- self$to_json()
            buffer <- paste0("Content-Length: ", nchar(data, type = "bytes"), "\r\n\r\n", data)
            cat(buffer, file = stdout())
        }
    )
)


Request <- R6::R6Class("Request",
    inherit = Message,
    public = list(
        id = NULL,
        method = NULL,
        params = NULL,
        initialize = function(id, method, params=NULL) {
            self$id <- id
            self$method <- method
            self$params <- params
        },
        to_json = function() {
            payload <- list()
            payload$jsonrpc <- self$jsonrpc
            payload$id <- self$id
            payload$method <- jsonlite::unbox(self$method)
            if (!is.null(self$params)) {
                payload$params <- self$params
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE)
        }
    )
)


Notification <- R6::R6Class("Notification",
    inherit = Message,
    public = list(
        method = NULL,
        params = NULL,
        initialize = function(method, params=NULL) {
            self$method <- method
            self$params <- params
        },
        to_json = function() {
            payload <- list()
            payload$jsonrpc <- jsonlite::unbox(self$jsonrpc)
            payload$method <- jsonlite::unbox(self$method)
            if (!is.null(self$params)) {
                payload$params <- self$params
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE)
        }
    )
)


Response <- R6::R6Class("Response",
    inherit = Message,
    public = list(
        id = NULL,
        result = NULL,
        error = NULL,
        initialize = function(id, result=NULL, error=NULL) {
            self$id <- id
            self$result <- result
            self$error <- error
        },
        to_json = function() {
            payload <- list()
            payload$jsonrpc <- self$jsonrpc
            payload$id <- self$id
            if (!is.null(self$result)) {
                payload$result <- self$result
            }
            if (!is.null(self$error)) {
                payload$error <- self$error
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE)
        }
    )
)
