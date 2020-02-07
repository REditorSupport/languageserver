#' Base Message class
#' @keywords internal
Message <- R6::R6Class("Message",
    public = list(
        jsonrpc = "2.0",
        to_json = function() {
        },
        format = function() {
            json <- self$to_json()
            paste0("Content-Length: ", nchar(json, type = "bytes"),
                   "\r\n\r\n", json)
        }
    )
)

#' Request Message class
#'
#' Describe a request between the client and the server.
#' @keywords internal
Request <- R6::R6Class("Request",
    inherit = Message,
    public = list(
        id = NULL,
        method = NULL,
        params = NULL,
        initialize = function(id, method, params = NULL) {
            self$id <- id
            self$method <- method
            self$params <- params
        },
        to_json = function() {
            payload <- list(
                jsonrpc = self$jsonrpc,
                id = self$id,
                method = jsonlite::unbox(self$method)
            )
            if (!is.null(self$params)) {
                payload$params <- self$params
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
        }
    )
)


#' Notification Message class
#' @keywords internal
Notification <- R6::R6Class("Notification",
    inherit = Message,
    public = list(
        method = NULL,
        params = NULL,
        initialize = function(method, params = NULL) {
            self$method <- method
            self$params <- params
        },
        to_json = function() {
            payload <- list(
                jsonrpc = self$jsonrpc,
                method = jsonlite::unbox(self$method)
            )
            if (!is.null(self$params)) {
                payload$params <- self$params
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
        }
    )
)

#' Response Message class
#'
#' Message sent as the result of a [Request]
#' @keywords internal
Response <- R6::R6Class("Response",
    inherit = Message,
    public = list(
        id = NULL,
        result = NULL,
        error = NULL,
        initialize = function(id = NULL, result = NULL, error = NULL) {
            self$id <- id
            self$result <- unclass(result)
            self$error <- error
        },
        to_json = function() {
            payload <- list(
                jsonrpc = self$jsonrpc,
                id = self$id,
                result = self$result
            )
            if (!is.null(self$error)) {
                payload$error <- self$error
            }
            jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
        }
    )
)


ErrorCodes <- list(
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,
    serverErrorStart = -32099,
    serverErrorEnd = -32000,
    ServerNotInitialized = -32002,
    UnknownErrorCode = -32001,
    RequestCancelled = -32800
)


#' Response Error Message class
#'
#' Message sent as the result of a [Request] in case of an error.
#' @keywords internal
ResponseErrorMessage <- R6::R6Class(
    "Response",
    inherit = Response,
    public = list(
        initialize = function(id, errortype, message = NULL) {
            self$id <- id
            self$error <- list(code = ErrorCodes[[errortype]])
            if (!is.null(message)) {
                self$error$message <- message
            }
        }
    )
)

MessageType <- list(
    Error = 1,
    Warning = 2,
    Info = 3,
    Log = 4
)
