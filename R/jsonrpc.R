ParseError = -32700;
InvalidRequest = -32600;
MethodNotFound = -32601;
InvalidParams = -32602;
InternalError = -32603;
serverErrorStart = -32099;
serverErrorEnd = -32000;
ServerNotInitialized = -32002;
UnknownErrorCode = -32001;
RequestCancelled = -32800;


Message <- R6::R6Class("Message",
    public = list(
        jsonrpc = 2,
        to_json = function(x) {}
    )
)


Request <- R6::R6Class("Request",
    inherit = Message,
    public = list(
        id = NULL,
        method = NULL,
        params = NULL,
        initialize = function(id, method, params=NULL) {
            self$id = id
            self$method = method
            self$params = params
        },
        to_json = function(x) {
            payload = list()
            payload$jsonrpc = jsonlite::unbox(self$jsonrpc)
            payload$id = jsonlite::unbox(self$id)
            payload$method = jsonlite::unbox(self$method)
            if (!is.null(self$params)) {
                payload$params = self$params
            }
            jsonlite::toJSON(payload)
        }
    )
)


Notification <- R6::R6Class("Notification",
    inherit = Message,
    public = list(
        method = NULL,
        params = NULL,
        initialize = function(method, params=NULL) {
            self$method = method
            self$params = params
        },
        to_json = function(x) {
            payload = list()
            payload$jsonrpc = jsonlite::unbox(self$jsonrpc)
            payload$method = jsonlite::unbox(self$method)
            if (!is.null(self$params)) {
                payload$params = self$params
            }
            jsonlite::toJSON(payload)
        }
    )
)
