# Parent class of LanguageServer and LanguageClient, containing
# some of the common functions

LanguageBase <- R6::R6Class("LanguageBase",
    private = list(
        ticket = 0
    ),
    public = list(
        catch_callback_error = TRUE,
        request_handlers = NULL,
        notification_handlers = NULL,
        request_callbacks = NULL,

        get_ticket = function() {
            private$ticket <- private$ticket + 1
            private$ticket
        },

        request = function(method, params) {
            Request$new(
                self$get_ticket(),
                method,
                params
            )
        },

        deliver = function(message, callback = NULL) {
            if (is.null(message)) {
                return(NULL)
            }
            logger$info("deliver: ", class(message))
            method <- message$method
            if (!is.null(method)) {
                logger$info("method: ", method)
            }
            self$write_text(message$format())
            if (inherits(message, "Request") && !is.null(callback)) {
                id <- message$id
                self$request_callbacks$set(as.character(id), callback)
            }
        },

        fetch = function(blocking = FALSE, timeout = Inf) {
            start_time <- Sys.time()
            nbytes <- self$read_header()
            if (is.null(nbytes)) {
                if (!blocking) {
                    return(NULL)
                } else {
                    while (is.null(nbytes)) {
                        if ((Sys.time() - start_time > timeout)) {
                            return(NULL)
                        }
                        Sys.sleep(0.1)
                        nbytes <- self$read_header()
                    }
                }
            }

            data <- self$read_content(nbytes)
            data
        },

        handle_raw = function(data) {
            payload <- tryCatch(
                jsonlite::fromJSON(data, simplifyVector = FALSE),
                error = function(e) e
            )
            if (inherits(payload, "error")) {
                logger$error("error handling json: ", payload)
                return(NULL)
            }
            pl_names <- names(payload)
            logger$info("received payload.")
            if ("id" %in% pl_names && "method" %in% pl_names) {
                self$handle_request(payload)
            } else if ("method" %in% pl_names) {
                self$handle_notification(payload)
            } else if ("id" %in% pl_names) {
                self$handle_response(payload)
            } else {
                logger$error("unknown message")
            }
        },

        handle_request = function(request) {
            id <- request$id
            method <- request$method
            params <- request$params
            if (method %in% names(self$request_handlers)) {
                logger$info("handling request: ", method)
                tryCatch({
                    dispatch <- self$request_handlers[[method]]
                    dispatch(self, id, params)
                },
                error = function(e) {
                    logger$error("internal error: ", e)
                    self$deliver(ResponseErrorMessage$new(id, "InternalError", to_string(e)))
                }
                )
            } else {
                logger$error("unknown request: ", method)
                self$deliver(ResponseErrorMessage$new(
                    id, "MethodNotFound", paste0("unknown request ", method)
                ))
            }
        },

        handle_notification = function(notification) {
            method <- notification$method
            params <- notification$params
            if (method %in% names(self$notification_handlers)) {
                logger$info("handling notification: ", method)
                tryCatch({
                    dispatch <- self$notification_handlers[[method]]
                    dispatch(self, params)
                },
                error = function(e) {
                    logger$error("internal error: ", e)
                }
                )
            } else {
                logger$error("unknown notification: ", method)
            }
        },

        handle_response = function(response) {
            id <- response$id
            callback <- tryCatch(
                self$request_callbacks$pop(as.character(id)),
                error = function(e) NULL
            )
            if ("error" %in% names(response)) {
                logger$info("got an error: ", response$error)
            } else if (!is.null(callback)) {
                logger$info("calling callback")
                if (self$catch_callback_error) {
                    tryCatch(
                        callback(self, response$result),
                        error = function(e) logger$error("callback error: ", e)
                    )
                } else {
                    callback(self, response$result)
                }
            }
        }
    )
)
