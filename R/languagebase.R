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

        initialize = function() {
            self$register_handlers()
            self$request_callbacks <- collections::dict()
        },

        finalize = function() {
            self$request_callbacks$clear()
        },

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

        read_header = function() {
            header <- self$read_line()
            if (length(header) == 0 || !nzchar(header)) {
                return(NULL)
            }
            bytes <- NULL

            while (TRUE) {
                if (length(header) == 0) {
                    Sys.sleep(0.01)
                } else if (!nzchar(header)) {
                    break
                } else {
                    logger$info("received: ", header)

                    if (!startsWith(header, "Content")) {
                        stop("Unexpected non-empty line")
                    }
                    matches <- stringi::stri_match_first_regex(header, "Content-Length: ([0-9]+)")
                    if (!is.na(matches[2])) {
                        bytes <- as.integer(matches[2])
                    }
                }
                header <- self$read_line()
            }
            bytes
        },

        read_content = function(nbytes) {
            data <- ""
            while (nbytes > 0) {
                newdata <- self$read_char(nbytes)
                if (length(newdata) > 0) {
                    nbytes <- nbytes - nchar(newdata, type = "bytes")
                    data <- paste0(data, newdata)
                }
                Sys.sleep(0.01)
            }
            data
        },

        fetch = function(blocking = FALSE, timeout = Inf) {
            self$check_connection()
            start_time <- Sys.time()
            nbytes <- self$read_header()
            if (is.null(nbytes)) {
                if (!blocking) {
                    return(NULL)
                } else {
                    while (is.null(nbytes)) {
                        if (Sys.time() - start_time > timeout) {
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
            payload <- tryCatchStack(
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
                logger$info("unknown message")
            }
        },

        handle_request = function(request) {
            id <- request$id
            method <- request$method
            params <- request$params
            if (method %in% names(self$request_handlers)) {
                logger$info("handling request: ", method)
                tryCatchStack({
                    dispatch <- self$request_handlers[[method]]
                    dispatch(self, id, params)
                },
                error = function(e) {
                    logger$info("internal error:", e)
                    self$deliver(ResponseErrorMessage$new(id, "InternalError", to_string(e)))
                }
                )
            } else {
                logger$info("unknown request: ", method)
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
                tryCatchStack({
                    dispatch <- self$notification_handlers[[method]]
                    dispatch(self, params)
                },
                error = function(e) {
                    logger$info("internal error:", e)
                }
                )
            } else {
                logger$info("unknown notification: ", method)
            }
        },

        handle_response = function(response) {
            id <- response$id
            callback <- tryCatch(
                self$request_callbacks$pop(as.character(id)),
                error = function(e) NULL
            )
            if (is.null(response$error)) {
                if (!is.null(callback)) {
                    logger$info("calling callback")
                    if (self$catch_callback_error) {
                        tryCatchStack(
                            callback(self, result = response$result),
                            error = function(e) logger$info("callback error: ", e)
                        )
                    } else {
                        callback(self, result = response$result)
                    }
                }
            } else {
                logger$info("error:", response$error)
                # only call callback if it handles error
                if (!is.null(callback) && "error" %in% names(formals(callback))) {
                    logger$info("calling callback")
                    if (self$catch_callback_error) {
                        tryCatchStack(
                            callback(self, result = response$result, error = response$error),
                            error = function(e) logger$info("callback error: ", e)
                        )
                    } else {
                        callback(self, result = response$result, error = response$error)
                    }
                }
            }
        }
    )
)
