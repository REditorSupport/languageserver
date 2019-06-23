# for testing purpose

LanguageClient <- R6::R6Class("LanguageClient",
    private = list(
        ticket = 0
    ),
    public = list(
        process = NULL,
        rootUri = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,

        request_handlers = NULL,
        notification_handlers = NULL,
        response_handlers = NULL,

        initialize = function(cmd, args) {
            self$process <- processx::process$new(cmd, args,
                stdin = "|", stdout = "|", stderr = "|", supervise = TRUE)
            self$register_handlers()
            self$response_handlers <- collections::Dict$new()
        },

        get_ticket = function() {
            private$ticket <- private$ticket + 1
            private$ticket
        },

        deliver = function(message, callback = NULL) {
            if (!is.null(message)) {
                self$process$write_input(message$format())
                logger$info("deliver: ", class(message))
                method <- message$method
                if (!is.null(method)) {
                    logger$info("method: ", method)
                }
                if (inherits(message, "Request") && !is.null(callback)) {
                    id <- message$id
                    self$response_handlers$set(as.character(id), callback)
                }
            }
        },

        request = function(method, params) {
            Request$new(
                self$get_ticket(),
                method,
                params
            )
        },

        check_connection = function() {
            if (!self$process$is_alive())
                stop("Server is dead.")
        },

        read_one_output_line = function() {
            line <- self$process$read_output_lines(1)
            trimws(line, "right")
        },

        read_header = function() {
            if (!self$process$is_alive() || self$process$poll_io(1)[1] != "ready") return(NULL)
            header <- self$read_one_output_line()
            if (length(header) == 0 || nchar(header) == 0) return(NULL)
            logger$info("received: ", header)
            matches <- stringr::str_match(header, "Content-Length: ([0-9]+)")
            if (is.na(matches[2]))
                stop("Unexpected input: ", header)
            as.integer(matches[2])
        },

        read_content = function(nbytes) {
            empty_line <- self$read_one_output_line()
            while (length(empty_line) == 0) {
                empty_line <- self$read_one_output_line()
                empty_line
                Sys.sleep(0.01)
            }
            if (nchar(empty_line) > 0) {
                stop("Unexpected non-empty line")
            }
            data <- ""
            while (nbytes > 0) {
                newdata <- self$process$read_output(nbytes)
                if (length(newdata) > 0) {
                    nbytes <- nbytes - nchar(newdata, type = "bytes")
                    data <- paste0(data, newdata)
                }
                Sys.sleep(0.01)
            }
            data
        },

        fetch = function(blocking = FALSE) {
            nbytes <- self$read_header()
            if (is.null(nbytes)) {
                if (!blocking) {
                    return(NULL)
                } else {
                    while (is.null(nbytes)) {
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
                error = function(e) e)
            if (inherits(payload, "error")) {
                logger$error("error handling json: ", payload)
                return(NULL)
            }
            pl_names <- names(payload)
            logger$info("received payload.")

            if ("result" %in% pl_names) {
                self$handle_response(payload)
            } else {
                logger$error("got unexpected message")
            }
        },

        handle_response = function(response) {
            id <- response$id
            callback <- self$response_handlers$pop(as.character(id))
            if ("error" %in% names(response)) {
                logger$info("got an error: ", response$error)
            } else {
                tryCatch(
                    callback(self, response$result),
                    error = function(e) logger$info("callback error: ", e))
            }
        },

        welcome = function() {
            self$deliver(
                self$request(
                    "initialize",
                    list(
                        rootUri = self$rootUri,
                        capabilities = self$ClientCapabilities
                    )
                ),
                callback = function(self, result) {
                    self$ServerCapabilities <- result$capabilities
                }
            )
        },

        start = function(working_dir = getwd(), capabilities = NULL) {
            self$rootUri <- path_to_uri(working_dir)
            self$ClientCapabilities <- capabilities
            self$welcome()
        },

        stop = function() {
            self$process$kill()
            self$response_handlers <- NULL
        }
    )
)


LanguageClient$set("public", "register_handlers", function() {
    self$request_handlers <- list()
    self$notification_handlers <- list()
})
