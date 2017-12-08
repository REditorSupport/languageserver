#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"


Documents <- R6::R6Class("Documents",
    public = list(
        get = function(uri) {
            if (uri %in% names(private$cache)) {
                private$cache[[uri]]
            } else {
                logger$error(uri, "not found")
                stop("file no found.")
            }
        },

        set = function(uri, text, multiline = FALSE) {
            private$cache[[uri]] <- text
        }
    ),
    private = list(
        cache = list()
    )
)


LanguageServer <- R6::R6Class("LanguageServer",
    public = list(
        stdin = NULL,
        stdout = NULL,
        will_exit = NULL,
        request_handlers = NULL,
        notification_handlers = NULL,
        documents = Documents$new(),

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        capabilities = NULL,

        initialize = function(stdin, stdout) {
            self$stdin <- stdin
            if (stdout == "stdout"){
                self$stdout <- stdout()
            } else {
                self$stdout <- stdout
            }
            self$registering_handlers()
        },

        deliver = function(message) {
            cat(message$format(), file = self$stdout)
        },

        handle_raw = function(data) {
            tryCatch({
                payload <- jsonlite::fromJSON(data)
                pl_names <- names(payload)
                logger$info("received payload.")
            },
            error = function(e){
                logger$error("error handling json: ", e)
            })
            if ("id" %in% pl_names && "method" %in% pl_names) {
                self$handle_request(payload)
            } else if ("method" %in% pl_names) {
                self$handle_notification(payload)
            } else {
                logger$error("not request or notification")
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
                    self$deliver(ResponseErrorMessage$new(id, "InternalError", str(e)))
                })
            } else {
                logger$error("unknown request: ", method)
                self$deliver(ResponseErrorMessage$new(id, MethodNotFound, "unknown request"))
            }
        },

        handle_notification = function(notification) {
            method <- notification$method
            params <- notification$params
            if (method %in% names(self$notification_handlers)) {
                logger$info("handling notification: ", method)
                dispatch <- self$notification_handlers[[method]]
                dispatch(self, params)
            } else {
                logger$error("unknown notification: ", method)
                self$deliver(ResponseErrorMessage$new(NULL, MethodNotFound, "unknown notification"))
            }
        },

        registering_handlers = function() {
            self$request_handlers <- list(
                initialize = on_initialize,
                `textDocument/completion` =  text_document_completion,
                `textDocument/signatureHelp` = text_document_signature_help
            )

            self$notification_handlers <- list(
                initialized = on_initialized,
                exit = on_exit,
                `textDocument/didOpen` = text_document_did_open,
                `textDocument/didChange` = text_document_did_change,
                `textDocument/didSave` = text_document_did_save
            )
        },

        eventloop = function() {
            content <- file(self$stdin)
            open(content, blocking = TRUE)
            while (TRUE) {
                ret <- try({
                    header <- readLines(content, n = 1)
                    if (str_empty(header))
                        next
                    logger$info("received: ", header)

                    matches <- stringr::str_match(header[1], "Content-Length: ([0-9]+)")
                    if (is.na(matches[2]))
                        stop("Unexpected input: ", header)

                    empty_line <- readLines(content, n = 1)
                    if (!str_empty(empty_line))
                        stop("Unexpected non-empty line")

                    data <- readChar(content, as.numeric(matches[2]), useBytes = TRUE)
                    self$handle_raw(data)
                })
                if (inherits(ret, "try-error")) {
                    logger$error(ret)
                    logger$error("exiting")
                    break
                }
                if (isTRUE(self$will_exit)) {
                    logger$info("exiting")
                    break
                }
            }
            close(content)
        },

        run = function() {
            self$eventloop()
        }
    )
)


#' @export
run <- function(debug = FALSE, stdin = "stdin", stdout = "stdout") {
    logger$set_mode(debug = debug)
    langserver <- LanguageServer$new(stdin = stdin, stdout = stdout)
    langserver$run()
}
