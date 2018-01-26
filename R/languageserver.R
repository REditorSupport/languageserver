#' @useDynLib languageserver
#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"

LanguageServer <- R6::R6Class("LanguageServer",
    public = list(
        stdin = NULL,
        stdout = NULL,
        will_exit = NULL,
        request_handlers = NULL,
        notification_handlers = NULL,
        documents = new.env(),

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        capabilities = NULL,

        coroutine_queue = Queue$new(),
        reply_queue = Queue$new(),

        initialize = function(stdin, stdout) {
            self$stdin <- stdin
            if (stdout == "stdout"){
                self$stdout <- stdout()
            } else {
                self$stdout <- stdout
            }
            self$register_handlers()
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
                    self$deliver(ResponseErrorMessage$new(id, "InternalError", to_string(e)))
                })
            } else {
                logger$error("unknown request: ", method)
                self$deliver(ResponseErrorMessage$new(
                    id, "MethodNotFound", paste0("unknown request ", method)))
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
                })
            } else {
                logger$error("unknown notification: ", method)
            }
        },

        register_handlers = function() {
            self$request_handlers <- list(
                initialize = on_initialize,
                shutdown = on_shutdown,
                `textDocument/completion` =  text_document_completion,
                `textDocument/hover` = text_document_hover,
                `textDocument/signatureHelp` = text_document_signature_help
            )

            self$notification_handlers <- list(
                initialized = on_initialized,
                exit = on_exit,
                `textDocument/didOpen` = text_document_did_open,
                `textDocument/didChange` = text_document_did_change,
                `textDocument/didSave` = text_document_did_save,
                `textDocument/didClose` = text_document_did_close
            )
        },

        process_event = function() {
            process_diagnostic_queue(self)
            self$process_coroutine_queue()
            self$process_reply_queue()
        },

        process_coroutine_queue = function() {
            for (i in seq_len(self$coroutine_queue$size())) {
                coroutine <- self$coroutine_queue$get()
                p <- coroutine$process
                if (p$is_alive()) {
                    self$coroutine_queue$put(coroutine)
                } else {
                    coroutine$callback(p$get_result())
                }
            }
        },

        process_reply_queue = function() {
            while (TRUE) {
                notification <- self$reply_queue$get()
                if (is.null(notification)) break
                self$deliver(notification)
            }
        },

        eventloop = function() {
            content <- file(self$stdin)
            open(content, blocking = FALSE)
            while (TRUE) {
                ret <- try({
                    self$process_event()
                    header <- readLines(content, n = 1)
                    if (length(header) == 0 || nchar(header) == 0) {
                        Sys.sleep(0.1)
                        next
                    }
                    logger$info("received: ", header)

                    matches <- stringr::str_match(header[1], "Content-Length: ([0-9]+)")
                    if (is.na(matches[2]))
                        stop("Unexpected input: ", header)

                    empty_line <- readLines(content, n = 1)
                    while (length(empty_line) == 0) {
                        empty_line <- readLines(content, n = 1)
                        Sys.sleep(0.05)
                    }
                    if (nchar(empty_line) > 0)
                        stop("Unexpected non-empty line")
                    nbytes <- as.numeric(matches[2])
                    data <- ""
                    while (nbytes > 0) {
                        newdata <- readChar(content, nbytes, useBytes = TRUE)
                        if (length(newdata) > 0) {
                            nbytes <- nbytes - nchar(newdata, type = "bytes")
                            data <- paste0(data, newdata)
                        }
                        Sys.sleep(0.05)
                    }
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
