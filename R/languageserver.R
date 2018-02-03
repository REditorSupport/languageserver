#' @useDynLib languageserver
#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"


LanguageServer <- R6::R6Class("LanguageServer",
    private = list(
        last_process_sync_queue_time = Sys.time(),
        ping_time = Sys.time()
    ),
    public = list(
        tcp = FALSE,
        inputcon = NULL,
        outputcon = NULL,
        will_exit = NULL,
        request_handlers = NULL,
        notification_handlers = NULL,
        documents = new.env(),
        workspace = NULL,

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        capabilities = NULL,

        sync_queue = NULL,
        coroutine_queue = NULL,
        reply_queue = NULL,

        initialize = function(host, port) {
            if (is.null(port)) {
                logger$info("connection type: stdio")
                outputcon <- stdout()
                inputcon <- file("stdin")
                if (.Platform$OS.type == "windows") {
                    # Windows doesn't non-blocking read stdin
                    open(inputcon)
                } else {
                    open(inputcon, blocking = FALSE)
                }
            } else {
                self$tcp <- TRUE
                logger$info("connection type: tcp at ", port)
                inputcon <- socketConnection(host = host, port = port, open = "r+")
                logger$info("connected")
                outputcon <- inputcon
            }

            self$inputcon <- inputcon
            self$outputcon <- outputcon
            self$register_handlers()

            self$workspace <- Workspace$new()
            self$sync_queue <- MutableQueue$new()
            self$coroutine_queue <- Queue$new()
            self$reply_queue <- Queue$new()
        },

        finalize = function() {
            close(self$inputcon)
        },

        deliver = function(message) {
            if (!is.null(message)) {
                cat(message$format(), file = self$outputcon)
            }
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

        process_events = function() {
            if (Sys.time() - private$last_process_sync_queue_time > 0.5) {
                process_sync_queue(self)
                private$last_process_sync_queue_time <- Sys.time()
            }
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
            tcp <- self$tcp
            con <- self$inputcon
            while (TRUE) {
                ret <- try({
                    if (!isOpen(con)) break

                    if (Sys.time() - private$ping_time > 10) {
                        # trigger SIGPIPE if the server becomes orphan
                        cat("\n", file = stderr())
                        private$ping_time <- Sys.time()
                    }

                    if (isTRUE(self$will_exit)) {
                        logger$info("exiting")
                        break
                    }

                    self$process_events()

                    if (tcp) {
                        if (!socketSelect(list(con), timeout = 0)) {
                            Sys.sleep(0.1)
                            next
                        }
                    }
                    header <- read_line(con)
                    if (length(header) == 0 || nchar(header) == 0) {
                        Sys.sleep(0.1)
                        next
                    }
                    logger$info("received: ", header)

                    matches <- stringr::str_match(header, "Content-Length: ([0-9]+)")
                    if (is.na(matches[2]))
                        stop("Unexpected input: ", header)

                    empty_line <- read_line(con)
                    while (length(empty_line) == 0) {
                        empty_line <- read_line(con)
                        Sys.sleep(0.05)
                    }
                    if (nchar(empty_line) > 0)
                        stop("Unexpected non-empty line")
                    nbytes <- as.integer(matches[2])
                    data <- ""
                    while (nbytes > 0) {
                        newdata <- read_char(con, nbytes)
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
                    logger$error(as.list(traceback()))
                    logger$error("exiting")
                    break
                }
            }
        },

        run = function() {
            self$eventloop()
        }
    )
)


#' @export
run <- function(debug = FALSE, host = "localhost", port = NULL) {
    tools::Rd2txt_options(underline_titles = FALSE)
    logger$set_mode(debug = debug)
    langserver <- LanguageServer$new(host, port)
    langserver$run()
}
