#' @useDynLib languageserver
#' @importFrom R6 R6Class
#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"

#' the language server
#'
#' Describe the language server and how it interacts with clients.
LanguageServer <- R6::R6Class("LanguageServer",
    inherit = LanguageBase,
    public = list(
        tcp = FALSE,
        inputcon = NULL,
        outputcon = NULL,
        exit_flag = NULL,

        documents = new.env(),
        workspace = NULL,

        run_lintr = TRUE,

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        ClientCapabilities = NULL,

        sync_in = NULL,
        sync_out = NULL,
        reply_queue = NULL,

        initialize = function(host, port) {
            if (is.null(port)) {
                logger$info("connection type: stdio")
                outputcon <- stdout()
                inputcon <- file("stdin")
                # note: windows doesn't non-blocking read stdin
                open(inputcon, blocking = FALSE)
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
            self$request_callbacks <- collections::Dict()

            self$workspace <- Workspace$new()
            self$sync_in <- collections::OrderedDictL()
            self$sync_out <- collections::OrderedDictL()
            self$reply_queue <- collections::QueueL()

            self$process_sync_in <- throttle(
                function() process_sync_in(self), 0.3
            )
            self$process_sync_out <- (function() process_sync_out(self))
        },

        finalize = function() {
            close(self$inputcon)
            self$request_callbacks <- NULL
        },

        process_events = function() {
            self$process_sync_in()
            self$process_sync_out()
            self$process_reply_queue()
        },

        text_sync = function(uri, document = NULL, run_lintr = TRUE, parse = TRUE) {
            if (self$sync_in$has(uri)) {
                # make sure we do not accidentially override list call with `parse = FALSE`
                item <- self$sync_in$pop(uri)
                parse <- parse || item$parse
                run_lintr <- run_lintr || item$run_lintr
            }
            self$sync_in$set(
                uri, list(document = document, run_lintr = run_lintr, parse = parse)
            )
        },

        process_sync_in = NULL,

        process_sync_out = NULL,

        process_reply_queue = function() {
            while (self$reply_queue$size() > 0) {
                reply <- self$reply_queue$pop()
                self$deliver(reply)
            }
        },

        check_connection = function() {
            if (!isOpen(self$inputcon)) {
                self$exit_flag <- TRUE
            }

            if (.Platform$OS.type == "unix" && become_orphan()) {
                # exit if the current process becomes orphan
                self$exit_flag <- TRUE
            }
        },

        write_text = function(text) {
            cat(text, file = self$outputcon)
        },

        read_line = function() {
            if (self$tcp) {
                readLines(self$inputcon, n = 1)
            } else {
                stdin_read_line()
            }
        },

        read_char = function(n) {
            if (self$tcp) {
                readChar(self$inputcon, n, useBytes = TRUE)
            } else {
                stdin_read_char(n)
            }
        },

        read_header = function() {
            if (self$tcp && !socketSelect(list(self$inputcon), timeout = 0)) {
                  return(NULL)
              }
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
                    matches <- stringr::str_match(header, "Content-Length: ([0-9]+)")
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

        run = function() {
            while (TRUE) {
                ret <- try({
                        self$check_connection()

                        if (isTRUE(self$exit_flag)) {
                            logger$info("exiting")
                            break
                        }

                        self$process_events()

                        data <- self$fetch(blocking = FALSE)
                        if (is.null(data)) {
                            Sys.sleep(0.1)
                            next
                        }
                        self$handle_raw(data)
                    },
                    silent = TRUE)
                if (inherits(ret, "try-error")) {
                    logger$error(ret)
                    logger$error(as.list(traceback()))
                    logger$error("exiting")
                    break
                }
            }
        }
    )
)

LanguageServer$set("public", "register_handlers", function() {
    self$request_handlers <- list(
        initialize = on_initialize,
        shutdown = on_shutdown,
        `textDocument/completion` = text_document_completion,
        `textDocument/definition` = text_document_definition,
        `textDocument/hover` = text_document_hover,
        `textDocument/signatureHelp` = text_document_signature_help,
        `textDocument/formatting` = text_document_formatting,
        `textDocument/rangeFormatting` = text_document_range_formatting,
        `textDocument/documentSymbol` = text_document_document_symbol,
        `workspace/symbol` = workspace_symbol
    )

    self$notification_handlers <- list(
        initialized = on_initialized,
        exit = on_exit,
        `textDocument/didOpen` = text_document_did_open,
        `textDocument/didChange` = text_document_did_change,
        `textDocument/didSave` = text_document_did_save,
        `textDocument/didClose` = text_document_did_close,
        `workspace/didChangeConfiguration` = workspace_did_change_configuration
    )
})


#' Run the R language server
#' @param debug set \code{TRUE} to show debug information in stderr;
#'              or it could be a character string specifying the log file
#' @param host the hostname used to create the tcp server, not used when \code{port} is \code{NULL}
#' @param port the port used to create the tcp server. If \code{NULL}, use stdio instead.
#' @examples
#' \dontrun{
#' # to use stdio
#' languageserver::run()
#'
#' # to use tcp server
#' languageserver::run(port = 8888)
#' }
#' @export
run <- function(debug = FALSE, host = "localhost", port = NULL) {
    tools::Rd2txt_options(underline_titles = FALSE)
    tools::Rd2txt_options(itemBullet = "* ")
    logger$debug_mode(debug)
    langserver <- LanguageServer$new(host, port)
    langserver$run()
}
