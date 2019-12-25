#' @useDynLib languageserver
#' @importFrom R6 R6Class
#' @import xml2
#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"

#' The language server
#'
#' Describe the language server and how it interacts with clients.
#' @keywords internal
LanguageServer <- R6::R6Class("LanguageServer",
    inherit = LanguageBase,
    public = list(
        tcp = FALSE,
        inputcon = NULL,
        outputcon = NULL,
        exit_flag = NULL,

        documents = new.env(parent = .GlobalEnv),
        workspace = NULL,

        run_lintr = TRUE,

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        ClientCapabilities = NULL,

        diagnostics_task_manager = NULL,
        parse_task_manager = NULL,
        resolve_task_manager = NULL,

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

            self$workspace <- Workspace$new()

            self$diagnostics_task_manager <- TaskManager$new()
            self$parse_task_manager <- TaskManager$new()
            self$resolve_task_manager <- TaskManager$new()

            super$initialize()
        },

        finalize = function() {
            close(self$inputcon)
            super$finalize()
        },

        process_events = function() {
            self$diagnostics_task_manager$run_tasks()
            self$diagnostics_task_manager$check_tasks()
            self$parse_task_manager$run_tasks()
            self$parse_task_manager$check_tasks()
            self$resolve_task_manager$run_tasks()
            self$resolve_task_manager$check_tasks()
        },

        text_sync = function(
                uri, document = NULL, run_lintr = FALSE, parse = FALSE, resolve = FALSE) {
            if (run_lintr && self$run_lintr) {
                self$diagnostics_task_manager$add_task(
                    uri,
                    diagnostics_task(self, uri, document)
                )
            }
            if (resolve) {
                self$resolve_task_manager$add_task(
                    uri,
                    parse_task(self, uri, document, resolve = TRUE)
                )
            } else if (parse) {
                self$parse_task_manager$add_task(
                    uri,
                    parse_task(self, uri, document, resolve = FALSE)
                )
            }
        },

        check_connection = function() {
            if (!isOpen(self$inputcon)) {
                self$exit_flag <- TRUE
            }

            if (.Platform$OS.type == "unix" && process_is_detached()) {
                # exit if the current process becomes orphan
                self$exit_flag <- TRUE
            }
        },

        write_text = function(text) {
            cat(text, file = self$outputcon)
        },

        read_line = function() {
            if (self$tcp) {
                if (socketSelect(list(self$inputcon), timeout = 0)) {
                    readLines(self$inputcon, n = 1)
                } else {
                    character(0)
                }
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

        run = function() {
            while (TRUE) {
                ret <- try({
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
                silent = TRUE
                )
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
        `completionItem/resolve` = completion_item_resolve,
        `textDocument/definition` = text_document_definition,
        `textDocument/hover` = text_document_hover,
        `textDocument/signatureHelp` = text_document_signature_help,
        `textDocument/formatting` = text_document_formatting,
        `textDocument/rangeFormatting` = text_document_range_formatting,
        `textDocument/documentSymbol` = text_document_document_symbol,
        `textDocument/documentHighlight` = text_document_document_highlight,
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
