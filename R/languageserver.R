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

        documents = NULL,
        workspace = NULL,

        run_lintr = TRUE,

        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,

        diagnostics_task_manager = NULL,
        parse_task_manager = NULL,
        resolve_task_manager = NULL,

        pending_replies = NULL,

        initialize = function(host, port) {
            if (is.null(port)) {
                logger$info("connection type: stdio")
                outputcon <- stdout()
                # note: windows doesn't support `blocking = FALSE`
                # we use `PeekNamedPipe` in c to mimic non-blocking reading
                inputcon <- file("stdin", open = "rb", blocking = FALSE)
            } else {
                self$tcp <- TRUE
                logger$info("connection type: tcp at ", port)
                inputcon <- socketConnection(host = host, port = port, open = "r+b")
                logger$info("connected")
                outputcon <- inputcon
            }

            self$inputcon <- inputcon
            self$outputcon <- outputcon

            cpus <- parallel::detectCores()
            pool_size <- as.integer(
                Sys.getenv("R_LANGSVR_POOL_SIZE", min(max(floor(cpus / 2), 1), 3)))

            # parse pool
            parse_pool <- if (pool_size > 0) SessionPool$new(pool_size, "parse") else NULL
            # diagnostics is slower, so use a separate pool
            diagnostics_pool <- if (pool_size > 0) SessionPool$new(pool_size, "diagnostics") else NULL

            self$parse_task_manager <- TaskManager$new("parse", parse_pool)
            self$diagnostics_task_manager <- TaskManager$new("diagnostics", diagnostics_pool)

            # no pool for resolve task
            # resolve task require a new session for every task
            self$resolve_task_manager <- TaskManager$new("resolve", NULL)

            self$pending_replies <- collections::dict()

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
            if (length(self$rootPath) && !is.null(self$workspace)) {
                self$workspace$poll_namespace_file()
            }
        },

        text_sync = function(
            # TODO: move it to Workspace!?
                uri, document, run_lintr = FALSE, parse = FALSE, delay = 0) {

            if (!self$pending_replies$has(uri)) {
                self$pending_replies$set(uri, list(
                    `textDocument/documentSymbol` = collections::queue(),
                    `textDocument/foldingRange` = collections::queue(),
                    `textDocument/documentLink` = collections::queue(),
                    `textDocument/documentColor` = collections::queue()
                ))
            }

            if (run_lintr && self$run_lintr) {
                temp_root <- dirname(tempdir())
                if (path_has_parent(self$rootPath, temp_root) ||
                    !path_has_parent(path_from_uri(uri), temp_root)) {
                    self$diagnostics_task_manager$add_task(
                        uri,
                        diagnostics_task(self, uri, document, delay = delay)
                    )
                }
            }

            if (parse) {
                self$parse_task_manager$add_task(
                    uri,
                    parse_task(self, uri, document, delay = delay)
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
            if (.Platform$OS.type == "windows") {
                writeLines(text, self$outputcon, sep = "", useBytes = TRUE)
            } else  {
                cat(text, file = self$outputcon)
            }
        },

        read_line = function() {
            if (self$tcp) {
                if (socketSelect(list(self$inputcon), timeout = 0)) {
                    readLines(self$inputcon, n = 1, encoding = "UTF-8")
                } else {
                    character(0)
                }
            } else {
                stdin_read_line()
            }
        },

        read_char = function(n) {
            if (self$tcp) {
                out <- readChar(self$inputcon, n, useBytes = TRUE)
                Encoding(out) <- "UTF-8"
                out
            } else {
                stdin_read_char(n)
            }
        },

        run = function() {
            while (TRUE) {
                ret <- tryCatchStack({
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
                }, error = function(e) e)
                if (inherits(ret, "error")) {
                    logger$error(ret)
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
        `textDocument/onTypeFormatting` = text_document_on_type_formatting,
        `textDocument/documentSymbol` = text_document_document_symbol,
        `textDocument/documentHighlight` = text_document_document_highlight,
        `textDocument/documentLink` = text_document_document_link,
        `textDocument/documentColor` = text_document_document_color,
        `textDocument/colorPresentation` = text_document_color_presentation,
        `textDocument/foldingRange` = text_document_folding_range,
        `textDocument/references` = text_document_references,
        `textDocument/rename` = text_document_rename,
        `textDocument/prepareRename` = text_document_prepare_rename,
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
#' @param debug set `TRUE` to show debug information in stderr;
#'              or it could be a character string specifying the log file
#' @param host the hostname used to create the tcp server, not used when `port` is `NULL`
#' @param port the port used to create the tcp server. If `NULL`, use stdio instead.
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
