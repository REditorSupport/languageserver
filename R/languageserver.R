#' @useDynLib languageserver
#' @importFrom R6 R6Class
#' @import xml2
#' @importFrom parallel detectCores
#' @details
#' An implementation of the Language Server Protocol for R
"_PACKAGE"

DEFAULT_WORKSPACE <- "_default_"

#' The language server
#'
#' Describe the language server and how it interacts with clients.
#' @noRd
LanguageServer <- R6::R6Class("LanguageServer",
    inherit = LanguageBase,
    private = list(
        finalize = function() {
            close(self$inputcon)
            super$finalize()
        }
    ),
    public = list(
        tcp = FALSE,
        inputcon = NULL,
        outputcon = NULL,
        exit_flag = NULL,
        documents = NULL,
        workspaces = NULL,
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

            self$parse_task_manager <- TaskManager$new(
                "parse",
                use_session = TRUE, process_recent_first = TRUE
            )
            self$diagnostics_task_manager <- TaskManager$new(
                "diagnostics",
                use_session = TRUE, process_recent_first = TRUE
            )

            # no pool for resolve task
            # resolve task require a new session for every task
            self$resolve_task_manager <- TaskManager$new("resolve")

            self$pending_replies <- collections::dict()
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, Workspace$new(NULL))

            super$initialize()
        },
        process_events = function() {
            self$diagnostics_task_manager$run_tasks()
            self$diagnostics_task_manager$check_tasks()
            self$parse_task_manager$run_tasks()
            self$parse_task_manager$check_tasks()
            self$resolve_task_manager$run_tasks()
            self$resolve_task_manager$check_tasks()
            for (workspace in self$workspaces$values()) {
                workspace$poll_namespace_file()
            }
        },
        add_workspace = function(uri) {
            key <- if (length(uri) == 0) DEFAULT_WORKSPACE else uri
            if (!self$workspaces$has(key)) {
                path <- if (length(uri) == 0) NULL else path_from_uri(uri)
                new_workspace <- Workspace$new(path)
                self$workspaces$set(key, new_workspace)

                # Remove documents from the fallback workspace if they belong here
                if (!is.null(path)) {
                    fallback <- self$workspaces$get(DEFAULT_WORKSPACE)
                    for (doc_uri in fallback$documents$keys()) {
                        doc_path <- path_from_uri(doc_uri)
                        if (path_has_parent(doc_path, path)) {
                            fallback$documents$remove(doc_uri)
                        }
                    }
                }
            }
        },
        remove_workspace = function(uri) {
            key <- if (length(uri) == 0) DEFAULT_WORKSPACE else uri
            if (key == DEFAULT_WORKSPACE) {
                return(invisible(NULL))
            }
            if (self$workspaces$has(key)) {
                workspace <- self$workspaces$get(key)
                for (doc_uri in workspace$documents$keys()) {
                    diagnostics_callback(self, doc_uri, NULL, list())
                    doc <- workspace$documents$get(doc_uri)
                    if (isTRUE(doc$is_open)) {
                        self$workspaces$get(DEFAULT_WORKSPACE)$documents$set(doc_uri, doc)
                    }
                }
                self$workspaces$remove(key)
            }
        },
        get_workspace = function(uri) {
            # Find the best matching workspace for a given URI.
            # If no match is found, or if URI is empty, return the default workspace.

            # Determine the default workspace to use as a fallback
            default_key <- if (length(self$rootUri) == 0) DEFAULT_WORKSPACE else self$rootUri
            fallback <- self$workspaces$get(default_key, self$workspaces$get(DEFAULT_WORKSPACE))

            if (length(uri) == 0) {
                return(fallback)
            }

            path <- path_from_uri(uri)
            best_match <- NULL
            max_len <- -1

            for (workspace in self$workspaces$values()) {
                root <- workspace$root
                if (length(root) > 0 && path_has_parent(path, root)) {
                    root_len <- nchar(root)
                    if (root_len > max_len) {
                        max_len <- root_len
                        best_match <- workspace
                    }
                }
            }

            if (is.null(best_match)) {
                return(fallback)
            }

            best_match
        },
        load_workspace = function(workspace) {
            if (!is_package(workspace$root)) {
                return(invisible(NULL))
            }
            logger$info("load workspace:", workspace$root)
            source_dir <- file.path(workspace$root, "R")
            files <- list.files(source_dir, pattern = "\\.r$", ignore.case = TRUE)
            for (f in files) {
                logger$info("load file:", f)
                path <- file.path(source_dir, f)
                uri <- path_to_uri(path)
                doc <- Document$new(uri, language = "r", version = NULL, content = stringi::stri_read_lines(path))
                workspace$documents$set(uri, doc)
                self$text_sync(uri, document = doc, parse = TRUE)
            }
            workspace$import_from_namespace_file()
        },
        load_workspaces = function() {
            for (workspace in self$workspaces$values()) {
                self$load_workspace(workspace)
            }
        },
        text_sync = function(uri, document, run_lintr = FALSE, parse = FALSE, delay = 0) {
            if (!self$pending_replies$has(uri)) {
                self$pending_replies$set(uri, list(
                    `textDocument/documentSymbol` = collections::queue(),
                    `textDocument/foldingRange` = collections::queue(),
                    `textDocument/documentLink` = collections::queue(),
                    `textDocument/documentColor` = collections::queue(),
                    `textDocument/semanticTokens/full` = collections::queue(),
                    `textDocument/semanticTokens/range` = collections::queue()
                ))
            }

            if (run_lintr && lsp_settings$get("diagnostics")) {
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
            # we have made effort to ensure that text is utf-8
            # so text is printed as is
            writeLines(text, self$outputcon, sep = "", useBytes = TRUE)
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
            on.exit(
                {
                    if (!is.null(self$parse_task_manager)) self$parse_task_manager$stop()
                    if (!is.null(self$diagnostics_task_manager)) self$diagnostics_task_manager$stop()
                    if (!is.null(self$resolve_task_manager)) self$resolve_task_manager$stop()
                },
                add = TRUE
            )
            while (TRUE) {
                ret <- tryCatchStack(
                    {
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
                    error = function(e) e
                )
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
        `documentLink/resolve` = document_link_resolve,
        `textDocument/documentColor` = text_document_document_color,
        `textDocument/codeAction` = text_document_code_action,
        `textDocument/colorPresentation` = text_document_color_presentation,
        `textDocument/foldingRange` = text_document_folding_range,
        `textDocument/references` = text_document_references,
        `textDocument/rename` = text_document_rename,
        `textDocument/prepareRename` = text_document_prepare_rename,
        `textDocument/selectionRange` = text_document_selection_range,
        `textDocument/prepareCallHierarchy` = text_document_prepare_call_hierarchy,
        `callHierarchy/incomingCalls` = call_hierarchy_incoming_calls,
        `callHierarchy/outgoingCalls` = call_hierarchy_outgoing_calls,
        `textDocument/prepareTypeHierarchy` = text_document_prepare_type_hierarchy,
        `typeHierarchy/supertypes` = type_hierarchy_supertypes,
        `typeHierarchy/subtypes` = type_hierarchy_subtypes,
        `textDocument/linkedEditingRange` = text_document_linked_editing_range,
        `textDocument/semanticTokens/full` = text_document_semantic_tokens_full,
        `textDocument/semanticTokens/range` = text_document_semantic_tokens_range,
        `workspace/symbol` = workspace_symbol
    )

    self$notification_handlers <- list(
        initialized = on_initialized,
        exit = on_exit,
        `textDocument/didOpen` = text_document_did_open,
        `textDocument/didChange` = text_document_did_change,
        `textDocument/didSave` = text_document_did_save,
        `textDocument/didClose` = text_document_did_close,
        `workspace/didChangeConfiguration` = workspace_did_change_configuration,
        `workspace/didChangeWatchedFiles` = workspace_did_change_watched_files,
        `workspace/didChangeWorkspaceFolders` = workspace_did_change_workspace_folders,
        `$/setTrace` = protocol_set_trace
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
    lsp_settings$update_from_options()
    if (isTRUE(debug)) {
        lsp_settings$set("debug", TRUE)
        lsp_settings$set("log_file", NULL)
    } else if (is.character(debug)) {
        lsp_settings$set("debug", TRUE)
        lsp_settings$set("log_file", debug)
    }
    langserver <- LanguageServer$new(host, port)
    langserver$run()
}
