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
        ready_reply_uris = NULL,
        ready_reply_methods = NULL,
        workspace_document_queue = NULL,
        workspace_document_keys = NULL,
        workspace_progress = NULL,
        diagnostics_workspaces = NULL,
        parse_retries = NULL,
        queue_workspace_document = function(workspace, uri) {
            document <- workspace$documents$get(uri, NULL)
            if (!is.null(document)) {
                if (isTRUE(document$is_open) ||
                        (!is.null(workspace$index) && workspace$index$parse_failed(uri)) ||
                        (!is.null(workspace$index) && workspace$index$document_pending(uri))) {
                    return(invisible(NULL))
                }
                summary <- if (is.null(workspace$index)) NULL else
                    workspace$index$summaries$get(index_canonical_uri(uri), NULL)
                if (!is.null(document$parse_data) &&
                        identical(document$content, document$parsed_content) &&
                        identical(document$version, document$parse_data$version) &&
                        (is.null(summary) || identical(document$index_content_hash, summary$content_hash))) {
                    return(invisible(NULL))
                }
            }
            if (is.null(private$workspace_document_queue)) {
                private$workspace_document_queue <- collections::queue()
                private$workspace_document_keys <- collections::dict()
            }
            key <- paste(workspace$root, uri)
            previous <- private$workspace_document_keys$get(key, NULL)
            if (!identical(previous$workspace, workspace) || !identical(previous$index, workspace$index)) {
                entry <- list(workspace = workspace, index = workspace$index, uri = uri, key = key,
                    reload = !is.null(document))
                private$workspace_document_keys$set(key, entry)
                private$workspace_document_queue$push(entry)
                if (!is.null(workspace$index)) workspace$index$queue_document(uri)
            }
            invisible(NULL)
        },
        send_workspace_progress = function(state, value) {
            self$deliver(Notification$new("$/progress", list(token = state$token, value = value)))
            invisible(NULL)
        },
        start_workspace_progress = function(workspace) {
            if (!isTRUE(self$ClientCapabilities$window$workDoneProgress)) return(invisible(NULL))
            if (is.null(private$workspace_progress)) private$workspace_progress <- collections::dict()
            self$end_workspace_progress(workspace, "Indexing restarted")
            state <- new.env(parent = emptyenv())
            state$workspace <- workspace
            state$index <- workspace$index
            state$token <- paste0("r-workspace-index-", self$get_ticket())
            state$active <- FALSE
            state$ended <- FALSE
            state$last_report <- -Inf
            state$last_value <- NULL
            state$percentage <- 0L
            state$end_message <- NULL
            private$workspace_progress$set(workspace$root, state)
            self$deliver(self$request("window/workDoneProgress/create", list(token = state$token)),
                callback = function(self, result, error = NULL) {
                    if (!is.null(error)) {
                        state$ended <- TRUE
                        if (identical(private$workspace_progress$get(workspace$root, NULL), state)) {
                            private$workspace_progress$remove(workspace$root)
                        }
                        return(invisible(NULL))
                    }
                    state$active <- TRUE
                    private$send_workspace_progress(state, list(kind = "begin",
                            title = paste0("Indexing R workspace: ", basename(workspace$root)),
                            cancellable = FALSE, message = "Discovering R files"))
                    if (state$ended) {
                        private$send_workspace_progress(state, list(kind = "end", message = state$end_message))
                    }
                })
            invisible(NULL)
        },
        transfer_document = function(source, target, uri) {
            doc <- source$documents$get(uri)
            source$documents$remove(uri)
            target$documents$set(uri, doc)
            if (!is.null(source$index) && isTRUE(source$index$enabled)) {
                source$index$remove(uri)
                if (file.exists(path_from_uri(uri))) source$index$queue_path(path_from_uri(uri))
            }
            for (workspace in list(source, target)) {
                workspace$diagnostics_globals_cache <- NULL
                workspace$type_hierarchy_cache$clear()
                workspace$update_loaded_packages()
            }
            parsed <- doc$parse_data
            if (!is.null(parsed) && identical(parsed$version, doc$version) &&
                    identical(doc$parsed_content, doc$content)) {
                target$update_parse_data(uri, parsed)
                if (!isTRUE(parsed$parse_error) && !is.null(self$resolve_task_manager)) {
                    doc$requested_packages <- member_package_request(parsed)
                    self$resolve_task_manager$add_task(uri,
                        resolve_task(self, uri, doc, normalize_package_request(parsed$packages),
                            namespace_packages = parsed$member_data$packages))
                }
            }
        },
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
        workspace_cache = NULL,
        processId = NULL,
        rootUri = NULL,
        rootPath = NULL,
        initializationOptions = NULL,
        ClientCapabilities = NULL,
        ServerCapabilities = NULL,
        diagnostics_task_manager = NULL,
        parse_task_manager = NULL,
        resolve_task_manager = NULL,
        formatting_task_manager = NULL,
        index_task_manager = NULL,
        formatting_requests = NULL,
        backend_generations = NULL,
        backend_documents = NULL,
        backend_messages = NULL,
        backend_poll_time = -Inf,
        backend_poll_queue = NULL,
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
                inputcon <- socketConnection(host = host, port = port, open = "r+b", blocking = FALSE)
                logger$info("connected")
                outputcon <- inputcon
            }

            self$inputcon <- inputcon
            self$outputcon <- outputcon

            self$parse_task_manager <- TaskManager$new(
                "parse",
                use_session = TRUE, process_recent_first = TRUE,
                cpu_load = 0.5, max_running_tasks = 4,
                cancellation_grace = 0.1,
                session_init = quote(loadNamespace("languageserver"))
            )
            self$diagnostics_task_manager <- TaskManager$new(
                "diagnostics",
                use_session = TRUE, process_recent_first = TRUE,
                cpu_load = 0.25, max_running_tasks = 2
            )

            # no pool for resolve task
            # resolve task require a new session for every task
            self$resolve_task_manager <- TaskManager$new("resolve")

            self$formatting_task_manager <- TaskManager$new(
                "formatting", use_session = TRUE,
                max_running_tasks = 1, min_idle_sessions = 0
            )
            self$index_task_manager <- TaskManager$new(
                "index", use_session = TRUE,
                max_running_tasks = 1, min_idle_sessions = 0
            )
            self$formatting_requests <- collections::dict()

            self$pending_replies <- collections::dict()
            self$workspaces <- collections::dict()
            self$workspace_cache <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, Workspace$new(NULL))

            super$initialize()
        },
        process_events = function() {
            self$parse_task_manager$check_tasks()
            self$diagnostics_task_manager$check_tasks()
            self$resolve_task_manager$check_tasks()
            self$formatting_task_manager$check_tasks()
            if (!is.null(self$index_task_manager)) self$index_task_manager$check_tasks()
            self$process_parse_retries()
            self$process_pending_replies()
            # Start latency-sensitive parse work before diagnostics.
            self$parse_task_manager$run_tasks()
            for (workspace in self$workspaces$values()) {
                if (!is.null(workspace$index) && isTRUE(workspace$index$enabled)) {
                    # Disk parsing and cache compression use one bounded worker
                    # and can progress while the parse pool handles edits.
                    if (!is.null(self$index_task_manager) ||
                            !self$parse_task_manager$has_work()) {
                        workspace$index$process_batch()
                    }
                }
            }
            self$process_workspace_documents()
            if (!is.null(self$index_task_manager)) self$index_task_manager$run_tasks()
            self$diagnostics_task_manager$run_tasks()
            self$resolve_task_manager$run_tasks()
            self$formatting_task_manager$run_tasks()
            self$refresh_workspace_diagnostics()
            poll_backend_configuration(self)
            self$report_workspace_progress()
            for (workspace in self$workspaces$values()) {
                if (!isTRUE(workspace$poll_namespace_file(throttle = TRUE)) ||
                        !diagnostics_needs_workspace_globals()) {
                    next
                }
                for (doc in workspace$documents$values()) {
                    if (!isTRUE(doc$is_open)) {
                        next
                    }
                    schedule_diagnostics(self, doc$uri, doc, delay = 0)
                }
            }
        },
        add_workspace = function(uri) {
            key <- if (length(uri) == 0) DEFAULT_WORKSPACE else uri
            if (!self$workspaces$has(key)) {
                path <- if (length(uri) == 0) NULL else path_from_uri(uri)
                new_workspace <- Workspace$new(path)
                self$workspaces$set(key, new_workspace)
                self$workspace_cache$clear()

                # Transfer buffers whose workspace changed, including nested roots.
                if (!is.null(path)) {
                    for (workspace in self$workspaces$values()) {
                        if (identical(workspace, new_workspace)) next
                        for (doc_uri in workspace$documents$keys()) {
                            if (identical(self$get_workspace(doc_uri), new_workspace)) {
                                private$transfer_document(workspace, new_workspace, doc_uri)
                            }
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
                self$end_workspace_progress(workspace, "Workspace removed")
                if (!is.null(workspace$index)) workspace$index$detach_worker()
                for (doc_uri in workspace$documents$keys()) {
                    diagnostics_callback(self, doc_uri, NULL, list())
                }
                self$workspaces$remove(key)
                self$workspace_cache$clear()
                for (doc_uri in workspace$documents$keys()) {
                    if (isTRUE(workspace$documents$get(doc_uri)$is_open)) {
                        private$transfer_document(workspace, self$get_workspace(doc_uri), doc_uri)
                    }
                }
                self$workspace_cache$clear()
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

            if (self$workspace_cache$has(uri)) {
                return(self$workspace_cache$get(uri))
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
                self$workspace_cache$set(uri, fallback)
                return(fallback)
            }

            self$workspace_cache$set(uri, best_match)
            best_match
        },
        load_index_document = function(workspace, uri, reload = FALSE) {
            existing <- workspace$documents$get(uri, NULL)
            if (!is.null(existing) && (!reload || isTRUE(existing$is_open))) return(invisible(existing))
            path <- path_from_uri(uri)
            if (!file.exists(path) || dir.exists(path)) return(invisible(NULL))
            content <- tryCatch(stringi::stri_read_lines(path),
                error = function(e) NULL)
            if (is.null(content)) return(invisible(NULL))
            doc <- Document$new(
                uri, language = "r", version = NULL, content = content)
            workspace$documents$set(uri, doc)
            if (!is.null(workspace$index)) workspace$index$queue_document(uri)
            if (!is.null(workspace$index$task_manager)) {
                workspace$index$await_parse(uri, cacheable = TRUE)
            }
            self$text_sync(uri, document = doc, parse = TRUE)
            invisible(doc)
        },

        refresh_index_documents = function(workspace, entry_uri = NULL) {
            index <- workspace$index
            if (is.null(index) || !isTRUE(index$enabled)) return(invisible(NULL))
            if (is.null(private$diagnostics_workspaces)) private$diagnostics_workspaces <- collections::dict()
            private$diagnostics_workspaces$set(workspace$root,
                list(workspace = workspace, ready_at = proc.time()[[3L]] + 0.25))
            entries <- if (is.null(entry_uri)) {
                Filter(function(uri) {
                    doc <- workspace$documents$get(uri, NULL)
                    !is.null(doc) && isTRUE(doc$is_open)
                }, workspace$documents$keys())
            } else {
                entry_uri
            }
            for (entry in entries) {
                entry_key <- index_canonical_uri(entry)
                queue <- collections::queue()
                queue$push(entry_key)
                visited <- new.env(hash = TRUE, parent = emptyenv())
                while (queue$size()) {
                    uri <- queue$pop()
                    if (exists(uri, envir = visited, inherits = FALSE)) next
                    assign(uri, TRUE, envir = visited)
                    if (!index$summaries$has(uri)) {
                        if (is.null(index$task_manager)) index$update_path(path_from_uri(uri))
                        else index$queue_path(path_from_uri(uri), invalidate = FALSE)
                    }
                    if (!identical(uri, entry_key)) {
                        if (is.null(index$task_manager)) self$load_index_document(workspace, uri)
                        else private$queue_workspace_document(workspace, uri)
                    }
                    for (target in index$source_edges$get(uri, character())) {
                        queue$push(target)
                    }
                }
            }
            self$prune_index_documents(workspace)
            invisible(NULL)
        },

        prune_index_documents = function(workspace) {
            index <- workspace$index
            if (is.null(index) || !isTRUE(index$enabled)) return(invisible(NULL))
            document_uris <- workspace$documents$keys()
            open_uris <- Filter(function(uri) {
                isTRUE(workspace$documents$get(uri)$is_open)
            }, document_uris)
            keep <- union(index$package_source_uris(), open_uris)
            for (uri in open_uris) keep <- union(keep, index$source_closure(uri))
            keep <- unique(vapply(keep, index_canonical_uri, character(1L)))
            remove <- document_uris[!vapply(document_uris, function(uri) {
                index_canonical_uri(uri) %in% keep
            }, logical(1L))]
            remove <- Filter(function(uri) {
                index$contains_path(path_from_uri(uri))
            }, remove)
            for (uri in remove) {
                diagnostics_callback(self, uri, NULL, list())
                workspace$documents$remove(uri)
                if (index$document_pending(uri) || index$parse_failed(uri)) {
                    if (index$document_pending(uri) && !is.null(self$parse_task_manager)) {
                        self$parse_task_manager$cancel(uri)
                    }
                    index$finish_document(uri)
                    if (file.exists(path_from_uri(uri))) index$queue_path(path_from_uri(uri), force = TRUE)
                    else index$remove(uri)
                }
            }
            if (length(remove)) {
                workspace$diagnostics_globals_cache <- NULL
                workspace$type_hierarchy_cache$clear()
                workspace$update_loaded_packages()
            }
            invisible(NULL)
        },

        prune_legacy_documents = function(workspace) {
            source_dir <- if (is_package(workspace$root)) {
                index_normalize_path(file.path(workspace$root, "R"))
            } else {
                NULL
            }
            remove <- Filter(function(uri) {
                doc <- workspace$documents$get(uri)
                if (isTRUE(doc$is_open)) return(FALSE)
                path <- index_normalize_path(path_from_uri(uri))
                is.null(source_dir) || !identical(dirname(path), source_dir)
            }, workspace$documents$keys())
            for (uri in remove) {
                diagnostics_callback(self, uri, NULL, list())
                workspace$documents$remove(uri)
            }
            if (length(remove)) {
                workspace$diagnostics_globals_cache <- NULL
                workspace$type_hierarchy_cache$clear()
                workspace$update_loaded_packages()
            }
            invisible(NULL)
        },

        load_workspace = function(workspace) {
            self$end_workspace_progress(workspace, "Indexing restarted")
            if (is.null(workspace$index) || !isTRUE(workspace$index$enabled)) {
                if (is_package(workspace$root)) {
                    source_dir <- file.path(workspace$root, "R")
                    files <- list.files(
                        source_dir, pattern = "\\.r$", ignore.case = TRUE)
                    for (file in files) {
                        uri <- path_to_uri(file.path(source_dir, file))
                        if (is.null(self$index_task_manager)) self$load_index_document(workspace, uri)
                        else private$queue_workspace_document(workspace, uri)
                    }
                    workspace$import_from_namespace_file()
                }
                return(invisible(NULL))
            }
            logger$info("load workspace:", workspace$root)
            if (!is.null(self$index_task_manager)) {
                workspace$index$attach_worker(self$index_task_manager,
                    on_update = function(uris) {
                        for (uri in uris) {
                            if (!is.null(workspace$index$package_root_for_uri(uri))) {
                                private$queue_workspace_document(workspace, uri)
                            }
                        }
                        self$refresh_index_documents(workspace)
                    })
                private$start_workspace_progress(workspace)
            }
            workspace$index$discover()
            # Discovery can load disk caches or drop files that only exist in
            # the editor. Restore open buffers before loading package files.
            for (doc in workspace$documents$values()) {
                if (isTRUE(doc$is_open)) {
                    refresh_index_summary(workspace, workspace$index, doc$uri)
                }
            }
            for (uri in workspace$index$package_source_uris()) {
                logger$info("load package file:", path_from_uri(uri))
                if (!workspace$index$summaries$has(uri)) {
                    if (is.null(workspace$index$task_manager)) {
                        workspace$index$update_path(path_from_uri(uri))
                    } else {
                        workspace$index$await_parse(uri, cacheable = TRUE)
                    }
                }
                self$load_index_document(workspace, uri)
            }
            if (is_package(workspace$root)) workspace$import_from_namespace_file()
        },
        load_workspaces = function() {
            for (workspace in self$workspaces$values()) {
                self$load_workspace(workspace)
            }
        },
        queue_parse_retry = function(uri, callback) {
            if (is.null(private$parse_retries)) private$parse_retries <- collections::ordered_dict()
            private$parse_retries$set(uri, callback)
            invisible(NULL)
        },
        process_parse_retries = function(max_documents = 20L) {
            if (is.null(private$parse_retries)) return(invisible(NULL))
            for (uri in utils::head(private$parse_retries$keys(), max_documents)) {
                retry <- private$parse_retries$pop(uri)
                retry()
            }
            invisible(NULL)
        },
        process_workspace_documents = function(max_documents = 20L, time_budget_ms = 5) {
            if (is.null(private$workspace_document_queue)) return(invisible(NULL))
            started <- proc.time()[[3L]]
            processed <- 0L
            while (private$workspace_document_queue$size() && processed < max_documents) {
                entry <- private$workspace_document_queue$pop()
                workspace <- entry$workspace
                queued <- private$workspace_document_keys$get(entry$key, NULL)
                if (identical(queued, entry)) {
                    private$workspace_document_keys$remove(entry$key)
                }
                current <- identical(entry$index, workspace$index) &&
                    any(vapply(self$workspaces$values(), identical, logical(1L), y = workspace))
                if (current && (is.null(workspace$index) || !isTRUE(workspace$index$enabled) ||
                            workspace$index$should_index(path_from_uri(entry$uri)))) {
                    loaded <- self$load_index_document(workspace, entry$uri, reload = entry$reload)
                    if (!workspace$documents$has(entry$uri) && !is.null(workspace$index)) {
                        workspace$index$remove(entry$uri)
                    } else if (is.null(loaded) && !is.null(workspace$index) &&
                            !isTRUE(workspace$documents$get(entry$uri)$is_open)) {
                        workspace$index$finish_document(entry$uri)
                    }
                } else if (!is.null(entry$index)) {
                    entry$index$finish_document(entry$uri)
                }
                processed <- processed + 1L
                if ((proc.time()[[3L]] - started) * 1000 >= time_budget_ms) break
            }
            invisible(NULL)
        },
        end_workspace_progress = function(workspace, message = "Indexing complete") {
            if (is.null(private$workspace_progress)) return(invisible(NULL))
            state <- private$workspace_progress$get(workspace$root, NULL)
            if (is.null(state) || state$ended || !identical(state$workspace, workspace)) return(invisible(NULL))
            private$workspace_progress$remove(workspace$root)
            state$ended <- TRUE
            state$end_message <- message
            if (state$active) private$send_workspace_progress(state, list(kind = "end", message = message))
            invisible(NULL)
        },
        refresh_workspace_diagnostics = function(now = proc.time()[[3L]]) {
            if (is.null(private$diagnostics_workspaces)) return(invisible(NULL))
            for (root in private$diagnostics_workspaces$keys()) {
                entry <- private$diagnostics_workspaces$get(root)
                workspace <- entry$workspace
                current <- any(vapply(self$workspaces$values(), identical, logical(1L), y = workspace))
                if (!current || !lsp_settings$get("diagnostics") ||
                        !diagnostics_needs_workspace_globals()) {
                    private$diagnostics_workspaces$remove(root)
                    next
                }
                if (now < entry$ready_at || !isTRUE(workspace$index$indexing_status()$done)) next
                # Coalesce the whole indexing burst, then refresh only documents
                # whose package or source globals changed since their last lint.
                private$diagnostics_workspaces$remove(root)
                for (document in workspace$documents$values()) {
                    if (!isTRUE(document$is_open) ||
                            !identical(document$content, document$parsed_content) ||
                            !identical(document$version, document$parse_data$version)) next
                    generation <- diagnostics_workspace_generation(workspace, document$uri)
                    if (identical(document$diagnostics_generation, generation)) next
                    schedule_diagnostics(self, document$uri, document,
                        delay = lsp_settings$get("diagnostics_delay"))
                }
            }
            invisible(NULL)
        },
        report_workspace_progress = function(now = proc.time()[[3L]], interval = 0.25) {
            if (is.null(private$workspace_progress)) return(invisible(NULL))
            for (state in private$workspace_progress$values()) {
                if (!state$active || state$ended || now - state$last_report < interval) next
                workspace <- state$workspace
                if (!identical(workspace$index, state$index)) {
                    self$end_workspace_progress(workspace, "Indexing restarted")
                    next
                }
                queued <- if (is.null(private$workspace_document_keys)) list() else
                    Filter(function(entry) identical(entry$workspace, workspace),
                        private$workspace_document_keys$values())
                status <- state$index$indexing_status(vapply(queued, `[[`, character(1L), "uri"))
                state$last_report <- now
                if (status$done) {
                    message <- if (status$failed) {
                        sprintf("Indexing finished (%d R files failed)", status$failed)
                    } else if (status$truncated) {
                        sprintf("Indexed %d R files (file limit reached)", status$total)
                    } else {
                        sprintf("Indexed %d R files", status$total)
                    }
                    self$end_workspace_progress(workspace, message)
                    next
                }
                value <- list(kind = "report", cancellable = FALSE)
                if (status$discovering) {
                    value$message <- if (status$total) sprintf("Discovered %d R files", status$total) else
                        "Discovering R files"
                } else {
                    state$percentage <- max(state$percentage, min(99L,
                            as.integer(floor(100 * status$completed / max(status$total, 1L)))))
                    value$percentage <- state$percentage
                    value$message <- sprintf("Indexed %d of %d R files", status$completed, status$total)
                }
                if (!identical(value, state$last_value)) {
                    private$send_workspace_progress(state, value)
                    state$last_value <- value
                }
            }
            invisible(NULL)
        },
        text_sync = function(uri, document, run_diagnostics = FALSE, parse = FALSE,
            delay = 0, parse_delay = delay,
            diagnostics_delay = delay) {
            if (!self$pending_replies$has(uri)) {
                self$pending_replies$set(uri, list(
                    `textDocument/completion` = collections::queue(),
                    `textDocument/hover` = collections::queue(),
                    `textDocument/signatureHelp` = collections::queue(),
                    `textDocument/documentSymbol` = collections::queue(),
                    `textDocument/foldingRange` = collections::queue(),
                    `textDocument/documentLink` = collections::queue(),
                    `textDocument/documentColor` = collections::queue(),
                    `textDocument/codeLens` = collections::queue(),
                    `textDocument/linkedEditingRange` = collections::queue(),
                    `textDocument/inlineValue` = collections::queue(),
                    `textDocument/inlayHint` = collections::queue(),
                    `textDocument/semanticTokens/full` = collections::queue(),
                    `textDocument/semanticTokens/full/delta` = collections::queue(),
                    `textDocument/semanticTokens/range` = collections::queue()
                ))
            }

            if (run_diagnostics && lsp_settings$get("diagnostics")) {
                if (parse) {
                    self$diagnostics_task_manager$cancel(uri)
                    document$pending_diagnostics <- TRUE
                    document$diagnostics_delay <- diagnostics_delay
                } else {
                    schedule_diagnostics(
                        self, uri, document, delay = diagnostics_delay)
                }
            }

            if (parse) {
                self$parse_task_manager$add_task(
                    uri,
                    parse_task(self, uri, document, delay = parse_delay)
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
        read_raw = function(n) {
            self$check_connection()
            if (isTRUE(self$exit_flag)) return(structure(raw(), eof = TRUE))
            if (self$tcp) {
                if (!socketSelect(list(self$inputcon), timeout = 0)) return(raw())
                data <- readBin(self$inputcon, what = "raw", n = n)
                if (!length(data)) attr(data, "eof") <- TRUE
                data
            } else {
                .Call("stdin_read_raw", as.integer(n), PACKAGE = "languageserver")
            }
        },
        wait_input = function(timeout) {
            if (!isOpen(self$inputcon)) return(TRUE)
            if (self$tcp) {
                socketSelect(list(self$inputcon), timeout = timeout)[[1L]]
            } else {
                .Call("stdin_wait", timeout * 1000, PACKAGE = "languageserver")
            }
        },
        on_input_eof = function() {
            self$exit_flag <- TRUE
            invisible(NULL)
        },
        input_wait_timeout = function(max_timeout = 0.01) {
            if (!is.null(private$ready_reply_uris) && private$ready_reply_uris$size()) return(0)
            timeout <- max_timeout
            for (manager in list(self$parse_task_manager, self$diagnostics_task_manager,
                    self$resolve_task_manager, self$formatting_task_manager, self$index_task_manager)) {
                if (is.function(manager$wait_timeout)) timeout <- min(timeout, manager$wait_timeout(max_timeout))
            }
            timeout
        },
        queue_pending_replies = function(uri) {
            if (is.null(private$ready_reply_uris)) {
                private$ready_reply_uris <- collections::queue()
                private$ready_reply_methods <- collections::dict()
            }
            if (!private$ready_reply_methods$has(uri)) {
                private$ready_reply_methods$set(uri, 1L)
                private$ready_reply_uris$push(uri)
            }
            invisible(NULL)
        },
        process_pending_replies = function(max_replies = 20L, time_budget_ms = 5) {
            processed <- 0L
            if (is.null(private$ready_reply_uris)) return(processed)
            started <- proc.time()[[3L]]
            while (private$ready_reply_uris$size() && processed < max_replies) {
                uri <- private$ready_reply_uris$pop()
                first <- private$ready_reply_methods$pop(uri)
                document <- self$get_workspace(uri)$documents$get(uri, NULL)
                if (!is.null(document$parse_data) &&
                        identical(document$version, document$parse_data$version) &&
                        identical(document$content, document$parsed_content)) {
                    result <- drain_pending_replies(self, uri, document$version, 1L, first)
                    processed <- processed + result$processed
                    if (result$ready) {
                        private$ready_reply_methods$set(uri, result$first)
                        private$ready_reply_uris$push(uri)
                    }
                }
                if ((proc.time()[[3L]] - started) * 1000 >= time_budget_ms) break
            }
            processed
        },
        process_input = function(max_messages = 20L) {
            processed <- 0L
            # Apply queued edits and serve their requests before installing
            # background results. Limit the batch so parsing and diagnostics
            # also progress while a client is sending a stream of messages.
            while (processed < max_messages && !isTRUE(self$exit_flag)) {
                data <- self$fetch(blocking = FALSE)
                if (is.null(data)) break
                self$handle_raw(data)
                processed <- processed + 1L
            }
            processed
        },
        run = function() {
            on.exit(
                {
                    if (!is.null(self$parse_task_manager)) self$parse_task_manager$stop()
                    if (!is.null(self$diagnostics_task_manager)) self$diagnostics_task_manager$stop()
                    if (!is.null(self$resolve_task_manager)) self$resolve_task_manager$stop()
                    if (!is.null(self$formatting_task_manager)) self$formatting_task_manager$stop()
                    if (!is.null(self$index_task_manager)) self$index_task_manager$stop()
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

                        processed <- self$process_input()
                        if (isTRUE(self$exit_flag)) next
                        self$process_events()
                        if (!processed && !(is.function(self$input_read_pending) && self$input_read_pending())) {
                            timeout <- self$input_wait_timeout()
                            if (timeout > 0) self$wait_input(timeout)
                        }
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
        `textDocument/rangesFormatting` = text_document_ranges_formatting,
        `textDocument/onTypeFormatting` = text_document_on_type_formatting,
        `textDocument/documentSymbol` = text_document_document_symbol,
        `textDocument/documentHighlight` = text_document_document_highlight,
        `textDocument/documentLink` = text_document_document_link,
        `documentLink/resolve` = document_link_resolve,
        `textDocument/documentColor` = text_document_document_color,
        `textDocument/codeAction` = text_document_code_action,
        `textDocument/codeLens` = text_document_code_lens,
        `codeLens/resolve` = code_lens_resolve,
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
        `textDocument/inlineValue` = text_document_inline_value,
        `textDocument/inlayHint` = text_document_inlay_hint,
        `inlayHint/resolve` = inlay_hint_resolve,
        `textDocument/semanticTokens/full` = text_document_semantic_tokens_full,
        `textDocument/semanticTokens/full/delta` = text_document_semantic_tokens_delta,
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
        `$/cancelRequest` = cancel_request,
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
