#' Convert a workspace-index glob to a regular expression
#' @noRd
index_glob_regex <- function(pattern) {
    pattern <- gsub("\\", "/", pattern, fixed = TRUE)
    output <- character()
    i <- 1L
    n <- nchar(pattern)
    while (i <= n) {
        remaining <- substr(pattern, i, n)
        if (startsWith(remaining, "**/")) {
            output <- c(output, "(?:.*/)?")
            i <- i + 3L
        } else if (startsWith(remaining, "**")) {
            output <- c(output, ".*")
            i <- i + 2L
        } else {
            char <- substr(pattern, i, i)
            if (identical(char, "*")) {
                output <- c(output, "[^/]*")
            } else if (identical(char, "?")) {
                output <- c(output, "[^/]")
            } else if (char %in% strsplit(".\\+()[]{}^$|", "", fixed = TRUE)[[1L]]) {
                output <- c(output, paste0("\\", char))
            } else {
                output <- c(output, char)
            }
            i <- i + 1L
        }
    }
    paste0("^", paste0(output, collapse = ""), "$")
}

#' Match a workspace-relative path against index globs
#' @noRd
index_glob_match <- function(path, patterns, directory = FALSE, regex = NULL) {
    if (is.null(regex)) {
        patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
        regex <- vapply(patterns, index_glob_regex, character(1L))
    }
    if (!length(regex)) return(FALSE)
    path <- gsub("\\", "/", path, fixed = TRUE)
    if (directory) path <- paste0(sub("/+$", "", path), "/")
    any(vapply(regex, function(pattern) {
        grepl(pattern, path, ignore.case = TRUE, perl = TRUE)
    }, logical(1L)))
}

#' Normalize a path without requiring it to exist
#' @noRd
index_normalize_path <- function(path) {
    path <- path.expand(path)
    absolute <- grepl("^(?:/|[[:alpha:]]:[/\\\\])", path)
    if (!absolute) path <- file.path(getwd(), path)
    if (.Platform$OS.type == "windows") {
        path <- gsub("\\", "/", path, fixed = TRUE)
    }
    path
}

#' Canonical file URI used internally by the workspace index
#' @noRd
index_canonical_uri <- function(uri) {
    path <- path_from_uri(uri)
    if (!length(path) || !nzchar(path)) return(uri)
    path_to_uri(index_normalize_path(path))
}

#' Return the nearest package root containing an R source file
#' @noRd
index_package_root <- function(path, workspace_root = NULL) {
    if (!length(path) || !nzchar(path)) return(NULL)
    path <- index_normalize_path(path)
    workspace_root <- if (length(workspace_root) && nzchar(workspace_root)) {
        index_normalize_path(workspace_root)
    } else {
        NULL
    }
    current <- dirname(path)
    repeat {
        source_dir <- file.path(current, "R")
        if (file.exists(file.path(current, "DESCRIPTION")) &&
                path_has_parent(path, source_dir)) {
            return(current)
        }
        if (!is.null(workspace_root) && identical(current, workspace_root)) break
        parent <- dirname(current)
        if (identical(parent, current)) break
        if (!is.null(workspace_root) && !path_has_parent(parent, workspace_root) &&
                !identical(parent, workspace_root)) break
        current <- parent
    }
    NULL
}

#' Evaluate the small, static path language supported by source indexing
#' @noRd
index_static_path <- function(expr) {
    if (is.character(expr) && length(expr) == 1L && !is.na(expr)) {
        return(list(path = expr, project_relative = FALSE))
    }
    if (!is.call(expr)) return(NULL)

    fun <- deparse(expr[[1L]], nlines = 1L)
    if (fun %in% c("file.path", "base::file.path")) {
        parts <- as.list(expr)[-1L]
        if (!length(parts) || !all(vapply(parts, function(part) {
            is.character(part) && length(part) == 1L && !is.na(part)
        }, logical(1L)))) return(NULL)
        return(list(
            path = do.call(file.path, unname(parts)),
            project_relative = FALSE
        ))
    }
    if (fun %in% c("here::here", "here:::here")) {
        parts <- as.list(expr)[-1L]
        if (!all(vapply(parts, function(part) {
            is.character(part) && length(part) == 1L && !is.na(part)
        }, logical(1L)))) return(NULL)
        return(list(
            path = if (length(parts)) do.call(file.path, unname(parts)) else "",
            project_relative = TRUE
        ))
    }
    NULL
}

#' Find statically resolvable source calls in an expression
#' @noRd
index_source_specs <- function(expr) {
    result <- list()
    calls <- .Call("source_calls_c", expr, PACKAGE = "languageserver")
    for (node in calls) {
        args <- as.list(node)[-1L]
        arg_names <- names(args)
        file_arg <- NULL
        if (length(args)) {
            named_file <- which(!is.null(arg_names) & arg_names == "file")
            if (length(named_file)) {
                file_index <- named_file[[1L]]
            } else {
                unnamed <- if (is.null(arg_names)) {
                    seq_along(args)
                } else {
                    which(!nzchar(arg_names))
                }
                file_index <- if (length(unnamed)) unnamed[[1L]] else NULL
            }
            if (length(file_index) &&
                    !identical(args[[file_index]], quote(expr = ))) {
                file_arg <- args[[file_index]]
            }
        }
        spec <- index_static_path(file_arg)
        if (!is.null(spec)) result[[length(result) + 1L]] <- spec
    }
    result
}

#' Resolve a static source path without evaluating project code
#' @noRd
index_source_candidates <- function(spec, from_path, workspace_root) {
    if (is.null(spec) || !length(spec$path) || !nzchar(spec$path)) return(NULL)
    raw_path <- path.expand(spec$path)
    absolute <- grepl("^(?:/|[[:alpha:]]:[/\\\\])", raw_path)
    join_path <- function(base, relative) {
        normalized_base <- fs::path_norm(base)
        normalized <- fs::path_norm(file.path(base, relative))
        rel <- fs::path_rel(normalized, start = normalized_base)
        file.path(base, rel)
    }
    candidates <- if (absolute) {
        raw_path
    } else if (isTRUE(spec$project_relative)) {
        join_path(workspace_root, raw_path)
    } else {
        c(
            join_path(workspace_root, raw_path),
            join_path(dirname(from_path), raw_path)
        )
    }
    candidates <- unique(vapply(candidates, index_normalize_path, character(1L)))
    candidates[vapply(candidates, function(candidate) {
        (identical(candidate, workspace_root) ||
            path_has_parent(candidate, workspace_root))
    }, logical(1L))]
}

#' Resolve a static source path without evaluating project code
#' @noRd
index_resolve_source <- function(spec, from_path, workspace_root) {
    candidates <- index_source_candidates(spec, from_path, workspace_root)
    if (is.null(candidates)) return(NULL)
    candidates <- candidates[vapply(candidates, function(candidate) {
        file.exists(candidate) && !dir.exists(candidate)
    }, logical(1L))]
    if (length(candidates)) candidates[[1L]] else NULL
}

#' Extract top-level definitions and source edges without building semantic XML
#' @noRd
index_shallow_summary <- function(path, content, workspace_root, metadata = NULL,
    parse_data = NULL) {
    path <- index_normalize_path(path)
    workspace_root <- index_normalize_path(workspace_root)
    if (is.null(metadata)) metadata <- file.info(path)
    expressions <- if (is.null(parse_data)) tryCatch(
        parse(text = content, keep.source = TRUE),
        error = function(e) NULL
    ) else NULL
    definitions <- list()
    functions <- list()
    source_specs <- list()
    if (!is.null(parse_data)) {
        definitions <- as.list(parse_data$definitions)
        functions <- as.list(parse_data$functions)
        source_specs <- parse_data$source_specs
    } else if (!is.null(expressions)) {
        srcrefs <- attr(expressions, "srcref")
        expression_list <- as.list(expressions)
        source_specs <- index_source_specs(expressions)
        # Record replacement values in hash tables and append each name once.
        # Growing named lists both copies the list and scans its names.
        definitions_store <- new.env(hash = TRUE, parent = emptyenv())
        functions_store <- new.env(hash = TRUE, parent = emptyenv())
        definition_names <- collections::queue()
        function_names <- collections::queue()
        for (i in seq_along(expression_list)) {
            expr <- expression_list[[i]]
            if (!is.call(expr) || length(expr) != 3L) next
            operator <- deparse(expr[[1L]], nlines = 1L)
            if (operator %in% c("<-", "=") && is.symbol(expr[[2L]])) {
                symbol <- as.character(expr[[2L]])
                value <- expr[[3L]]
            } else if (operator %in% c("->", "->>") &&
                    is.symbol(expr[[3L]])) {
                symbol <- as.character(expr[[3L]])
                value <- expr[[2L]]
            } else {
                next
            }
            srcref <- if (length(srcrefs) >= i) srcrefs[[i]] else NULL
            if (is.null(srcref)) next
            def_type <- get_expr_type(value)
            if (def_type == "function") {
                fun <- null_function
                tryCatch(formals(fun) <- value[[2L]], error = function(e) NULL)
                if (!exists(symbol, envir = functions_store, inherits = FALSE)) {
                    function_names$push(symbol)
                }
                assign(symbol, utils::removeSource(fun), envir = functions_store)
            }
            if (!exists(symbol, envir = definitions_store, inherits = FALSE)) {
                definition_names$push(symbol)
            }
            assign(symbol, list(
                name = symbol,
                type = def_type,
                range = expr_range(srcref)
            ), envir = definitions_store)
        }
        materialize <- function(store, names) {
            if (!names$size()) return(list())
            mget(unlist(names$as_list(), use.names = FALSE),
                envir = store, inherits = FALSE)
        }
        definitions <- materialize(definitions_store, definition_names)
        functions <- materialize(functions_store, function_names)
    }

    sources <- unique(Filter(Negate(is.null), lapply(source_specs, function(spec) {
        resolved <- index_resolve_source(spec, path, workspace_root)
        if (is.null(resolved)) NULL else path_to_uri(resolved)
    })))
    source_candidates <- unique(unlist(lapply(source_specs, function(spec) {
        candidates <- index_source_candidates(spec, path, workspace_root)
        if (length(candidates)) vapply(candidates, path_to_uri, character(1L))
        else character()
    }), use.names = FALSE))
    source_candidate_exists <- vapply(source_candidates, function(uri) {
        candidate <- path_from_uri(uri)
        file.exists(candidate) && !dir.exists(candidate)
    }, logical(1L))
    size <- if (is.data.frame(metadata) && nrow(metadata)) metadata$size[[1L]] else NA_real_
    mtime <- if (is.data.frame(metadata) && nrow(metadata)) metadata$mtime[[1L]] else as.POSIXct(NA)
    list(
        uri = path_to_uri(path),
        path = path,
        size = as.numeric(size),
        mtime = as.numeric(mtime),
        content_hash = get_content_hash(content),
        definitions = definitions,
        functions = functions,
        sources = sources,
        source_candidates = source_candidates,
        source_candidate_exists = source_candidate_exists,
        parse_error = if (is.null(parse_data)) is.null(expressions) else
            isTRUE(parse_data$parse_error),
        package_root = index_package_root(path, workspace_root)
    )
}

#' Check that cached source calls still resolve to the same files
#' @noRd
index_source_candidates_current <- function(summary) {
    candidates <- summary$source_candidates
    if (!length(candidates)) return(TRUE)
    current_exists <- vapply(candidates, function(uri) {
        path <- path_from_uri(uri)
        file.exists(path) && !dir.exists(path)
    }, logical(1L))
    !is.null(summary$source_candidate_exists) && identical(
        unname(current_exists), unname(summary$source_candidate_exists))
}

#' A bounded, project-wide index of R source metadata
#' @noRd
WorkspaceIndex <- R6::R6Class("WorkspaceIndex",
    private = list(
        glob_patterns = list(),
        glob_regex = list(),
        catalog = NULL,
        catalog_revision = NULL,
        context_generations = NULL,
        reference_generations = NULL,
        generations = NULL,
        awaiting_parses = NULL,
        pending_documents = NULL,
        diagnostics_generations = NULL,
        package_diagnostics_generations = NULL,
        diagnostics_discovery = 0L,
        dirty_updates = NULL,
        dirty_since = NULL,
        cache_path = NULL,
        worker_bootstrap = TRUE,
        worker_sync_uris = character(),
        worker_requests = list(),
        worker_key = NULL,
        worker_lifecycle = 0L,
        discovery_start = FALSE,
        discovery_generations = NULL,
        discovery_stale = NULL,
        discovery_result = NULL,
        discovery_position = 1L,
        source_closures = NULL,
        dependent_closures = NULL,
        package_memberships = NULL,
        invalidate_scopes = function(contexts, references) {
            for (uri in contexts) {
                private$context_generations$set(uri,
                    private$context_generations$get(uri, 0L) + 1L)
                if (private$source_closures$has(uri)) private$source_closures$remove(uri)
            }
            for (uri in references) {
                private$reference_generations$set(uri,
                    private$reference_generations$get(uri, 0L) + 1L)
                if (private$dependent_closures$has(uri)) private$dependent_closures$remove(uri)
            }
        },
        invalidate_diagnostics = function(uri, roots) {
            dependents <- if (self$reverse_edges$has(uri)) self$dependent_closure(uri) else uri
            for (dependent in dependents) {
                private$diagnostics_generations$set(dependent,
                    private$diagnostics_generations$get(dependent, 0L) + 1L)
            }
            for (root in unique(Filter(Negate(is.null), roots))) {
                private$package_diagnostics_generations$set(root,
                    private$package_diagnostics_generations$get(root, 0L) + 1L)
            }
            invisible(NULL)
        },
        mark_dirty = function(uri, summary = NULL) {
            if (!is.null(self$cache_file())) private$dirty_updates$set(uri, summary)
            if (is.null(private$dirty_since)) private$dirty_since <- proc.time()[[3L]]
            self$cache_dirty <- TRUE
        },
        request_is_current = function(request) {
            identical(private$generations$get(request$uri, 0L), request$generation) &&
                !private$awaiting_parses$has(request$uri)
        },
        discovery_is_current = function(uri) {
            generation <- private$discovery_generations[[uri]]
            if (is.null(generation)) generation <- 0L
            identical(private$generations$get(uri, 0L), generation)
        },
        finish_discovery = function(batch_size, budget) {
            result <- private$discovery_result
            if (isTRUE(result$restart)) {
                private$discovery_start <- TRUE
                private$discovery_result <- NULL
                return(invisible(NULL))
            }
            started <- proc.time()[[3L]]
            changed <- character()
            processed <- 0L
            while (private$discovery_position <= length(result$files) && processed < batch_size) {
                i <- private$discovery_position
                private$discovery_position <- i + 1L
                processed <- processed + 1L
                file <- result$files[[i]]
                uri <- file$uri
                if (!private$discovery_is_current(uri)) next
                info <- file.info(file$path)
                if (!nrow(info) || is.na(info$size[[1L]]) || isTRUE(info$isdir[[1L]])) next
                self$files$set(uri, file)
                summary <- result$summaries[[i]]
                if (!private$awaiting_parses$has(uri) && !is.null(summary) &&
                        identical(as.numeric(info$size[[1L]]), summary$size) &&
                        identical(as.numeric(info$mtime[[1L]]), summary$mtime) &&
                        index_source_candidates_current(summary)) {
                    self$set_summary(summary)
                } else if (!private$awaiting_parses$has(uri)) {
                    self$pending <- union(self$pending, uri)
                }
                changed <- c(changed, uri)
                if ((proc.time()[[3L]] - started) * 1000 >= budget) break
            }
            if (length(changed)) {
                self$package_revision <- self$package_revision + 1L
                private$package_memberships$clear()
                self$revision <- self$revision + 1
                if (is.function(self$on_update)) self$on_update(changed)
            }
            if (private$discovery_position > length(result$files)) {
                private$discovery_result <- NULL
                if (isTRUE(result$done)) {
                    self$truncated <- result$truncated
                    private$discovery_stale <- setdiff(self$summaries$keys(), self$files$keys())
                }
            }
            invisible(NULL)
        },
        finish_batch = function(result, requests) {
            self$worker_running <- FALSE
            private$worker_requests <- list()
            private$worker_bootstrap <- isTRUE(result$sync_needed)
            temp_file <- result$temp_file
            if (!is.null(temp_file)) {
                on.exit(unlink(temp_file), add = TRUE)
                if (identical(result$revision, self$revision) &&
                        private$dirty_updates$size() == 0L && !length(private$worker_sync_uris)) {
                    cache_file <- self$cache_file()
                    saved <- if (is.null(cache_file)) FALSE else tryCatch({
                        if (file.exists(cache_file)) file.copy(temp_file, cache_file,
                            overwrite = TRUE) else file.rename(temp_file, cache_file)
                    }, error = function(e) FALSE)
                    if (isTRUE(saved)) {
                        self$cache_dirty <- FALSE
                        private$dirty_since <- NULL
                    }
                }
            }
            changed <- character()
            for (i in seq_along(requests)) {
                request <- requests[[i]]
                if (!private$request_is_current(request)) next
                if (i > result$processed) {
                    self$pending <- union(self$pending, request$uri)
                    next
                }
                summary <- result$summaries[[i]]
                if (is.null(summary)) {
                    if (isTRUE(result$retry[[i]]) && file.exists(request$path)) {
                        self$pending <- union(self$pending, request$uri)
                    }
                    next
                }
                info <- file.info(request$path)
                if (!nrow(info) || is.na(info$size[[1L]]) ||
                        !identical(as.numeric(info$size[[1L]]), summary$size) ||
                        !identical(as.numeric(info$mtime[[1L]]), summary$mtime)) {
                    self$pending <- union(self$pending, request$uri)
                    next
                }
                summary$cacheable <- TRUE
                self$set_summary(summary)
                previous_file <- self$files$get(request$uri, NULL)
                self$files$set(request$uri,
                    summary[c("uri", "path", "size", "mtime", "package_root")])
                if (!identical(previous_file$package_root, summary$package_root)) {
                    self$package_revision <- self$package_revision + 1L
                    private$package_memberships$clear()
                }
                changed <- c(changed, request$uri)
            }
            if (length(changed) && is.function(self$on_update)) self$on_update(changed)
            invisible(changed)
        }
    ),
    public = list(
        root = NULL,
        files = NULL,
        summaries = NULL,
        source_edges = NULL,
        reverse_edges = NULL,
        pending = NULL,
        truncated = FALSE,
        enabled = TRUE,
        cache_dirty = FALSE,
        processing_batch = FALSE,
        revision = 0,
        topology_revision = 0L,
        package_revision = 0L,
        candidate_reverse_edges = NULL,
        task_manager = NULL,
        on_update = NULL,
        worker_running = FALSE,
        discovering = FALSE,

        initialize = function(root, cache_path = NULL) {
            private$cache_path <- cache_path
            self$root <- if (length(root) && nzchar(root)) {
                index_normalize_path(root)
            } else {
                NULL
            }
            self$files <- collections::dict()
            self$summaries <- collections::dict()
            self$source_edges <- collections::dict()
            self$reverse_edges <- collections::dict()
            self$pending <- character()
            self$candidate_reverse_edges <- collections::dict()
            private$context_generations <- collections::dict()
            private$reference_generations <- collections::dict()
            private$generations <- collections::dict()
            private$awaiting_parses <- collections::dict()
            private$pending_documents <- collections::dict()
            private$diagnostics_generations <- collections::dict()
            private$package_diagnostics_generations <- collections::dict()
            private$dirty_updates <- collections::dict()
            private$source_closures <- collections::dict()
            private$dependent_closures <- collections::dict()
            private$package_memberships <- collections::dict()
            private$worker_key <- tempfile("workspace-index-")
            mode <- lsp_settings$get("index_mode")
            if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
                mode <- "auto"
            }
            self$enabled <- !is.null(self$root) &&
                !identical(tolower(mode), "off")
        },

        attach_worker = function(manager, on_update = NULL) {
            self$task_manager <- manager
            self$on_update <- on_update
            invisible(NULL)
        },

        detach_worker = function() {
            private$worker_lifecycle <- private$worker_lifecycle + 1L
            if (!is.null(self$task_manager)) self$task_manager$cancel(private$worker_key)
            self$task_manager <- NULL
            self$on_update <- NULL
            self$worker_running <- FALSE
            self$discovering <- FALSE
            private$discovery_stale <- NULL
            private$discovery_result <- NULL
            private$discovery_generations <- NULL
            private$worker_bootstrap <- TRUE
            private$worker_sync_uris <- character()
            private$worker_requests <- list()
            private$pending_documents$clear()
            invisible(NULL)
        },

        invalidate = function(uri) {
            uri <- index_canonical_uri(uri)
            private$generations$set(uri, private$generations$get(uri, 0L) + 1L)
            if (!self$processing_batch) self$pending <- setdiff(self$pending, uri)
            invisible(NULL)
        },

        await_parse = function(uri, cacheable = FALSE) {
            if (!self$enabled || !self$should_index(path_from_uri(uri))) return(NULL)
            uri <- index_canonical_uri(uri)
            self$invalidate(uri)
            private$awaiting_parses$set(uri, isTRUE(cacheable))
            previous <- self$summaries$get(uri, NULL)
            if (!is.null(previous) && identical(previous$cacheable, FALSE) !=
                    !isTRUE(cacheable)) {
                previous$cacheable <- isTRUE(cacheable)
                self$set_summary(previous)
                private$awaiting_parses$set(uri, isTRUE(cacheable))
            }
            if (!self$files$has(uri)) {
                path <- path_from_uri(uri)
                info <- file.info(path)
                self$files$set(uri, list(uri = uri, path = path,
                        size = as.numeric(info$size[[1L]]),
                        mtime = as.numeric(info$mtime[[1L]]),
                        package_root = index_package_root(path, self$root)))
                if (!is.null(self$files$get(uri)$package_root)) {
                    self$package_revision <- self$package_revision + 1L
                    private$package_memberships$clear()
                }
            }
            invisible(NULL)
        },

        queue_path = function(path, force = FALSE, invalidate = TRUE) {
            if (!self$enabled || !self$should_index(path)) return(NULL)
            uri <- index_canonical_uri(path_to_uri(path))
            if (private$awaiting_parses$has(uri)) {
                if (!isTRUE(force)) return(invisible(NULL))
                private$awaiting_parses$remove(uri)
            }
            if (!isTRUE(invalidate)) {
                # Lookup-driven requests share pending reads. File events use
                # the default invalidation path to supersede older snapshots.
                if (uri %in% self$pending) return(invisible(NULL))
                for (request in private$worker_requests) {
                    if (identical(request$uri, uri) && private$request_is_current(request)) {
                        return(invisible(NULL))
                    }
                }
            }
            if (isTRUE(invalidate)) self$invalidate(uri)
            self$pending <- union(self$pending, uri)
            invisible(NULL)
        },

        queue_document = function(uri) {
            # Shallow summaries can arrive before the full parse needed by
            # refactoring. Keep that work visible until its parse is accepted.
            private$pending_documents$set(index_canonical_uri(uri), TRUE)
            invisible(NULL)
        },

        finish_document = function(uri) {
            private$pending_documents$pop(index_canonical_uri(uri), NULL)
            invisible(NULL)
        },

        scope_generation = function(uri, references = FALSE) {
            uri <- index_canonical_uri(uri)
            list(package = self$package_revision,
                source = if (references) private$reference_generations$get(uri, 0L)
                else private$context_generations$get(uri, 0L))
        },

        indexing_status = function(queued_documents = character()) {
            current <- Filter(private$request_is_current, private$worker_requests)
            pending <- unique(c(self$pending, private$awaiting_parses$keys(),
                    vapply(current, `[[`, character(1L), "uri"),
                    private$pending_documents$keys(), queued_documents))
            total <- self$files$size()
            list(discovering = self$discovering, total = total,
                completed = max(total - length(pending), 0L),
                done = !self$discovering && !length(pending), truncated = self$truncated)
        },

        diagnostics_generation = function(uri) {
            uri <- index_canonical_uri(uri)
            root <- self$package_root_for_uri(uri)
            list(index = private$worker_key, discovery = private$diagnostics_discovery,
                source = private$diagnostics_generations$get(uri, 0L),
                package = if (is.null(root)) NULL else
                    private$package_diagnostics_generations$get(root, 0L))
        },

        invalidate_document_diagnostics = function(uri) {
            uri <- index_canonical_uri(uri)
            private$invalidate_diagnostics(uri, list(self$package_root_for_uri(uri)))
            invisible(NULL)
        },

        include_patterns = function() {
            value <- lsp_settings$get("index_include")
            if (!is.character(value) || !length(value)) "**/*.R" else value
        },

        exclude_patterns = function() {
            value <- lsp_settings$get("index_exclude")
            if (!is.character(value)) character() else value
        },

        compiled_globs = function(exclude = FALSE) {
            key <- if (exclude) "exclude" else "include"
            patterns <- if (exclude) self$exclude_patterns() else self$include_patterns()
            if (!identical(private$glob_patterns[[key]], patterns)) {
                private$glob_patterns[[key]] <- patterns
                valid <- patterns[!is.na(patterns) & nzchar(patterns)]
                private$glob_regex[[key]] <- vapply(valid, index_glob_regex, character(1L))
            }
            private$glob_regex[[key]]
        },

        max_files = function() {
            value <- suppressWarnings(as.integer(lsp_settings$get("index_max_files")))
            if (length(value) != 1L || is.na(value) || value < 1L) 10000L else value
        },

        max_file_bytes = function() {
            value <- suppressWarnings(as.numeric(
                lsp_settings$get("index_max_file_size_mb")))
            if (length(value) != 1L || is.na(value) || value <= 0) value <- 2
            value * 1024^2
        },

        contains_path = function(path) {
            if (!self$enabled || !length(path) || !nzchar(path)) return(FALSE)
            path <- index_normalize_path(path)
            identical(path, self$root) || path_has_parent(path, self$root)
        },

        should_index = function(path, directory = FALSE) {
            if (!self$contains_path(path)) return(FALSE)
            rel <- gsub("\\", "/", fs::path_rel(path, start = self$root),
                fixed = TRUE)
            if (index_glob_match(rel, NULL, directory, self$compiled_globs(TRUE))) {
                return(FALSE)
            }
            directory || index_glob_match(rel, NULL, regex = self$compiled_globs())
        },

        cache_file = function() {
            if (!isTRUE(lsp_settings$get("index_persistent_cache"))) return(NULL)
            if (!is.null(private$cache_path)) return(private$cache_path)
            base <- if (exists("R_user_dir", envir = asNamespace("tools"),
                    inherits = FALSE)) {
                get("R_user_dir", envir = asNamespace("tools"))(
                    "languageserver", "cache")
            } else {
                file.path(path.expand("~"), ".cache", "R", "languageserver")
            }
            file.path(base, "workspace-index",
                paste0(digest::digest(self$root, algo = "xxhash64"), ".rds"))
        },

        load_cache = function() {
            cache_file <- self$cache_file()
            if (is.null(cache_file) || !file.exists(cache_file)) return(NULL)
            cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
            if (!is.list(cached) || !identical(cached$version, 1L) ||
                    !identical(cached$root, self$root) ||
                    !is.list(cached$summaries)) return(NULL)
            for (summary in cached$summaries) {
                if (!is.list(summary) || is.null(summary$functions) ||
                        !file.exists(summary$path)) next
                info <- file.info(summary$path)
                if (!nrow(info) || is.na(info$size[[1L]]) ||
                        !identical(as.numeric(info$size[[1L]]), summary$size) ||
                        !identical(as.numeric(info$mtime[[1L]]), summary$mtime)) next
                if (!index_source_candidates_current(summary)) next
                self$set_summary(summary)
            }
            invisible(NULL)
        },

        save_cache = function() {
            if (!self$cache_dirty) return(NULL)
            cache_file <- self$cache_file()
            if (is.null(cache_file)) return(NULL)
            dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
            temp_file <- tempfile("index-", tmpdir = dirname(cache_file))
            on.exit(unlink(temp_file), add = TRUE)
            value <- list(
                version = 1L,
                root = self$root,
                summaries = unname(Filter(function(summary) {
                    !identical(summary$cacheable, FALSE)
                }, self$summaries$values()))
            )
            saved <- tryCatch({
                suppressWarnings(saveRDS(value, temp_file))
                if (file.exists(cache_file)) {
                    file.copy(temp_file, cache_file, overwrite = TRUE)
                } else {
                    file.rename(temp_file, cache_file)
                }
            }, error = function(e) FALSE)
            if (isTRUE(saved)) {
                self$cache_dirty <- FALSE
                private$dirty_since <- NULL
            }
            invisible(NULL)
        },

        discover = function() {
            if (!self$enabled || !dir.exists(self$root)) return(invisible(NULL))
            private$diagnostics_discovery <- private$diagnostics_discovery + 1L
            if (!is.null(self$task_manager)) {
                private$worker_lifecycle <- private$worker_lifecycle + 1L
                if (self$worker_running) self$task_manager$cancel(private$worker_key)
                self$worker_running <- FALSE
                private$worker_requests <- list()
                self$discovering <- TRUE
                private$worker_bootstrap <- TRUE
                private$discovery_start <- TRUE
                private$discovery_stale <- NULL
                private$discovery_result <- NULL
                private$discovery_generations <- stats::setNames(
                    private$generations$values(), private$generations$keys())
                self$files$clear()
                self$pending <- character()
                self$truncated <- FALSE
                self$package_revision <- self$package_revision + 1L
                private$package_memberships$clear()
                self$revision <- self$revision + 1
                return(invisible(NULL))
            }
            old_files <- self$files$values()
            self$files$clear()
            self$pending <- character()
            self$truncated <- FALSE
            self$load_cache()
            queue <- collections::queue()
            queue$push(self$root)
            visited <- new.env(hash = TRUE, parent = emptyenv())
            count <- 0L
            max_files <- self$max_files()
            max_bytes <- self$max_file_bytes()
            pending <- character(max_files)
            pending_count <- 0L
            real_root <- normalizePath(
                self$root, winslash = "/", mustWork = FALSE)

            while (queue$size() && count < max_files) {
                directory <- queue$pop()
                canonical <- normalizePath(
                    directory, winslash = "/", mustWork = FALSE)
                if (exists(canonical, envir = visited, inherits = FALSE)) next
                assign(canonical, TRUE, envir = visited)
                entries <- tryCatch(list.files(
                    directory, all.files = TRUE, no.. = TRUE,
                    full.names = TRUE), error = function(e) character())
                if (!length(entries)) next
                entries <- sort(entries, method = "radix")
                info <- file.info(entries)
                for (i in seq_along(entries)) {
                    path <- index_normalize_path(entries[[i]])
                    real_path <- normalizePath(
                        path, winslash = "/", mustWork = FALSE)
                    if (!identical(real_path, real_root) &&
                            !path_has_parent(real_path, real_root)) next
                    is_dir <- isTRUE(info$isdir[[i]])
                    if (is_dir) {
                        if (self$should_index(path, directory = TRUE)) {
                            queue$push(path)
                        }
                        next
                    }
                    if (!self$should_index(path) || is.na(info$size[[i]]) ||
                            info$size[[i]] > max_bytes) next
                    count <- count + 1L
                    if (count > max_files) break
                    uri <- path_to_uri(path)
                    metadata <- list(
                        uri = uri,
                        path = path,
                        size = as.numeric(info$size[[i]]),
                        mtime = as.numeric(info$mtime[[i]]),
                        package_root = index_package_root(path, self$root)
                    )
                    self$files$set(uri, metadata)
                    if (!self$summaries$has(uri) && !private$awaiting_parses$has(uri)) {
                        pending_count <- pending_count + 1L
                        pending[[pending_count]] <- uri
                    }
                }
            }
            self$pending <- if (pending_count) {
                pending[seq_len(pending_count)]
            } else {
                character()
            }
            stale <- setdiff(self$summaries$keys(), self$files$keys())
            for (uri in stale) self$remove(uri)
            self$truncated <- queue$size() > 0L || count >= max_files
            package <- vapply(self$pending, function(uri) {
                !is.null(self$files$get(uri)$package_root)
            }, logical(1L))
            self$pending <- c(self$pending[package], self$pending[!package])
            package_files <- function(files) {
                lapply(Filter(function(file) !is.null(file$package_root), files),
                    function(file) file[c("uri", "package_root")])
            }
            if (!identical(package_files(old_files), package_files(self$files$values()))) {
                self$package_revision <- self$package_revision + 1L
                private$package_memberships$clear()
            }
            self$revision <- self$revision + 1
            invisible(NULL)
        },

        set_summary = function(summary) {
            uri <- summary$uri
            previous <- self$summaries$get(uri, NULL)
            if (private$awaiting_parses$has(uri)) summary$cacheable <- private$awaiting_parses$get(uri)
            if (isTRUE(summary$parse_error) && !is.null(previous)) {
                for (field in c("definitions", "functions", "sources",
                        "source_candidates", "source_candidate_exists")) {
                    summary[[field]] <- previous[[field]]
                }
            }
            old_sources <- self$source_edges$get(uri, character())
            sources <- unlist(summary$sources, use.names = FALSE)
            old_sources <- unlist(old_sources, use.names = FALSE)
            topology_changed <- !identical(old_sources, sources)
            if (!identical(names(previous$definitions), names(summary$definitions)) ||
                    !identical(previous$functions, summary$functions) ||
                    !identical(previous$package_root, summary$package_root) || topology_changed) {
                private$invalidate_diagnostics(uri, list(previous$package_root, summary$package_root))
            }
            if (topology_changed) {
                contexts <- self$dependent_closure(uri)
                references <- self$source_closure(uri)
                for (target in setdiff(old_sources, sources)) {
                    reverse <- setdiff(self$reverse_edges$get(target, character()), uri)
                    if (length(reverse)) self$reverse_edges$set(target, reverse)
                    else if (self$reverse_edges$has(target)) self$reverse_edges$remove(target)
                }
                for (target in setdiff(sources, old_sources)) {
                    self$reverse_edges$set(target,
                        union(self$reverse_edges$get(target, character()), uri))
                }
                self$source_edges$set(uri, sources)
                # Traverse the new graph after clearing only the closure at
                # this root, then invalidate all affected old/new reachability.
                if (private$source_closures$has(uri)) private$source_closures$remove(uri)
                if (private$dependent_closures$has(uri)) private$dependent_closures$remove(uri)
                contexts <- union(contexts, self$dependent_closure(uri))
                references <- union(references, self$source_closure(uri))
                private$invalidate_scopes(contexts, references)
                self$topology_revision <- self$topology_revision + 1L
            } else if (!self$source_edges$has(uri)) {
                self$source_edges$set(uri, sources)
            }
            old_candidates <- previous$source_candidates
            candidates <- summary$source_candidates
            for (target in setdiff(old_candidates, candidates)) {
                reverse <- setdiff(self$candidate_reverse_edges$get(target, character()), uri)
                if (length(reverse)) self$candidate_reverse_edges$set(target, reverse)
                else if (self$candidate_reverse_edges$has(target)) self$candidate_reverse_edges$remove(target)
            }
            for (target in setdiff(candidates, old_candidates)) {
                self$candidate_reverse_edges$set(target,
                    union(self$candidate_reverse_edges$get(target, character()), uri))
            }
            if (!identical(previous$package_root, summary$package_root)) {
                self$package_revision <- self$package_revision + 1L
                private$package_memberships$clear()
            }
            self$summaries$set(uri, summary)
            if (private$awaiting_parses$has(uri)) private$awaiting_parses$remove(uri)
            private$mark_dirty(uri, summary)
            self$revision <- self$revision + 1
            invisible(summary)
        },

        update_content = function(uri, content, metadata = NULL,
            cacheable = TRUE, parse_data = NULL) {
            if (!self$enabled) return(NULL)
            path <- path_from_uri(uri)
            if (!self$should_index(path)) return(NULL)
            uri <- index_canonical_uri(uri)
            if (private$awaiting_parses$has(uri)) {
                cacheable <- private$awaiting_parses$get(uri)
            }
            self$invalidate(uri)
            if (is.null(metadata)) metadata <- file.info(path)
            summary <- index_shallow_summary(
                path, content, self$root, metadata = metadata,
                parse_data = parse_data)
            uri <- index_canonical_uri(uri)
            summary$uri <- uri
            summary$cacheable <- isTRUE(cacheable)
            if (isTRUE(summary$parse_error) && self$summaries$has(uri)) {
                previous <- self$summaries$get(uri)
                summary$definitions <- previous$definitions
                summary$functions <- previous$functions
                summary$sources <- previous$sources
                summary$source_candidates <- previous$source_candidates
                summary$source_candidate_exists <-
                    previous$source_candidate_exists
            }
            self$set_summary(summary)
            if (!self$processing_batch) {
                self$pending <- setdiff(self$pending, uri)
            }
            previous_file <- self$files$get(uri, NULL)
            self$files$set(uri, summary[c("uri", "path", "size", "mtime", "package_root")])
            if (!identical(previous_file$package_root, summary$package_root)) {
                self$package_revision <- self$package_revision + 1L
                private$package_memberships$clear()
            }
            summary
        },

        update_path = function(path) {
            if (!self$enabled || !self$should_index(path) || !file.exists(path)) {
                return(NULL)
            }
            info <- file.info(path)
            if (!nrow(info) || is.na(info$size[[1L]]) ||
                    info$size[[1L]] > self$max_file_bytes()) return(NULL)
            content <- tryCatch(stringi::stri_read_lines(path),
                error = function(e) NULL)
            if (is.null(content)) return(NULL)
            self$update_content(path_to_uri(index_normalize_path(path)), content, info)
        },

        process_batch = function() {
            if (!is.null(self$task_manager)) {
                if (self$worker_running) return(character())
                batch_size <- suppressWarnings(as.integer(lsp_settings$get("index_batch_size")))
                if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) batch_size <- 20L
                budget <- suppressWarnings(as.numeric(lsp_settings$get("index_time_budget_ms")))
                if (length(budget) != 1L || is.na(budget) || budget <= 0) budget <- 25
                if (self$discovering) {
                    if (!is.null(private$discovery_result)) {
                        private$finish_discovery(batch_size, budget)
                        return(character())
                    }
                    if (!is.null(private$discovery_stale)) {
                        started <- proc.time()[[3L]]
                        consumed <- 0L
                        for (uri in head(private$discovery_stale, batch_size)) {
                            consumed <- consumed + 1L
                            summary <- self$summaries$get(uri, NULL)
                            if (private$discovery_is_current(uri) &&
                                    !identical(summary$cacheable, FALSE) &&
                                    !private$awaiting_parses$has(uri)) self$remove(uri)
                            if ((proc.time()[[3L]] - started) * 1000 >= budget) break
                        }
                        if (consumed) private$discovery_stale <- private$discovery_stale[-seq_len(consumed)]
                        if (!length(private$discovery_stale)) {
                            self$discovering <- FALSE
                            private$diagnostics_discovery <- private$diagnostics_discovery + 1L
                            private$discovery_stale <- NULL
                            private$discovery_generations <- NULL
                            if (is.function(self$on_update)) self$on_update(character())
                        }
                        return(character())
                    }
                    settings <- if (private$discovery_start) {
                        setting_names <- c("index_mode", "index_include", "index_exclude",
                            "index_max_files", "index_max_file_size_mb")
                        stats::setNames(lapply(setting_names, lsp_settings$get), setting_names)
                    } else {
                        NULL
                    }
                    private$discovery_start <- FALSE
                    self$worker_running <- TRUE
                    lifecycle <- private$worker_lifecycle
                    self$task_manager$add_task(private$worker_key, create_task(
                        package_call(index_worker_discover),
                        list(key = private$worker_key, root = self$root, settings = settings,
                            cache_file = self$cache_file(), batch_size = min(batch_size, 20L)),
                        callback = function(result) {
                            if (!identical(lifecycle, private$worker_lifecycle)) return(NULL)
                            self$worker_running <- FALSE
                            private$discovery_result <- result
                            private$discovery_position <- 1L
                        },
                        error = function(e) {
                            if (!identical(lifecycle, private$worker_lifecycle)) return(NULL)
                            self$worker_running <- FALSE
                            private$discovery_start <- TRUE
                            logger$info("workspace discovery task: ", e)
                        }, profiles = FALSE
                    ))
                    return(character())
                }
                candidates <- head(self$pending, batch_size)
                requests <- lapply(candidates, function(uri) {
                    list(uri = uri, path = path_from_uri(uri),
                        generation = private$generations$get(uri, 0L))
                })
                cache_file <- if (!length(requests) && self$cache_dirty &&
                        !is.null(private$dirty_since) &&
                        proc.time()[[3L]] - private$dirty_since >= 1) self$cache_file() else NULL
                if (!length(requests) && private$dirty_updates$size() == 0L &&
                        !length(private$worker_sync_uris) &&
                        is.null(cache_file)) return(character())
                bootstrap <- private$worker_bootstrap
                if (bootstrap) {
                    private$worker_sync_uris <- self$summaries$keys()
                }
                sync_uris <- head(private$worker_sync_uris, batch_size)
                if (length(sync_uris)) private$worker_sync_uris <- private$worker_sync_uris[-seq_along(sync_uris)]
                remaining <- max(batch_size - length(sync_uris), 0L)
                update_uris <- unique(c(sync_uris, head(private$dirty_updates$keys(), remaining)))
                updates <- stats::setNames(lapply(update_uris, function(uri) {
                    if (private$dirty_updates$has(uri)) private$dirty_updates$pop(uri)
                    else self$summaries$get(uri, NULL)
                }), update_uris)
                if (private$dirty_updates$size() || length(private$worker_sync_uris)) cache_file <- NULL
                self$pending <- setdiff(self$pending, candidates)
                self$worker_running <- TRUE
                private$worker_requests <- requests
                lifecycle <- private$worker_lifecycle
                self$task_manager$add_task(private$worker_key, create_task(
                    package_call(index_worker_batch),
                    list(key = private$worker_key, root = self$root,
                        updates = updates, requests = requests, bootstrap = bootstrap,
                        cache_file = cache_file, revision = self$revision,
                        time_budget_ms = budget, max_file_bytes = self$max_file_bytes()),
                    callback = function(result) {
                        if (!identical(lifecycle, private$worker_lifecycle)) {
                            if (!is.null(result$temp_file)) unlink(result$temp_file)
                            return(invisible(NULL))
                        }
                        private$finish_batch(result, requests)
                    },
                    error = function(e) {
                        if (!identical(lifecycle, private$worker_lifecycle)) return(NULL)
                        self$worker_running <- FALSE
                        private$worker_requests <- list()
                        private$worker_bootstrap <- TRUE
                        current <- vapply(requests, private$request_is_current, logical(1L))
                        self$pending <- union(self$pending, candidates[current])
                        logger$info("workspace index task: ", e)
                    }, profiles = FALSE
                ))
                return(character())
            }
            if (!length(self$pending)) {
                self$save_cache()
                return(character())
            }
            batch_size <- suppressWarnings(as.integer(
                lsp_settings$get("index_batch_size")))
            if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
                batch_size <- 20L
            }
            budget <- suppressWarnings(as.numeric(
                lsp_settings$get("index_time_budget_ms")))
            if (length(budget) != 1L || is.na(budget) || budget <= 0) budget <- 25
            started <- proc.time()[[3L]]
            processed <- character()
            candidates <- head(self$pending, batch_size)
            consumed <- 0L
            self$processing_batch <- TRUE
            on.exit({
                self$processing_batch <- FALSE
            }, add = TRUE)
            for (uri in candidates) {
                consumed <- consumed + 1L
                summary <- self$update_path(path_from_uri(uri))
                if (!is.null(summary)) processed <- c(processed, uri)
                elapsed_ms <- (proc.time()[[3L]] - started) * 1000
                if (elapsed_ms >= budget) break
            }
            if (consumed) self$pending <- self$pending[-seq_len(consumed)]
            self$processing_batch <- FALSE
            if (!length(self$pending)) self$save_cache()
            processed
        },

        remove = function(uri) {
            uri <- index_canonical_uri(uri)
            self$finish_document(uri)
            self$invalidate(uri)
            if (private$awaiting_parses$has(uri)) private$awaiting_parses$remove(uri)
            previous <- self$summaries$get(uri, NULL)
            if (!is.null(previous)) {
                empty <- previous
                empty$sources <- empty$source_candidates <- character()
                empty$definitions <- empty$functions <- list()
                empty$parse_error <- FALSE
                empty$package_root <- NULL
                self$set_summary(empty)
                self$summaries$remove(uri)
            }
            if (self$source_edges$has(uri)) self$source_edges$remove(uri)
            if (self$files$has(uri)) {
                package_file <- !is.null(self$files$get(uri)$package_root)
                self$files$remove(uri)
                if (package_file) {
                    self$package_revision <- self$package_revision + 1L
                    private$package_memberships$clear()
                }
            }
            private$mark_dirty(uri)
            self$revision <- self$revision + 1
            invisible(NULL)
        },

        source_closure = function(uri, update = FALSE) {
            uri <- index_canonical_uri(uri)
            if (!isTRUE(update) && private$source_closures$has(uri)) {
                return(private$source_closures$get(uri))
            }
            result <- character(self$max_files() + 1L)
            result_count <- 0L
            queue <- collections::queue()
            queue$push(index_canonical_uri(uri))
            visited <- new.env(hash = TRUE, parent = emptyenv())
            while (queue$size()) {
                current <- queue$pop()
                if (exists(current, envir = visited, inherits = FALSE)) next
                assign(current, TRUE, envir = visited)
                if (isTRUE(update) && !self$summaries$has(current)) {
                    if (is.null(self$task_manager)) self$update_path(path_from_uri(current))
                    else self$queue_path(path_from_uri(current), invalidate = FALSE)
                }
                result_count <- result_count + 1L
                if (result_count > length(result)) {
                    result <- c(result, character(length(result)))
                }
                result[[result_count]] <- current
                for (target in self$source_edges$get(current, character())) {
                    queue$push(target)
                }
            }
            result <- if (result_count) result[seq_len(result_count)] else character()
            if (!isTRUE(update)) private$source_closures$set(uri, result)
            result
        },

        dependent_closure = function(uri) {
            uri <- index_canonical_uri(uri)
            if (private$dependent_closures$has(uri)) return(private$dependent_closures$get(uri))
            result <- character(self$max_files() + 1L)
            result_count <- 0L
            queue <- collections::queue()
            queue$push(index_canonical_uri(uri))
            visited <- new.env(hash = TRUE, parent = emptyenv())
            while (queue$size()) {
                current <- queue$pop()
                if (exists(current, envir = visited, inherits = FALSE)) next
                assign(current, TRUE, envir = visited)
                result_count <- result_count + 1L
                if (result_count > length(result)) {
                    result <- c(result, character(length(result)))
                }
                result[[result_count]] <- current
                for (dependent in self$reverse_edges$get(current, character())) {
                    queue$push(dependent)
                }
            }
            result <- if (result_count) result[seq_len(result_count)] else character()
            private$dependent_closures$set(uri, result)
            result
        },

        dependents = function(uri, include_candidates = FALSE) {
            uri <- index_canonical_uri(uri)
            result <- self$reverse_edges$get(uri, character())
            if (isTRUE(include_candidates)) {
                result <- union(result, self$candidate_reverse_edges$get(uri, character()))
            }
            result
        },

        package_root_for_uri = function(uri) {
            uri <- index_canonical_uri(uri)
            if (self$summaries$has(uri)) {
                return(self$summaries$get(uri)$package_root)
            }
            if (self$files$has(uri)) return(self$files$get(uri)$package_root)
            path <- path_from_uri(uri)
            if (self$contains_path(path)) index_package_root(path, self$root) else NULL
        },

        package_source_uris = function(package_root = NULL) {
            key <- if (is.null(package_root)) "*" else package_root
            if (private$package_memberships$has(key)) return(private$package_memberships$get(key))
            uris <- self$files$keys()
            keep <- vapply(uris, function(uri) {
                root <- self$files$get(uri)$package_root
                !is.null(root) && (is.null(package_root) || identical(root, package_root))
            }, logical(1L))
            result <- uris[keep]
            private$package_memberships$set(key, result)
            result
        },

        definitions_for_query = function(pattern) {
            if (is.null(private$catalog) || !identical(private$catalog_revision, self$revision)) {
                summaries <- self$summaries$values()
                sizes <- vapply(summaries, function(summary) length(names(summary$definitions)), integer(1L))
                private$catalog <- list(
                    summaries = summaries,
                    names = unlist(lapply(summaries, function(summary) names(summary$definitions)), use.names = FALSE),
                    source = rep.int(seq_along(summaries), sizes),
                    row = sequence(sizes),
                    ascii = rep.int(vapply(summaries, function(summary) {
                        is_ascii_vector(names(summary$definitions))
                    }, logical(1L)), sizes)
                )
                private$catalog_revision <- self$revision
            }
            catalog <- private$catalog
            # A mixed Unicode document takes the regex fallback even for its
            # ASCII names. Keep that established per-document fuzzy behavior.
            matches <- logical(length(catalog$names))
            if (any(catalog$ascii)) matches[catalog$ascii] <-
                fuzzy_find(catalog$names[catalog$ascii], pattern)
            if (any(!catalog$ascii)) matches[!catalog$ascii] <-
                fuzzy_find(catalog$names[!catalog$ascii], pattern)
            selected <- which(matches)
            lapply(selected, function(i) {
                summary <- catalog$summaries[[catalog$source[[i]]]]
                c(uri = summary$uri, summary$definitions[[catalog$row[[i]]]])
            })
        }
    )
)
