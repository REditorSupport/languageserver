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
index_glob_match <- function(path, patterns, directory = FALSE) {
    if (!length(patterns)) return(FALSE)
    path <- gsub("\\", "/", path, fixed = TRUE)
    if (directory) path <- paste0(sub("/+$", "", path), "/")
    any(vapply(patterns, function(pattern) {
        is.character(pattern) && length(pattern) == 1L && nzchar(pattern) &&
            grepl(index_glob_regex(pattern), path, ignore.case = TRUE,
                perl = TRUE)
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
    visit <- function(node) {
        if (!is.call(node)) return(NULL)
        fun <- deparse(node[[1L]], nlines = 1L)
        if (fun %in% c(
                "source", "sys.source", "base::source", "base::sys.source")) {
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
            if (!is.null(spec)) result[[length(result) + 1L]] <<- spec
        }
        children <- as.list(node)[-1L]
        for (i in seq_along(children)) {
            if (identical(children[[i]], quote(expr = ))) next
            visit(children[[i]])
        }
        NULL
    }
    visit(expr)
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
index_shallow_summary <- function(path, content, workspace_root, metadata = NULL) {
    path <- index_normalize_path(path)
    workspace_root <- index_normalize_path(workspace_root)
    if (is.null(metadata)) metadata <- file.info(path)
    expressions <- tryCatch(
        parse(text = content, keep.source = TRUE),
        error = function(e) NULL
    )
    definitions <- list()
    source_specs <- list()
    if (!is.null(expressions)) {
        srcrefs <- attr(expressions, "srcref")
        expression_list <- as.list(expressions)
        for (i in seq_along(expression_list)) {
            expr <- expression_list[[i]]
            source_specs <- c(source_specs, index_source_specs(expr))
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
            definitions[[symbol]] <- list(
                name = symbol,
                type = get_expr_type(value),
                range = expr_range(srcref)
            )
        }
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
        sources = sources,
        source_candidates = source_candidates,
        source_candidate_exists = source_candidate_exists,
        parse_error = is.null(expressions),
        package_root = index_package_root(path, workspace_root)
    )
}

#' A bounded, project-wide index of R source metadata
#' @noRd
WorkspaceIndex <- R6::R6Class("WorkspaceIndex",
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

        initialize = function(root) {
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
            mode <- lsp_settings$get("index_mode")
            if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
                mode <- "auto"
            }
            self$enabled <- !is.null(self$root) &&
                !identical(tolower(mode), "off")
        },

        include_patterns = function() {
            value <- lsp_settings$get("index_include")
            if (!is.character(value) || !length(value)) "**/*.R" else value
        },

        exclude_patterns = function() {
            value <- lsp_settings$get("index_exclude")
            if (!is.character(value)) character() else value
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
            if (index_glob_match(rel, self$exclude_patterns(), directory)) {
                return(FALSE)
            }
            directory || index_glob_match(rel, self$include_patterns())
        },

        cache_file = function() {
            if (!isTRUE(lsp_settings$get("index_persistent_cache"))) return(NULL)
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
                if (!is.list(summary) || !file.exists(summary$path)) next
                info <- file.info(summary$path)
                if (!nrow(info) || is.na(info$size[[1L]]) ||
                        !identical(as.numeric(info$size[[1L]]), summary$size) ||
                        !identical(as.numeric(info$mtime[[1L]]), summary$mtime)) next
                candidates <- summary$source_candidates
                if (length(candidates)) {
                    current_exists <- vapply(candidates, function(uri) {
                        path <- path_from_uri(uri)
                        file.exists(path) && !dir.exists(path)
                    }, logical(1L))
                    if (is.null(summary$source_candidate_exists) || !identical(
                            unname(current_exists),
                            unname(summary$source_candidate_exists))) next
                }
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
                saveRDS(value, temp_file)
                if (file.exists(cache_file)) {
                    file.copy(temp_file, cache_file, overwrite = TRUE)
                } else {
                    file.rename(temp_file, cache_file)
                }
            }, error = function(e) FALSE)
            if (isTRUE(saved)) self$cache_dirty <- FALSE
            invisible(NULL)
        },

        discover = function() {
            if (!self$enabled || !dir.exists(self$root)) return(invisible(NULL))
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
                    if (!self$summaries$has(uri)) {
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
            invisible(NULL)
        },

        set_summary = function(summary) {
            uri <- summary$uri
            old_sources <- self$source_edges$get(uri, character())
            for (target in old_sources) {
                reverse <- setdiff(self$reverse_edges$get(target, character()), uri)
                if (length(reverse)) {
                    self$reverse_edges$set(target, reverse)
                } else if (self$reverse_edges$has(target)) {
                    self$reverse_edges$remove(target)
                }
            }
            self$summaries$set(uri, summary)
            self$source_edges$set(uri, summary$sources)
            for (target in summary$sources) {
                self$reverse_edges$set(target,
                    union(self$reverse_edges$get(target, character()), uri))
            }
            self$cache_dirty <- TRUE
            invisible(summary)
        },

        update_content = function(uri, content, metadata = NULL,
            cacheable = TRUE) {
            if (!self$enabled) return(NULL)
            path <- path_from_uri(uri)
            if (!self$should_index(path)) return(NULL)
            if (is.null(metadata)) metadata <- file.info(path)
            summary <- index_shallow_summary(
                path, content, self$root, metadata = metadata)
            uri <- index_canonical_uri(uri)
            summary$uri <- uri
            summary$cacheable <- isTRUE(cacheable)
            if (isTRUE(summary$parse_error) && self$summaries$has(uri)) {
                previous <- self$summaries$get(uri)
                summary$definitions <- previous$definitions
                summary$sources <- previous$sources
                summary$source_candidates <- previous$source_candidates
                summary$source_candidate_exists <-
                    previous$source_candidate_exists
            }
            self$set_summary(summary)
            if (!self$processing_batch) {
                self$pending <- setdiff(self$pending, uri)
            }
            if (!self$files$has(uri)) {
                self$files$set(uri, summary[c(
                    "uri", "path", "size", "mtime", "package_root")])
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
            old_sources <- self$source_edges$get(uri, character())
            for (target in old_sources) {
                reverse <- setdiff(self$reverse_edges$get(target, character()), uri)
                if (length(reverse)) self$reverse_edges$set(target, reverse)
                else if (self$reverse_edges$has(target)) self$reverse_edges$remove(target)
            }
            if (self$source_edges$has(uri)) self$source_edges$remove(uri)
            if (self$summaries$has(uri)) self$summaries$remove(uri)
            if (self$files$has(uri)) self$files$remove(uri)
            self$pending <- setdiff(self$pending, uri)
            self$cache_dirty <- TRUE
            invisible(NULL)
        },

        source_closure = function(uri) {
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
                for (target in self$source_edges$get(current, character())) {
                    queue$push(target)
                }
            }
            if (result_count) result[seq_len(result_count)] else character()
        },

        dependent_closure = function(uri) {
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
            if (result_count) result[seq_len(result_count)] else character()
        },

        dependents = function(uri, include_candidates = FALSE) {
            uri <- index_canonical_uri(uri)
            result <- self$reverse_edges$get(uri, character())
            if (isTRUE(include_candidates)) {
                for (summary in self$summaries$values()) {
                    candidates <- summary$source_candidates
                    if (!is.null(candidates) && uri %in% candidates) {
                        result <- union(result, summary$uri)
                    }
                }
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
            uris <- self$files$keys()
            keep <- vapply(uris, function(uri) {
                root <- self$files$get(uri)$package_root
                !is.null(root) && (is.null(package_root) || identical(root, package_root))
            }, logical(1L))
            uris[keep]
        },

        definitions_for_query = function(pattern) {
            result <- list()
            for (summary in self$summaries$values()) {
                definitions <- summary$definitions
                symbols <- names(definitions)
                matches <- symbols[fuzzy_find(symbols, pattern)]
                result <- c(result, lapply(unname(definitions[matches]), function(def) {
                    c(uri = summary$uri, def)
                }))
            }
            result
        }
    )
)
