startup_packages <- c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

workspace_startup_packages <- local({
    cached <- NULL
    function() {
        if (!is.null(cached)) return(cached)
        cached <<- tryCatch(
            callr::r(
                resolve_attached_packages,
                system_profile = TRUE,
                user_profile = TRUE,
                timeout = if (identical(Sys.getenv("R_COVR"), "true")) 30 else 3
            ),
            error = function(e) {
                logger$info("workspace initialize error: ", e)
                startup_packages
            }
        )
        cached
    }
})

#' Return semantic document scope with compatibility for lightweight fixtures
#' @noRd
workspace_document_uris <- function(workspace, uri = NULL) {
    if (is.function(workspace$document_uris_for_context)) {
        workspace$document_uris_for_context(uri)
    } else {
        workspace$documents$keys()
    }
}

#' Return documents that can reference a definition
#' @noRd
workspace_reference_document_uris <- function(workspace, definition_uri,
    context_uri = definition_uri) {
    if (is.function(workspace$document_uris_for_references)) {
        workspace$document_uris_for_references(definition_uri, context_uri)
    } else {
        workspace_document_uris(workspace, context_uri)
    }
}

#' A byte-bounded least-recently-used cache
#' @noRd
ByteLruCache <- R6::R6Class(
    "ByteLruCache",
    private = list(
        entries = NULL,
        sizes = NULL,
        current_bytes = 0,
        max_bytes = NULL,
        max_entries = NULL,
        trim = function(protect = character()) {
            while (private$entries$size() > private$max_entries ||
                    private$current_bytes > private$max_bytes) {
                keys <- private$entries$keys()
                if (!length(keys)) break
                candidates <- setdiff(keys, protect)
                self$remove(if (length(candidates)) candidates[[1L]] else keys[[1L]])
            }
        }
    ),
    public = list(
        initialize = function(max_bytes, max_entries = 10L) {
            private$entries <- collections::ordered_dict()
            private$sizes <- collections::dict()
            private$max_bytes <- max(as.numeric(max_bytes), 0)
            private$max_entries <- max(as.integer(max_entries), 1L)
        },
        has = function(key) private$entries$has(key),
        get = function(key, default = NULL) {
            if (!private$entries$has(key)) return(default)
            value <- private$entries$pop(key)
            private$entries$set(key, value)
            value
        },
        set = function(key, value, protect = character()) {
            if (private$entries$has(key)) self$remove(key)
            size <- as.numeric(object.size(value))
            # An individual value larger than the whole budget would evict
            # every useful entry and still leave the cache over budget.
            if (size > private$max_bytes) return(invisible(NULL))
            private$entries$set(key, value)
            private$sizes$set(key, size)
            private$current_bytes <- private$current_bytes + size
            private$trim(protect)
            invisible(value)
        },
        remove = function(key) {
            if (!private$entries$has(key)) return(invisible(NULL))
            private$entries$remove(key)
            size <- private$sizes$get(key, 0)
            private$sizes$remove(key)
            private$current_bytes <- max(private$current_bytes - size, 0)
            invisible(NULL)
        },
        clear = function() {
            private$entries$clear()
            private$sizes$clear()
            private$current_bytes <- 0
            invisible(NULL)
        },
        size = function() private$entries$size(),
        keys = function() private$entries$keys(),
        bytes = function() private$current_bytes
    )
)

# Keep compact, inert snapshots when decoded package indexes compete for space.
# A request can restore an evicted index without waiting for another package
# resolution task or requiring an edit to the document's library calls.
MemberMetadataCache <- R6::R6Class(
    "MemberMetadataCache",
    private = list(
        snapshots = NULL, indexes = NULL, summaries = NULL,
        max_summary_bytes = NULL,
        summary_sizes = function() {
            vapply(private$summaries$values(), function(cache) {
                if (is.null(cache$.bytes)) 0 else cache$.bytes
            }, numeric(1L))
        },
        reserve_summary = function(size) {
            if (size > private$max_summary_bytes) return(FALSE)
            sizes <- private$summary_sizes()
            total <- sum(sizes)
            for (key in private$summaries$keys()) {
                if (total + size <= private$max_summary_bytes) break
                cache <- private$summaries$get(key)
                # Clear the environment itself: decoded indexes may still
                # reference it after another package becomes most recent.
                total <- total - if (is.null(cache$.bytes)) 0 else cache$.bytes
                rm(list = setdiff(ls(cache, all.names = TRUE), ".reserve"), envir = cache)
                cache$.bytes <- 0
            }
            TRUE
        },
        summary_cache = function(key) {
            cache <- private$summaries$pop(key, NULL)
            if (is.null(cache)) cache <- new.env(parent = emptyenv())
            # Inference reserves space before each write, including decoded
            # cache hits, so retained summaries share one bounded budget.
            cache$.reserve <- private$reserve_summary
            private$summaries$set(key, cache)
            cache
        }
    ),
    public = list(
        initialize = function(max_bytes, max_entries = 16L) {
            private$snapshots <- ByteLruCache$new(max_bytes, max_entries)
            private$indexes <- ByteLruCache$new(max_bytes, max_entries)
            private$summaries <- collections::ordered_dict()
            private$max_summary_bytes <- min(max(as.numeric(max_bytes), 0), 4 * 1024^2)
        },
        has = function(key) private$snapshots$has(key),
        get = function(key, default = NULL) {
            if (!self$has(key)) return(default)
            snapshot <- private$snapshots$get(key)
            cache <- private$summary_cache(key)
            if (private$indexes$has(key)) return(private$indexes$get(key))
            # Snapshots were validated on insertion. Restoring their unchanged
            # syntax need not hash every package definition on each request.
            value <- unserialize(memDecompress(snapshot, "gzip"))
            value$cache <- cache
            private$indexes$set(key, value)
            value
        },
        set = function(key, value, protect = character()) {
            snapshot <- as.list(value)
            index <- member_index_thaw(snapshot)
            if (is.null(index)) return(invisible(NULL))
            # A replacement must use validated returns and fresh summaries,
            # even when the caller supplied an already decoded index.
            value <- as.list(index)
            snapshot$method_results <- index$method_results
            snapshot$cache <- NULL
            private$snapshots$set(key, memCompress(serialize(snapshot, NULL), "gzip"), protect)
            private$indexes$remove(key)
            private$summaries$pop(key, NULL)
            if (self$has(key)) {
                value$cache <- private$summary_cache(key)
                private$indexes$set(key, value, protect)
            }
            for (expired in setdiff(private$indexes$keys(), self$keys())) private$indexes$remove(expired)
            for (expired in setdiff(private$summaries$keys(), self$keys())) private$summaries$remove(expired)
            invisible(value)
        },
        remove = function(key) {
            private$snapshots$remove(key)
            private$indexes$remove(key)
            private$summaries$pop(key, NULL)
            invisible(NULL)
        },
        clear = function() {
            private$snapshots$clear()
            private$indexes$clear()
            private$summaries$clear()
            invisible(NULL)
        },
        size = function() private$snapshots$size(),
        keys = function() private$snapshots$keys(),
        bytes = function() private$snapshots$bytes() + private$indexes$bytes() + sum(private$summary_sizes())
    )
)

#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#' @noRd
Workspace <- R6::R6Class("Workspace",
    private = list(
        scope_index = NULL,
        scope_revision = NULL,
        scope_uris = NULL,
        scope_contexts = NULL,
        scope_references = NULL,
        scope_namespaces = NULL,
        scope_namespace_sets = NULL,
        import_globals_cache = NULL,
        refresh_scope_cache = function(uris) {
            revision <- self$index$revision
            if (is.null(private$scope_contexts) ||
                    !identical(private$scope_index, self$index) ||
                    !identical(private$scope_revision, revision) ||
                    !identical(private$scope_uris, uris)) {
                private$scope_index <- self$index
                private$scope_revision <- revision
                private$scope_uris <- uris
                private$scope_contexts <- collections::dict()
                private$scope_references <- collections::dict()
                private$scope_namespaces <- collections::dict()
                private$scope_namespace_sets <- collections::dict()
            }
        },
        import_package_bindings = function(bindings, pkg, except = character(),
            include_lazydata = FALSE) {
            ns <- tryCatch(self$get_namespace(pkg), error = function(e) NULL)
            if (is.null(ns)) return(FALSE)
            nonfuncts <- ns$get_symbols(want_functs = FALSE, exported_only = TRUE)
            if (include_lazydata) {
                nonfuncts <- c(nonfuncts, ns$get_lazydata())
            }
            for (symbol in setdiff(nonfuncts, except)) {
                bindings[[symbol]] <- NULL
            }
            for (symbol in setdiff(ns$get_symbols(want_functs = TRUE, exported_only = TRUE), except)) {
                bindings[[symbol]] <- ns$get_diagnostic_stub(symbol)
            }
            TRUE
        },
        package_import_bindings = function(package_root) {
            if (is.null(package_root) || !length(package_root) || !nzchar(package_root)) {
                return(list())
            }
            key <- index_normalize_path(package_root)
            desc_file <- file.path(package_root, "DESCRIPTION")
            ns_file <- file.path(package_root, "NAMESPACE")
            desc_mt <- if (file.exists(desc_file)) as.numeric(file.mtime(desc_file)) else NA_real_
            ns_mt <- if (file.exists(ns_file)) as.numeric(file.mtime(ns_file)) else NA_real_
            cached <- private$import_globals_cache$get(key, NULL)
            is_fresh <- !is.null(cached) &&
                identical(cached$desc_mt, desc_mt) &&
                identical(cached$ns_mt, ns_mt) &&
                !any(vapply(cached$missing_pkgs, function(pkg) {
                    !is.null(tryCatch(self$get_namespace(pkg), error = function(e) NULL))
                }, logical(1L)))
            if (is_fresh) {
                return(cached$bindings)
            }
            bindings <- new.env(hash = TRUE, parent = emptyenv())
            missing_pkgs <- character()
            for (pkg in extract_depends_packages(desc_file)) {
                if (!private$import_package_bindings(bindings, pkg, include_lazydata = TRUE)) {
                    missing_pkgs <- c(missing_pkgs, pkg)
                }
            }
            ns_imports <- extract_namespace_imports(ns_file)
            for (directive in ns_imports$directives) {
                if (directive$type == "import") {
                    for (pkg in directive$packages) {
                        if (!private$import_package_bindings(
                            bindings, pkg, except = directive$except
                        )) {
                            missing_pkgs <- c(missing_pkgs, pkg)
                        }
                    }
                } else {
                    ns <- tryCatch(self$get_namespace(directive$package), error = function(e) NULL)
                    if (is.null(ns)) {
                        missing_pkgs <- c(missing_pkgs, directive$package)
                    }
                    for (i in seq_along(directive$symbols)) {
                        sym <- directive$symbols[[i]]
                        target <- directive$targets[[i]]
                        bindings[[sym]] <- if (is.null(ns)) {
                            any_args_function
                        } else {
                            ns$get_diagnostic_stub(target)
                        }
                    }
                }
            }
            result <- as.list(bindings, all.names = TRUE)
            self$diagnostics_globals_cache <- NULL
            private$import_globals_cache$set(key, list(
                desc_mt = desc_mt,
                ns_mt = ns_mt,
                missing_pkgs = unique(missing_pkgs),
                bindings = result
            ))
            result
        }
    ),
    public = list(
        root = NULL,
        namespaces = NULL,
        global_env = NULL,
        documents = NULL,
        index = NULL,

        # from NAMESPACE importFrom()
        imported_objects = NULL,
        # from NAMESPACE import()
        imported_packages = NULL,
        namespace_file_mt = NULL,

        startup_packages = NULL,
        loaded_packages = NULL,
        help_cache = NULL,
        parse_cache = NULL,  # Performance: Cache parse results by content hash
        diagnostics_cache = NULL,  # Performance: Cache diagnostics by content hash
        diagnostics_globals_cache = NULL,
        type_hierarchy_cache = NULL,
        member_metadata = NULL,

        initialize = function(root) {
            self$root <- root
            self$documents <- collections::dict()
            self$index <- WorkspaceIndex$new(root)
            self$imported_objects <- collections::dict()
            self$imported_packages <- character(0)
            self$global_env <- GlobalEnv$new(self$documents)
            self$namespaces <- collections::dict()
            self$startup_packages <- workspace_startup_packages()
            self$loaded_packages <- self$startup_packages
            for (pkgname in self$loaded_packages) {
                self$namespaces$set(pkgname, PackageNamespace$new(pkgname))
            }
            self$help_cache <- collections::dict()
            parse_cache_mb <- lsp_settings$get("parse_cache_max_mb")
            if (!is.numeric(parse_cache_mb) || length(parse_cache_mb) != 1L ||
                    is.na(parse_cache_mb) || parse_cache_mb < 0) {
                parse_cache_mb <- 64
            }
            diagnostics_cache_mb <- lsp_settings$get(
                "diagnostics_cache_max_mb")
            if (!is.numeric(diagnostics_cache_mb) ||
                    length(diagnostics_cache_mb) != 1L ||
                    is.na(diagnostics_cache_mb) || diagnostics_cache_mb < 0) {
                diagnostics_cache_mb <- 16
            }
            self$parse_cache <- ByteLruCache$new(
                parse_cache_mb * 1024^2, max_entries = 10L)
            self$diagnostics_cache <- ByteLruCache$new(
                diagnostics_cache_mb * 1024^2, max_entries = 100L)
            self$diagnostics_globals_cache <- NULL
            private$import_globals_cache <- collections::dict()
            self$type_hierarchy_cache <- collections::dict()
            self$member_metadata <- MemberMetadataCache$new(32 * 1024^2, max_entries = 16L)
        },

        load_package = function(pkgname) {
            if (!(pkgname %in% self$loaded_packages)) {
                ns <- tryCatch(self$get_namespace(pkgname), error = function(e) NULL)
                logger$info("ns: ", ns)
                if (!is.null(ns)) {
                    self$loaded_packages <- c(self$loaded_packages, pkgname)
                    logger$info("loaded_packages: ", self$loaded_packages)
                }
            }
        },

        load_packages = function(packages) {
            for (package in packages) {
                self$load_package(package)
            }
        },

        document_uris_for_context = function(uri = NULL) {
            all_uris <- self$documents$keys()
            private$refresh_scope_cache(all_uris)
            if (is.null(uri) || !length(uri) || !nzchar(uri) ||
                    is.null(self$index) || !isTRUE(self$index$enabled)) {
                return(all_uris)
            }
            if (!self$index$contains_path(path_from_uri(uri))) {
                return(all_uris)
            }
            if (private$scope_contexts$has(uri)) {
                return(private$scope_contexts$get(uri))
            }
            package_root <- self$index$package_root_for_uri(uri)
            result <- if (!is.null(package_root)) {
                all_uris[vapply(all_uris, function(document_uri) {
                    identical(
                        self$index$package_root_for_uri(document_uri),
                        package_root
                    )
                }, logical(1L))]
            } else {
                closure <- self$index$source_closure(uri)
                all_uris[vapply(all_uris, function(document_uri) {
                    index_canonical_uri(document_uri) %in% closure
                }, logical(1L))]
            }
            private$scope_contexts$set(uri, result)
            result
        },

        document_uris_for_references = function(definition_uri,
            context_uri = definition_uri) {
            all_uris <- self$documents$keys()
            private$refresh_scope_cache(all_uris)
            if (is.null(definition_uri) || !length(definition_uri) ||
                    !nzchar(definition_uri) || is.null(self$index) ||
                    !isTRUE(self$index$enabled)) {
                return(self$document_uris_for_context(context_uri))
            }
            definition_path <- path_from_uri(definition_uri)
            if (!self$index$contains_path(definition_path)) {
                return(self$document_uris_for_context(context_uri))
            }
            if (private$scope_references$has(definition_uri)) {
                return(private$scope_references$get(definition_uri))
            }
            package_root <- self$index$package_root_for_uri(definition_uri)
            result <- if (!is.null(package_root)) {
                all_uris[vapply(all_uris, function(document_uri) {
                    identical(
                        self$index$package_root_for_uri(document_uri),
                        package_root
                    )
                }, logical(1L))]
            } else {
                closure <- self$index$dependent_closure(definition_uri)
                all_uris[vapply(all_uris, function(document_uri) {
                    index_canonical_uri(document_uri) %in% closure
                }, logical(1L))]
            }
            private$scope_references$set(definition_uri, result)
            result
        },

        loaded_packages_for_context = function(uri = NULL) {
            if (is.null(uri) || !length(uri) || !nzchar(uri) ||
                    is.null(self$index) || !isTRUE(self$index$enabled)) {
                return(self$loaded_packages)
            }
            packages <- union(self$startup_packages, self$imported_packages)
            for (document_uri in self$document_uris_for_context(uri)) {
                doc <- self$documents$get(document_uri, NULL)
                if (!is.null(doc)) packages <- union(packages, doc$loaded_packages)
            }
            packages
        },

        guess_namespace = function(object, isf = FALSE, uri = NULL) {
            if (!nzchar(object)) {
                return(NULL)
            }

            packages <- c(
                WORKSPACE,
                rev(self$loaded_packages_for_context(uri))
            )

            for (pkgname in packages) {
                ns <- self$get_namespace(pkgname, uri = uri)
                if (isf) {
                    if (!is.null(ns) && ns$exists_funct(object)) {
                        logger$info("guess namespace:", pkgname)
                        return(pkgname)
                    }
                } else {
                    if (!is.null(ns) && ns$exists(object)) {
                        logger$info("guess namespace:", pkgname)
                        return(pkgname)
                    }
                }
            }

            if (self$imported_objects$has(object)) {
                pkgname <- self$imported_objects$get(object)
                logger$info("object from:", pkgname)
                return(pkgname)
            }
            NULL
        },

        get_namespace = function(pkgname, uri = NULL) {
            if (pkgname == WORKSPACE) {
                if (is.null(uri)) {
                    self$global_env
                } else {
                    uris <- self$document_uris_for_context(uri)
                    if (!private$scope_namespaces$has(uri)) {
                        # Package files often share the same scope. Keep one
                        # aggregate symbol map for that set of documents.
                        key <- get_content_hash(uris)
                        if (!private$scope_namespace_sets$has(key)) {
                            private$scope_namespace_sets$set(key,
                                GlobalEnv$new(self$documents, uris))
                        }
                        private$scope_namespaces$set(uri,
                            private$scope_namespace_sets$get(key))
                    }
                    private$scope_namespaces$get(uri)
                }
            } else if (self$namespaces$has(pkgname)) {
                self$namespaces$get(pkgname)
            } else if (length(find.package(pkgname, quiet = TRUE))) {
                ns <- tryCatch(PackageNamespace$new(pkgname), error = function(e) NULL)
                if (!is.null(ns)) {
                    self$namespaces$set(pkgname, ns)
                }
                ns
            } else {
                NULL
            }
        },

        get_signature = function(funct, pkgname = NULL, exported_only = TRUE,
            uri = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(funct, isf = TRUE, uri = uri)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname, uri = uri)
            if (!is.null(ns)) {
                ns$get_signature(funct, exported_only = exported_only)
            }
        },

        get_formals = function(funct, pkgname = NULL, exported_only = TRUE,
            uri = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(funct, isf = TRUE, uri = uri)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname, uri = uri)
            if (!is.null(ns)) {
                ns$get_formals(funct, exported_only = exported_only)
            }
        },

        get_help = function(topic, pkgname = NULL, uri = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(topic, uri = uri)
            }
            # note: the parantheses are neccessary
            hfile <- tryCatch({
                    if (is.null(pkgname)) {
                        utils::help((topic))
                    } else {
                        utils::help((topic), (pkgname))
                    }
                },
                error = function(e) character(0)
            )

            if (length(hfile) > 0) {
                key <- as.character(hfile)
                if (self$help_cache$has(key)) {
                    return(self$help_cache$get(key))
                } else {
                    result <- NULL

                    if (lsp_settings$get("rich_documentation") &&
                            requireNamespace("rmarkdown", quietly = TRUE) &&
                            rmarkdown::pandoc_available()) {
                        html <- get_help(hfile, "html")
                        # Make header look prettier:
                        pattern <- "<table.*?<td>(.*?)\\s*{(.*?)}<\\/td>.*?<\\/table>\\n*<h2>\\s*(.*?)\\s*<\\/h2>"
                        replacement <- "<b>\\1</b> <i>{\\2}</i><p>\\3</p><hr/>"
                        html <- gsub(pattern, replacement, html, perl = TRUE)
                        result <- html_to_markdown(html)
                    }

                    if (is.null(result)) {
                        result <- get_help(hfile, "text")
                    }

                    if (!is.null(result)) {
                        self$help_cache$set(key, result)
                    }
                    return(result)
                }
            }
        },

        get_documentation = function(topic, pkgname = NULL, isf = FALSE,
            uri = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(topic, isf = isf, uri = uri)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname, uri = uri)
            if (!is.null(ns)) {
                ns$get_documentation(topic)
            }
        },

        get_definition = function(symbol, pkgname = NULL, exported_only = TRUE,
            uri = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(symbol, isf = FALSE, uri = uri)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname, uri = uri)
            if (!is.null(ns)) {
                ns$get_definition(symbol, exported_only = exported_only)
            }
        },

        get_definitions_for_uri = function(uri) {
            parse_data <- self$get_parse_data(uri)
            if (is.null(parse_data)) {
                return(list())
            }
            parse_data$definitions
        },

        get_definitions_for_query = function(pattern) {
            if (!is.null(self$index) && isTRUE(self$index$enabled)) {
                result <- self$index$definitions_for_query(pattern)
                indexed_uris <- self$index$summaries$keys()
                documents <- self$documents$values()
                documents <- documents[!vapply(documents, function(doc) {
                    index_canonical_uri(doc$uri) %in% indexed_uris
                }, logical(1L))]
            } else {
                result <- list()
                documents <- self$documents$values()
            }
            for (doc in documents) {
                parse_data <- doc$parse_data
                if (is.null(parse_data)) next
                symbols <- names(parse_data$definitions)
                matches <- symbols[fuzzy_find(symbols, pattern)]
                result <- c(result, lapply(
                    unname(parse_data$definitions[matches]),
                    function(def) {
                        c(uri = doc$uri, def)
                    }
                ))
            }
            result
        },

        get_parse_data = function(uri) {
            self$documents$get(uri, NULL)$parse_data
        },

        update_loaded_packages = function() {
            loaded_packages <- union(self$startup_packages, self$imported_packages)
            for (doc in self$documents$values()) {
                loaded_packages <- union(loaded_packages, doc$loaded_packages)
            }
            self$loaded_packages <- loaded_packages
        },

        get_diagnostics_globals = function(uri = NULL) {
            if (!is.null(uri) && !is.null(self$index) &&
                    isTRUE(self$index$enabled)) {
                if (!self$index$files$size()) {
                    self$index$discover()
                }
                package_root <- self$index$package_root_for_uri(uri)
                import_root <- if (!is.null(package_root)) {
                    package_root
                } else if (is_package(self$root)) {
                    self$root
                } else {
                    NULL
                }
                globals <- list2env(
                    private$package_import_bindings(import_root),
                    hash = TRUE,
                    parent = emptyenv()
                )
                uris <- if (is.null(package_root)) {
                    self$index$source_closure(uri, update = TRUE)
                } else {
                    self$index$package_source_uris(package_root)
                }
                for (summary_uri in uris) {
                    summary <- self$index$summaries$get(summary_uri, NULL)
                    if (is.null(summary)) {
                        summary <- self$index$update_path(path_from_uri(summary_uri))
                    }
                    if (is.null(summary)) next
                    doc <- self$documents$get(summary_uri, NULL)
                    parse_data <- if (!is.null(doc)) doc$parse_data else NULL
                    if (!is.null(parse_data)) {
                        for (symbol in parse_data$nonfuncts) {
                            if (nzchar(symbol) &&
                                    !exists(symbol, envir = globals, inherits = FALSE)) {
                                globals[[symbol]] <- NULL
                            }
                        }
                        for (symbol in names(parse_data$functions)) {
                            if (nzchar(symbol)) {
                                globals[[symbol]] <- parse_data$functions[[symbol]]
                            }
                        }
                    } else {
                        for (symbol in names(summary$definitions)) {
                            if (!nzchar(symbol)) next
                            fn <- if (!is.null(summary$functions)) {
                                summary$functions[[symbol]]
                            } else {
                                NULL
                            }
                            if (!is.null(fn)) {
                                globals[[symbol]] <- fn
                            } else if (!exists(symbol, envir = globals, inherits = FALSE)) {
                                globals[[symbol]] <- NULL
                            }
                        }
                    }
                }
                return(globals)
            }
            import_bindings <- if (is_package(self$root)) {
                private$package_import_bindings(self$root)
            } else {
                list()
            }
            if (!is.null(self$diagnostics_globals_cache)) {
                return(self$diagnostics_globals_cache)
            }
            globals <- list2env(
                import_bindings,
                hash = TRUE,
                parent = emptyenv()
            )
            if (is_package(self$root)) {
                source_dir <- normalizePath(
                    file.path(self$root, "R"),
                    winslash = "/",
                    mustWork = FALSE
                )
                for (doc in self$documents$values()) {
                    document_dir <- normalizePath(
                        dirname(path_from_uri(doc$uri)),
                        winslash = "/",
                        mustWork = FALSE
                    )
                    if (document_dir != source_dir) next
                    parse_data <- doc$parse_data
                    if (is.null(parse_data)) next
                    for (symbol in parse_data$nonfuncts) {
                        if (nzchar(symbol) &&
                                !exists(symbol, envir = globals, inherits = FALSE)) {
                            globals[[symbol]] <- NULL
                        }
                    }
                    list2env(parse_data$functions, globals)
                }
            }
            self$diagnostics_globals_cache <- globals
            globals
        },

        update_parse_data = function(uri, parse_data) {
            self$diagnostics_globals_cache <- NULL
            self$type_hierarchy_cache$clear()
            # IMPORTANT: Always create xml_doc in the main process from xml_data
            # parse_document runs in a child process and cannot create xml_doc there
            # because xml2 external pointers cannot cross process boundaries
            if (!is.null(parse_data$xml_data)) {
                parse_data$xml_doc <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
                if (!is.null(parse_data$xml_doc)) {
                    attr(parse_data$xml_doc, "top_level_index") <-
                        xdoc_top_level_index(parse_data$xml_doc)
                }
            }
            self$documents$get(uri)$update_parse_data(parse_data)
            if (!is.null(self$index) && isTRUE(self$index$enabled)) {
                doc <- self$documents$get(uri)
                index_uri <- index_canonical_uri(uri)
                previous <- self$index$summaries$get(index_uri, NULL)
                cacheable <- if (is.null(previous)) {
                    !isTRUE(doc$is_open)
                } else {
                    !identical(previous$cacheable, FALSE)
                }
                summary <- self$index$update_content(
                    uri, doc$content, cacheable = cacheable,
                    parse_data = if (is.null(parse_data$source_specs)) NULL else
                        parse_data)
                if (is.null(parse_data$source_specs) && !is.null(summary) &&
                        !isTRUE(parse_data$parse_error)) {
                    summary$definitions <- as.list(parse_data$definitions)
                    summary$functions <- as.list(parse_data$functions)
                    self$index$set_summary(summary)
                }
            }
        },

        import_from_namespace_file = function() {
            if (length(self$root) == 0) {
                return(NULL)
            }
            namespace_file <- file.path(self$root, "NAMESPACE")
            if (!file.exists(namespace_file)) {
                return(NULL)
            }
            namespace_file_mt <- file.mtime(namespace_file)
            if (is.na(namespace_file_mt)) {
                return(NULL)
            }
            self$namespace_file_mt <- namespace_file_mt
            self$diagnostics_globals_cache <- NULL
            private$import_globals_cache$clear()
            if (!is.null(self$diagnostics_cache)) {
                self$diagnostics_cache$clear()
            }
            ns_imports <- extract_namespace_imports(namespace_file)
            if (length(ns_imports$packages)) {
                logger$info("load packages:", ns_imports$packages)
                self$load_packages(ns_imports$packages)
                self$imported_packages <- c(
                    self$imported_packages, ns_imports$packages)
            }
            for (object in names(ns_imports$objects)) {
                self$imported_objects$set(object, ns_imports$objects[[object]])
            }
            self$update_loaded_packages()
        },

        poll_namespace_file = function() {
            if (length(self$root) == 0) {
                return(NULL)
            }
            namespace_file <- file.path(self$root, "NAMESPACE")
            if (!file.exists(namespace_file)) {
                if (!is.null(self$namespace_file_mt)) {
                    self$namespace_file_mt <- NULL
                    self$imported_objects$clear()
                    self$imported_packages <- character(0)
                    self$diagnostics_globals_cache <- NULL
                    private$import_globals_cache$clear()
                    if (!is.null(self$diagnostics_cache)) {
                        self$diagnostics_cache$clear()
                    }
                    self$update_loaded_packages()
                    return(invisible(TRUE))
                }
                return(NULL)
            }
            namespace_file_mt <- file.mtime(namespace_file)
            # avoid change that is too recent
            if (is.na(namespace_file_mt) || Sys.time() - namespace_file_mt < 1) {
                return(NULL)
            }
            if (is.null(self$namespace_file_mt) || self$namespace_file_mt < namespace_file_mt) {
                self$imported_objects$clear()
                self$imported_packages <- character(0)
                self$import_from_namespace_file()
                return(invisible(TRUE))
            }
            NULL
        }
    )
)

extract_depends_packages <- function(desc_file) {
    if (!file.exists(desc_file)) {
        return(character())
    }
    depends <- tryCatch(
        read.dcf(desc_file, fields = "Depends")[1L, 1L],
        error = function(e) NA_character_
    )
    if (is.na(depends) || !nzchar(depends)) {
        return(character())
    }
    dep_pkgs <- trimws(strsplit(depends, ",", fixed = TRUE)[[1L]])
    dep_pkgs <- trimws(sub("\\s*\\([\\s\\S]*\\)$", "", dep_pkgs, perl = TRUE))
    setdiff(dep_pkgs[nzchar(dep_pkgs)], "R")
}

extract_namespace_imports <- function(namespace_file) {
    packages <- character()
    objects <- list()
    directives <- list()
    if (!file.exists(namespace_file)) {
        return(list(
            packages = packages,
            objects = objects,
            directives = directives
        ))
    }
    exprs <- tryCatch(
        parse(namespace_file, keep.source = FALSE),
        error = function(e) NULL
    )
    as_char <- function(args) {
        vapply(args, function(x) {
            if (is.character(x) || is.name(x)) as.character(x)[1L] else ""
        }, character(1L), USE.NAMES = FALSE)
    }
    parse_directive <- function(expr) {
        if (!is.call(expr) || !length(expr)) return(invisible(NULL))
        op <- as.character(expr[[1L]])[1L]
        if (op == "{") {
            for (sub_expr in as.list(expr[-1L])) {
                parse_directive(sub_expr)
            }
            return(invisible(NULL))
        }
        if (op == "if") {
            cond <- tryCatch(
                isTRUE(eval(expr[[2L]], baseenv())),
                error = function(e) NA
            )
            if (is.na(cond)) {
                if (length(expr) >= 3L) parse_directive(expr[[3L]])
                if (length(expr) >= 4L) parse_directive(expr[[4L]])
            } else if (cond) {
                if (length(expr) >= 3L) parse_directive(expr[[3L]])
            } else if (length(expr) >= 4L) {
                parse_directive(expr[[4L]])
            }
            return(invisible(NULL))
        }
        if (op %in% c("=", "<-") && length(expr) >= 3L) {
            parse_directive(expr[[3L]])
            return(invisible(NULL))
        }
        if (length(expr) < 2L) return(invisible(NULL))
        args <- as.list(expr[-1L])
        arg_names <- names(args)
        if (op == "import") {
            is_except <- if (is.null(arg_names)) {
                rep(FALSE, length(args))
            } else {
                arg_names == "except"
            }
            pkgs <- as_char(args[!is_except])
            pkgs <- pkgs[nzchar(pkgs)]
            if (!length(pkgs)) return(invisible(NULL))
            except <- if (any(is_except)) {
                ex_expr <- args[is_except][[1L]]
                if (is.call(ex_expr) && identical(ex_expr[[1L]], quote(c))) {
                    as_char(as.list(ex_expr[-1L]))
                } else {
                    as_char(list(ex_expr))
                }
            } else {
                character()
            }
            except <- except[nzchar(except)]
            packages <<- c(packages, pkgs)
            directives[[length(directives) + 1L]] <<- list(
                type = "import",
                packages = pkgs,
                except = except
            )
        } else if (op %in% c("importFrom", "importMethodsFrom") && length(args) >= 2L) {
            pkg <- as_char(args[1L])
            if (!nzchar(pkg)) return(invisible(NULL))
            rest <- args[-1L]
            if (!is.null(names(rest)) && "except" %in% names(rest)) {
                return(invisible(NULL))
            }
            targets <- as_char(rest)
            aliases <- names(rest)
            syms <- targets
            if (!is.null(aliases)) {
                has_alias <- nzchar(aliases)
                syms[has_alias] <- aliases[has_alias]
            }
            keep <- nzchar(syms) & nzchar(targets)
            syms <- syms[keep]
            targets <- targets[keep]
            if (!length(syms)) return(invisible(NULL))
            if (op == "importFrom") {
                for (sym in syms) {
                    objects[[sym]] <<- pkg
                }
            }
            directives[[length(directives) + 1L]] <<- list(
                type = op,
                package = pkg,
                symbols = syms,
                targets = targets
            )
        }
    }
    for (expr in as.list(exprs)) {
        parse_directive(expr)
    }
    list(
        packages = unique(packages),
        objects = objects,
        directives = directives
    )
}
