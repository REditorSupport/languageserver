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
        trim = function() {
            while (private$entries$size() > private$max_entries ||
                    private$current_bytes > private$max_bytes) {
                keys <- private$entries$keys()
                if (!length(keys)) break
                self$remove(keys[[1L]])
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
        set = function(key, value) {
            if (private$entries$has(key)) self$remove(key)
            size <- as.numeric(object.size(value))
            # An individual value larger than the whole budget would evict
            # every useful entry and still leave the cache over budget.
            if (size > private$max_bytes) return(invisible(NULL))
            private$entries$set(key, value)
            private$sizes$set(key, size)
            private$current_bytes <- private$current_bytes + size
            private$trim()
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
            self$type_hierarchy_cache <- collections::dict()
        },

        load_package = function(pkgname) {
            if (!(pkgname %in% self$loaded_packages)) {
                ns <- self$get_namespace(pkgname)
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
                ns <- PackageNamespace$new(pkgname)
                self$namespaces$set(pkgname, ns)
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
                globals <- new.env(parent = emptyenv())
                package_root <- self$index$package_root_for_uri(uri)
                if (is.null(package_root) && is_package(self$root)) {
                    package_root <- self$root
                }
                uris <- if (is.null(package_root)) {
                    self$index$source_closure(uri)
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
                    doc_functions <- if (!is.null(doc) && !is.null(doc$parse_data)) {
                        doc$parse_data$functions
                    } else {
                        NULL
                    }
                    for (symbol in names(summary$definitions)) {
                        def <- summary$definitions[[symbol]]
                        fn <- if (!is.null(doc_functions) && !is.null(doc_functions[[symbol]])) {
                            doc_functions[[symbol]]
                        } else if (!is.null(def$funct)) {
                            def$funct
                        } else {
                            any_args_function
                        }
                        globals[[symbol]] <- fn
                    }
                }
                if (!is.null(package_root)) {
                    pkg_imports <- extract_package_imports(package_root)
                    imported_packages <- pkg_imports$packages
                    imported_objects <- names(pkg_imports$objects)
                    if (identical(
                        index_normalize_path(package_root),
                        index_normalize_path(self$root)
                    )) {
                        imported_packages <- union(
                            imported_packages, self$imported_packages)
                        imported_objects <- union(
                            imported_objects, self$imported_objects$keys())
                    }
                    populate_package_import_globals(
                        globals,
                        imported_packages = imported_packages,
                        imported_objects = imported_objects,
                        except_map = pkg_imports$except
                    )
                }
                return(globals)
            }
            if (!is.null(self$diagnostics_globals_cache)) {
                return(self$diagnostics_globals_cache)
            }
            globals <- new.env(parent = emptyenv())
            if (is_package(self$root)) {
                pkg_imports <- extract_package_imports(self$root)
                imported_packages <- union(
                    pkg_imports$packages, self$imported_packages)
                imported_objects <- union(
                    names(pkg_imports$objects), self$imported_objects$keys())
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
                    assign_diagnostics_globals(globals, parse_data$nonfuncts)
                    list2env(parse_data$functions, globals)
                }
                populate_package_import_globals(
                    globals,
                    imported_packages = imported_packages,
                    imported_objects = imported_objects,
                    except_map = pkg_imports$except
                )
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
                    definitions <- as.list(parse_data$definitions)
                    if (length(parse_data$functions)) {
                        for (symbol in intersect(names(definitions), names(parse_data$functions))) {
                            definitions[[symbol]]$funct <- parse_data$functions[[symbol]]
                        }
                    }
                    summary$definitions <- definitions
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
            if (!is.null(self$diagnostics_cache)) {
                self$diagnostics_cache$clear()
            }
            ns_imports <- extract_namespace_imports(self$root)
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

assign_diagnostics_globals <- function(globals, symbols, value = any_args_function) {
    symbols <- unique(symbols[nzchar(symbols)])
    for (symbol in symbols) {
        if (!exists(symbol, envir = globals, inherits = FALSE)) {
            globals[[symbol]] <- value
        }
    }
    globals
}

populate_package_import_globals <- function(globals,
    imported_packages = character(),
    imported_objects = character(),
    except_map = list()) {
    for (pkg in unique(imported_packages[nzchar(imported_packages)])) {
        exports <- tryCatch(getNamespaceExports(pkg), error = function(e) character())
        if (length(except_map[[pkg]])) {
            exports <- setdiff(exports, except_map[[pkg]])
        }
        assign_diagnostics_globals(globals, exports)
    }
    assign_diagnostics_globals(globals, imported_objects)
    globals
}

extract_namespace_imports <- function(package_root) {
    packages <- character()
    objects <- list()
    except_map <- list()
    namespace_file <- file.path(package_root, "NAMESPACE")
    if (!file.exists(namespace_file)) {
        return(list(packages = packages, objects = objects, except = except_map))
    }
    ns <- tryCatch(
        base::parseNamespaceFile(
            basename(package_root),
            dirname(package_root),
            mustExist = FALSE
        ),
        error = function(e) NULL
    )
    if (is.null(ns)) {
      return(list(packages = packages, objects = objects, except = except_map))
    }
    for (imp in ns$imports) {
        if (is.character(imp) && length(imp) == 1L) {
            packages <- c(packages, imp)
            next
        }
        if (!is.list(imp) || length(imp) == 0L) {
            next
        }
        pkg <- as.character(imp[[1L]])[1L]
        if ("except" %in% names(imp)) {
            packages <- c(packages, pkg)
            except_map[[pkg]] <- union(
                except_map[[pkg]],
                as.character(imp$except)
            )
        } else if (length(imp) >= 2L) {
            syms <- as.character(imp[[2L]])
            aliases <- names(imp[[2L]])
            if (!is.null(aliases)) {
                has_alias <- nzchar(aliases)
                syms[has_alias] <- aliases[has_alias]
            }
            for (sym in syms[nzchar(syms)]) {
                objects[[sym]] <- pkg
            }
        }
    }
    for (imp in ns$importMethods) {
        if (!is.list(imp) || length(imp) < 2L) {
            next
        }
        pkg <- as.character(imp[[1L]])[1L]
        syms <- as.character(imp[[2L]])
        syms <- syms[nzchar(syms)]
        for (sym in syms) {
            objects[[sym]] <- pkg
        }
    }
    list(
        packages = unique(packages[nzchar(packages)]),
        objects = objects,
        except = except_map
    )
}

extract_package_imports <- function(package_root) {
    if (is.null(package_root) || !length(package_root) || !nzchar(package_root)) {
        return(list(packages = character(), objects = list(), except = list()))
    }
    ns_imports <- extract_namespace_imports(package_root)
    packages <- ns_imports$packages

    desc_file <- file.path(package_root, "DESCRIPTION")
    if (file.exists(desc_file)) {
        depends <- tryCatch(
            read.dcf(desc_file, fields = "Depends")[1L, 1L],
            error = function(e) NA_character_
        )
        if (!is.na(depends) && nzchar(depends)) {
            dep_pkgs <- trimws(strsplit(depends, ",", fixed = TRUE)[[1L]])
            dep_pkgs <- trimws(sub("\\s*\\(.*\\)$", "", dep_pkgs))
            dep_pkgs <- setdiff(dep_pkgs[nzchar(dep_pkgs)], "R")
            packages <- c(dep_pkgs, packages)
        }
    }

    list(
        packages = unique(packages[nzchar(packages)]),
        objects = ns_imports$objects,
        except = ns_imports$except
    )
}
