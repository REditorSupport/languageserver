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
            if (is.null(uri) || !length(uri) || !nzchar(uri) ||
                    is.null(self$index) || !isTRUE(self$index$enabled)) {
                return(all_uris)
            }
            if (!self$index$contains_path(path_from_uri(uri))) {
                return(all_uris)
            }
            package_root <- self$index$package_root_for_uri(uri)
            if (!is.null(package_root)) {
                return(all_uris[vapply(all_uris, function(document_uri) {
                    identical(
                        self$index$package_root_for_uri(document_uri),
                        package_root
                    )
                }, logical(1L))])
            }
            closure <- self$index$source_closure(uri)
            all_uris[vapply(all_uris, function(document_uri) {
                index_canonical_uri(document_uri) %in% closure
            }, logical(1L))]
        },

        document_uris_for_references = function(definition_uri,
            context_uri = definition_uri) {
            all_uris <- self$documents$keys()
            if (is.null(definition_uri) || !length(definition_uri) ||
                    !nzchar(definition_uri) || is.null(self$index) ||
                    !isTRUE(self$index$enabled)) {
                return(self$document_uris_for_context(context_uri))
            }
            definition_path <- path_from_uri(definition_uri)
            if (!self$index$contains_path(definition_path)) {
                return(self$document_uris_for_context(context_uri))
            }
            package_root <- self$index$package_root_for_uri(definition_uri)
            if (!is.null(package_root)) {
                return(all_uris[vapply(all_uris, function(document_uri) {
                    identical(
                        self$index$package_root_for_uri(document_uri),
                        package_root
                    )
                }, logical(1L))])
            }
            closure <- self$index$dependent_closure(definition_uri)
            all_uris[vapply(all_uris, function(document_uri) {
                index_canonical_uri(document_uri) %in% closure
            }, logical(1L))]
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
                    GlobalEnv$new(
                        self$documents,
                        self$document_uris_for_context(uri)
                    )
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
                globals <- new.env(parent = emptyenv())
                package_root <- self$index$package_root_for_uri(uri)
                uris <- if (is.null(package_root)) {
                    self$index$source_closure(uri)
                } else {
                    self$index$package_source_uris(package_root)
                }
                for (summary_uri in uris) {
                    summary <- self$index$summaries$get(summary_uri, NULL)
                    if (is.null(summary)) next
                    for (symbol in names(summary$definitions)) {
                        globals[[symbol]] <- NULL
                    }
                }
                return(globals)
            }
            if (!is.null(self$diagnostics_globals_cache)) {
                return(self$diagnostics_globals_cache)
            }
            globals <- new.env(parent = emptyenv())
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
                        globals[[symbol]] <- NULL
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
                    uri, doc$content, cacheable = cacheable)
                if (!is.null(summary) && !isTRUE(parse_data$parse_error)) {
                    summary$definitions <- as.list(parse_data$definitions)
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
            exprs <- tryCatch(
                parse(namespace_file),
                error = function(e) list())
            for (expr in exprs) {
                if (!is.call(expr) || !is.name(expr[[1]])) {
                    next
                }
                if (expr[[1]] == "import") {
                    packages <- as.list(expr[-1])
                    if (is.null(names(packages))) {
                        packages <- as.character(packages)
                    } else {
                        # handle import(foo, except = c(bar))
                        packages <- as.character(packages[names(packages) == ""])
                    }
                    logger$info("load packages:", packages)
                    self$load_packages(packages)
                    self$imported_packages <- c(self$imported_packages, packages)
                } else if (expr[[1]] == "importFrom") {
                    package <- as.character(expr[[2]])
                    objects <- as.character(expr[3:length(expr)])
                    logger$info("load package objects:", package, objects)
                    for (object in objects) {
                        self$imported_objects$set(object, package)
                    }
                }
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
            }
        }
    )
)
