#' A class for storing package information
#' @noRd
PackageNamespace <- R6::R6Class("PackageNamespace",
    private = list(
        namespace = NULL,
        membership = NULL,
        function_metadata = NULL,
        documentation = NULL,
        objects = character(0),
        functs = character(0),
        nonfuncts = character(0),
        exports = character(0),
        exported_functs = character(0),
        exported_nonfuncts = character(0),
        lazydata = character(0),

        ensure_namespace = function() {
            # Development tools can unload and reload a package while the
            # workspace keeps the same PackageNamespace object.
            if (!identical(getNamespace(self$package_name), private$namespace)) {
                self$initialize(self$package_name)
            }
        },

        function_info = function(funct, exported_only) {
            if (!self$exists_funct(funct, exported_only = exported_only)) {
                return(NULL)
            }
            fn <- get(funct, envir = private$namespace)
            if (!is.function(fn)) return(NULL)
            cached <- private$function_metadata$get(funct, NULL)
            # Identity also detects assignInNamespace()/trace() replacements.
            if (!is.null(cached) && identical(cached$fn, fn)) return(cached)
            args <- args(fn)
            sig <- NULL
            if (!is.null(args)) {
                sig <- format(args)
                sig <- sig[-length(sig)]
                sig <- paste0(trimws(sig, which = "left"), collapse = "")
                sig <- gsub("^function\\s*", funct, sig)
            }
            value <- list(fn = fn, signature = sig,
                formals = if (is.null(args)) NULL else
                    formals(if (is.primitive(fn)) args else fn))
            private$function_metadata$set(funct, value)
            value
        }
    ),
    public = list(
        package_name = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            private$namespace <- ns
            exports <- getNamespaceExports(ns)
            objects <- union(names(ns), exports)
            private$objects <- sanitize_names(objects)
            is_function <- vapply(private$objects, function(x) {
                is.function(get0(x, ns))
            }, logical(1L), USE.NAMES = FALSE)
            is_exported <- private$objects %in% exports
            private$functs <- private$objects[is_function]
            private$nonfuncts <- private$objects[!is_function]
            private$exports <- private$objects[is_exported]
            private$exported_functs <- private$objects[is_exported & is_function]
            private$exported_nonfuncts <- private$objects[is_exported & !is_function]
            flags <- 1L + as.integer(is_function) + 2L * as.integer(is_exported)
            private$membership <- list2env(
                stats::setNames(as.list(flags), private$objects),
                hash = TRUE, parent = emptyenv())
            private$function_metadata <- collections::dict()
            private$lazydata <- as.character(names(.getNamespaceInfo(ns, "lazydata")))
            private$documentation <- collections::dict()
        },

        exists = function(objname, exported_only = TRUE) {
            private$ensure_namespace()
            if (!is.character(objname) || length(objname) != 1L ||
                    is.na(objname) || !nzchar(objname)) {
                return(objname %in% if (exported_only) private$exports else private$objects)
            }
            flag <- get0(objname, private$membership, inherits = FALSE, ifnotfound = 0L)
            flag >= if (exported_only) 3L else 1L
        },

        exists_funct = function(funct, exported_only = TRUE) {
            private$ensure_namespace()
            if (!is.character(funct) || length(funct) != 1L ||
                    is.na(funct) || !nzchar(funct)) {
                return(funct %in% if (exported_only) private$exported_functs else private$functs)
            }
            flag <- get0(funct, private$membership, inherits = FALSE, ifnotfound = 0L)
            if (exported_only) flag == 4L else flag == 2L || flag == 4L
        },

        get_symbols = function(want_functs = TRUE, exported_only = TRUE) {
            private$ensure_namespace()
            if (want_functs && exported_only) {
                private$exported_functs
            } else if (!want_functs && exported_only) {
                private$exported_nonfuncts
            } else if (want_functs && !exported_only) {
                private$functs
            } else if (!want_functs && !exported_only) {
                private$nonfuncts
            }
        },

        get_lazydata = function() {
            private$ensure_namespace()
            private$lazydata
        },

        get_signature = function(funct, exported_only = TRUE) {
            private$function_info(funct, exported_only)$signature
        },

        get_formals = function(funct, exported_only = TRUE) {
            private$function_info(funct, exported_only)$formals
        },

        get_documentation = function(topic) {
            private$ensure_namespace()
            pkgname <- self$package_name
            if (private$documentation$has(topic)) {
                return(private$documentation$get(topic))
            }
            hfile <- utils::help((topic), (pkgname))

            if (length(hfile) > 0) {
                doc <- get_help_rd(hfile)
                title_item <- find_doc_item(doc, "\\title")
                description_item <- find_doc_item(doc, "\\description")
                arguments_item <- find_doc_item(doc, "\\arguments")
                title <- convert_doc_string(title_item)
                description <- convert_doc_string(description_item)
                arguments <- list()
                if (length(arguments_item)) {
                    arg_items <- arguments_item[vapply(arguments_item,
                        function(arg) attr(arg, "Rd_tag") == "\\item", logical(1L))]
                    arg_names <- vapply(arg_items, function(item) {
                        argname <- item[[1]][[1]]
                        switch(attr(argname, "Rd_tag"),
                            TEXT = argname, "\\dots" = "...", "")
                    }, character(1L))
                    names(arg_items) <- arg_names
                    arguments <- lapply(arg_items, function(item) {
                        convert_doc_string(item[[2]])
                    })
                }
                value <- list(
                    title = title,
                    description = description,
                    arguments = arguments
                )
            } else {
                value <- list()
            }
            private$documentation$set(topic, value)
            value
        },

        get_definition = function(symbol, exported_only = TRUE) {
            code <- self$get_body(symbol, exported_only = exported_only)
            if (is.null(code)) {
                return(NULL)
            }

            # if the function exists in the workspace, write the code to a file
            temp_file <- file.path(tempdir(), paste0(symbol, ".R"))
            stringi::stri_write_lines(c(
                "# Generated from function body. Editing this file has no effect.",
                code
            ), temp_file)
            def <- list(
                uri = path_to_uri(temp_file),
                range = range(
                    start = position(line = 0, character = 0),
                    end = position(line = length(code) + 1, character = 0)
                )
            )
            attr(def, "namespace") <- self$package_name
            def
        },

        get_body = function(funct, exported_only = TRUE) {
            if (!self$exists_funct(funct, exported_only = exported_only)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            if (is.primitive(fn)) {
                code <- utils::capture.output(print(fn))
            } else {
                code <- deparse(fn)
            }
            code
        },

        print = function() {
            cat(paste0("Namespace: ", self$package_name))
        }
    )
)

WORKSPACE <- "_workspace_"

#' A class for storing global environment information
#' @noRd
GlobalEnv <- R6::R6Class("GlobalEnv",
    private = list(
        parse_snapshot = NULL,
        uri_snapshot = NULL,
        functs = character(),
        nonfuncts = character(),
        function_sources = NULL,
        object_sources = NULL,
        symbols = NULL,

        refresh = function() {
            documents <- self$document_values()
            parses <- lapply(documents, function(doc) doc$parse_data)
            uris <- vapply(documents, function(doc) doc$uri, character(1L))
            # Keep parse values, rather than mutable Document environments, so
            # direct parse_data replacement and document removal invalidate us.
            if (identical(parses, private$parse_snapshot) &&
                    identical(uris, private$uri_snapshot)) return(invisible(NULL))
            private$parse_snapshot <- parses
            private$uri_snapshot <- uris
            collect <- function(field) {
                unlist(lapply(parses, "[[", field), use.names = FALSE)
            }
            function_names <- collect("functs")
            object_names <- collect("objects")
            private$functs <- unique(as.character(function_names))
            private$nonfuncts <- unique(as.character(collect("nonfuncts")))
            source_index <- function(field, symbols) {
                sources <- rep.int(seq_along(parses),
                    vapply(parses, function(data) length(data[[field]]), integer(1L)))
                keep <- !duplicated(symbols)
                list2env(stats::setNames(as.list(sources[keep]), symbols[keep]),
                    hash = TRUE, parent = emptyenv())
            }
            private$function_sources <- source_index("functs", function_names)
            private$object_sources <- source_index("objects", object_names)
            symbols <- unique(c(private$functs, private$nonfuncts))
            private$symbols <- list2env(
                stats::setNames(rep(list(TRUE), length(symbols)), symbols),
                hash = TRUE, parent = emptyenv())
            invisible(NULL)
        },

        source = function(symbol, is_function = FALSE) {
            if (length(symbol) != 1L || is.na(symbol) || !nzchar(symbol)) return(NULL)
            private$refresh()
            index <- if (is_function) private$function_sources else private$object_sources
            get0(symbol, index, inherits = FALSE)
        }
    ),
    public = list(
        documents = NULL,
        document_uris = NULL,
        package_name = NULL,

        initialize = function(documents, document_uris = NULL) {
            self$documents <- documents
            self$document_uris <- document_uris
            self$package_name <- WORKSPACE
        },

        document_values = function() {
            if (is.null(self$document_uris)) return(self$documents$values())
            values <- lapply(self$document_uris, function(uri) self$documents$get(uri, NULL))
            Filter(Negate(is.null), values)
        },

        exists = function(objname, exported_only = TRUE) {
            if (length(objname) != 1L || is.na(objname) || !nzchar(objname)) return(FALSE)
            private$refresh()
            exists(objname, envir = private$symbols, inherits = FALSE)
        },

        exists_funct = function(funct, exported_only = TRUE) {
            !is.null(private$source(funct, is_function = TRUE))
        },

        get_symbols = function(want_functs = TRUE, exported_only = TRUE) {
            private$refresh()
            if (want_functs) private$functs else private$nonfuncts
        },

        get_lazydata = function() {
            character(0)
        },

        get_signature = function(funct, exported_only = TRUE) {
            source <- private$source(funct, is_function = TRUE)
            if (!is.null(source)) private$parse_snapshot[[source]]$signatures[[funct]]
        },

        get_formals = function(funct, exported_only = TRUE) {
            source <- private$source(funct, is_function = TRUE)
            if (!is.null(source)) formals(private$parse_snapshot[[source]]$functions[[funct]])
        },

        get_documentation = function(topic) {
            source <- private$source(topic)
            if (!is.null(source)) private$parse_snapshot[[source]]$documentation[[topic]]
        },

        get_definition = function(symbol, exported_only = TRUE) {
            source <- private$source(symbol)
            if (!is.null(source)) {
                location(uri = private$uri_snapshot[[source]],
                    range = private$parse_snapshot[[source]]$definitions[[symbol]]$range)
            }
        }
    )
)


resolve_attached_packages <- function(pkgs = NULL) {
    for (pkg in pkgs) {
        tryCatch(library(pkg, character.only = TRUE),
            error = function(e) NULL
        )
    }
    rev(.packages())
}
