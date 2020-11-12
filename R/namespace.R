#' A class for storing package information
#' @keywords internal
PackageNamespace <- R6::R6Class("PackageNamespace",
    private = list(
        documentation = NULL,
        objects = character(0),
        functs = character(0),
        nonfuncts = character(0),
        exports = character(0),
        exported_functs = character(0),
        exported_nonfuncts = character(0),
        lazydata = character(0)
    ),
    public = list(
        package_name = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            private$objects <- sanitize_names(objects(ns, all.names = TRUE))
            is_function <- vapply(private$objects, function(x) {
                        is.function(get(x, envir = ns))}, logical(1L), USE.NAMES = FALSE)
            is_exported <- private$objects %in% sanitize_names(getNamespaceExports(ns))
            private$functs <- private$objects[is_function]
            private$nonfuncts <- private$objects[!is_function]
            private$exports <- private$objects[is_exported]
            private$exported_functs <- private$objects[is_exported & is_function]
            private$exported_nonfuncts <- private$objects[is_exported & !is_function]
            private$lazydata <- as.character(names(.getNamespaceInfo(ns, "lazydata")))
            private$documentation <- collections::dict()
        },

        exists = function(objname, exported_only = TRUE) {
            if (exported_only) {
                objname %in% private$exports
            } else {
                objname %in% private$objects
            }
        },

        exists_funct = function(funct, exported_only = TRUE) {
            if (exported_only) {
                funct %in% private$exported_functs
            } else {
                funct %in% private$functs
            }
        },

        get_symbols = function(want_functs = TRUE, exported_only = TRUE) {
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
            private$lazydata
        },

        get_signature = function(funct, exported_only = TRUE) {
            if (!self$exists_funct(funct, exported_only = exported_only)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            args <- args(fn)
            if (!is.null(args)) {
                sig <- format(args)
                sig <- sig[-length(sig)]
                sig <- paste0(trimws(sig, which = "left"), collapse = "")
                sig <- gsub("^function\\s*", funct, sig)
                sig
            }
        },

        get_formals = function(funct, exported_only = TRUE) {
            if (!self$exists_funct(funct, exported_only = exported_only)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            formals(fn)
        },

        get_documentation = function(topic) {
            pkgname <- self$package_name
            if (private$documentation$has(topic)) {
                return(private$documentation$get(topic))
            }
            hfile <- utils::help((topic), (pkgname))

            if (length(hfile) > 0) {
                doc <- utils:::.getHelpFile(hfile)
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
            list(
                uri = path_to_uri(temp_file),
                range = range(
                    start = position(line = 0, character = 0),
                    end = position(line = length(code) + 1, character = 0)
                )
            )
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
#' @keywords internal
GlobalEnv <- R6::R6Class("GlobalEnv",
    public = list(
        documents = NULL,
        package_name = NULL,

        initialize = function(documents) {
            self$documents <- documents
            self$package_name <- WORKSPACE
        },

        exists = function(objname, exported_only = TRUE) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (objname %in% doc$parse_data$nonfuncts) {
                        return(TRUE)
                    } else if (objname %in% doc$parse_data$functs) {
                        return(TRUE)
                    }
                }
            }
            return(FALSE)
        },

        exists_funct = function(funct, exported_only = TRUE) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (funct %in% doc$parse_data$functs) {
                        return(TRUE)
                    }
                }
            }
            return(FALSE)
        },

        get_symbols = function(want_functs = TRUE, exported_only = TRUE) {
            symbols <- character(0)
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (want_functs) {
                        symbols <- c(symbols, doc$parse_data$functs)
                    } else {
                        symbols <- c(symbols, doc$parse_data$nonfuncts)
                    }
                }
            }
            unique(symbols)
        },

        get_lazydata = function() {
            character(0)
        },

        get_signature = function(funct, exported_only = TRUE) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (funct %in% doc$parse_data$functs) {
                        return(doc$parse_data$signatures[[funct]])
                    }
                }
            }
            NULL
        },

        get_formals = function(funct, exported_only = TRUE) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (funct %in% doc$parse_data$functs) {
                        return(doc$parse_data$formals[[funct]])
                    }
                }
            }
            NULL
        },

        get_documentation = function(topic) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (topic %in% doc$parse_data$objects) {
                        return(doc$parse_data$documentation[[topic]])
                    }
                }
            }
            NULL
        },

        get_definition = function(symbol, exported_only = TRUE) {
            for (doc in self$documents$values()) {
                if (!is.null(doc$parse_data)) {
                    if (symbol %in% doc$parse_data$objects) {
                        def <- location(
                            uri = doc$uri,
                            range = doc$parse_data$definitions[[symbol]]$range
                        )
                        return(def)
                    }
                }
            }
            NULL
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
