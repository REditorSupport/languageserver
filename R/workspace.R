#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#' @keywords internal
Workspace <- R6::R6Class("Workspace",
    private = list(
        global_env = NULL,
        namespaces = list(),
        definition_cache = NULL,
        documentation = list(),
        parse_data = list()
    ),
    public = list(
        loaded_packages = c(
            "base", "stats", "methods", "utils", "graphics", "grDevices", "datasets"),

        initialize = function() {
            for (pkgname in self$loaded_packages) {
                private$namespaces[[pkgname]] <- Namespace$new(pkgname)
            }
            private$global_env <- GlobalNameSpace$new()
            private$definition_cache <- DefinitionCache$new()
        },

        load_package = function(pkgname) {
            if (!(pkgname %in% self$loaded_packages)) {
                ns <- self$get_namespace(pkgname)
                logger$info("ns: ", ns)
                if (!is.null(ns)) {
                    self$loaded_packages <- append(self$loaded_packages, pkgname)
                    logger$info("loaded_packages: ", self$loaded_packages)
                }
            }
        },

        load_packages = function(packages) {
            for (package in packages) {
                self$load_package(package)
            }
        },

        guess_namespace = function(object, isf = FALSE) {
            packages <- c(WORKSPACE, rev(self$loaded_packages))

            for (pkgname in packages) {
                ns <- self$get_namespace(pkgname)
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
            NULL
        },

        get_namespace = function(pkgname) {
            if (pkgname == WORKSPACE) {
                private$global_env
            } else if (pkgname %in% names(private$namespaces)) {
                private$namespaces[[pkgname]]
            } else if (length(find.package(pkgname, quiet = TRUE))) {
                private$namespaces[[pkgname]] <- Namespace$new(pkgname)
                private$namespaces[[pkgname]]
            } else {
                NULL
            }
        },

        get_signature = function(funct, pkgname = NULL, exported_only = TRUE) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(funct, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_signature(funct, exported_only = exported_only)
            }
        },

        get_formals = function(funct, pkgname = NULL, exported_only = TRUE) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(funct, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_formals(funct, exported_only = exported_only)
            }
        },

        get_help = function(topic, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(topic)
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
                enc2utf8(repr::repr_text(hfile))
            } else {
                NULL
            }
        },

        get_documentation = function(topic, pkgname = NULL, isf = FALSE) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_namespace(topic, isf = isf)
            }
            if (!is.null(pkgname) && pkgname == WORKSPACE) {
                # no documentation for workspace objects
                return(NULL)
            }

            item <- paste0(c(pkgname, topic), collapse = "::")
            if (!is.null(private$documentation[[item]])) {
                return(private$documentation[[item]])
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
                private$documentation[[item]] <- list(
                    title = title,
                    description = description,
                    arguments = arguments
                )
            } else {
                private$documentation[[item]] <- list()
            }
        },


        get_definition = function(symbol, pkgname = NULL, exported_only = TRUE) {
            if (is.null(pkgname)) {
                # look in global_env
                definition <- private$definition_cache$get(symbol)
                if (!is.null(definition)) {
                    return(definition)
                }
                pkgname <- self$guess_namespace(symbol, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_definition(symbol, exported_only = exported_only)
            }
        },

        get_definitions_for_uri = function(uri) {
            private$definition_cache$get_functs_for_uri(uri)
        },

        get_definitions_for_query = function(query) {
            private$definition_cache$filter(query)
        },

        get_xml_doc = function(uri) {
            private$parse_data[[uri]]$xml_doc
        },

        update_parse_data = function(uri, parse_data) {
            self$load_packages(parse_data$packages)
            if (!is.null(parse_data$xml_data)) {
                parse_data$xml_doc <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
            }
            private$parse_data[[uri]] <- parse_data
            private$global_env$update(private$parse_data)
            private$definition_cache$update(uri, parse_data$definition_ranges)
        }
    )
)
