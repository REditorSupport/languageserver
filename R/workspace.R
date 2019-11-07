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
        xml_docs = list()
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

        guess_package = function(object, isf = FALSE) {
            packages <- c(WORKSPACE, rev(self$loaded_packages))

            for (pkgname in packages) {
                ns <- self$get_namespace(pkgname)
                if (isf) {
                    if (!is.null(ns) && ns$exists(object)) {
                        logger$info("guess package:", pkgname)
                        return(pkgname)
                    }
                } else {
                    if (!is.null(ns) && ns$exists_funct(object)) {
                        logger$info("guess package:", pkgname)
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

        get_signature = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(funct, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_signature(funct)
            }
        },

        get_formals = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(funct, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_formals(funct)
            }
        },

        get_help = function(topic, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(topic)
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

        get_definition = function(token, pkg = NULL) {
            if (is.null(pkg)) {
                # look in file first
                definition <- private$definition_cache$get(token)
                if (is.null(definition)) {
                    definition <- self$get_definition_in_package(token)
                }
            } else {
                definition <- self$get_definition_in_package(token, pkg)
            }
            definition
        },

        get_definitions_for_uri = function(uri) {
            private$definition_cache$get_functs_for_uri(uri)
        },

        get_definitions_for_query = function(query) {
            private$definition_cache$filter(query)
        },

        get_definition_in_package = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(funct, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            code <- ns$get_body(funct)
            if (is.null(code)) {
                return(NULL)
            }

            # if the function exists in the workspace, write the code to a file
            temp_file <- file.path(tempdir(), paste0(funct, ".R"))
            readr::write_lines(code, temp_file)
            list(
                uri = path_to_uri(temp_file),
                range = range(
                    start = position(line = 0, character = 0),
                    end = position(line = stringr::str_count(code, "\n") + 1, character = 0)
                )
            )
        },

        get_xml_doc = function(uri) {
            private$xml_docs[[uri]]
        },

        update_parse_data = function(uri, parse_data) {
            self$load_packages(parse_data$packages)

            private$global_env$update(
                parse_data$nonfuncts,
                parse_data$functs,
                parse_data$signatures,
                parse_data$formals
            )
            private$definition_cache$update(uri, parse_data$definition_ranges)

            if (!is.null(parse_data$xml_data)) {
                private$xml_docs[[uri]] <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
            }
        }
    )
)
