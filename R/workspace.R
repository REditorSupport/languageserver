startup_packages <- c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#' @keywords internal
Workspace <- R6::R6Class("Workspace",
    public = list(
        namespaces = NULL,
        global_env = NULL,
        documents = NULL,
        loaded_packages = startup_packages,

        initialize = function() {
            self$documents <- collections::Dict()
            self$global_env <- GlobalEnv$new(self$documents)
            self$namespaces <- collections::Dict()
            for (pkgname in self$loaded_packages) {
                self$namespaces$set(pkgname, PackageNamespace$new(pkgname))
            }
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
                self$global_env
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
                pkgname <- self$guess_namespace(topic, isf = TRUE)
                if (is.null(pkgname)) {
                    return(NULL)
                }
            }
            ns <- self$get_namespace(pkgname)
            if (!is.null(ns)) {
                ns$get_documentation(topic)
            }
        },

        get_definition = function(symbol, pkgname = NULL, exported_only = TRUE) {
            if (is.null(pkgname)) {
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
            parse_data <- self$get_parse_data(uri)
            if (is.null(parse_data)) {
                return(list())
            }
            parse_data$definition_ranges
        },

        get_definitions_for_query = function(pattern) {
            ranges <- list()
            for (doc in self$documents$values()) {
                parse_data <- doc$parse_data
                if (is.null(parse_data)) next
                doc_ranges <- lapply(
                        parse_data$definition_ranges,
                        function(r) list(
                            uri = doc$uri,
                            range = r
                        )
                    )
                ranges <- append(ranges, doc_ranges[fuzzy_find(names(doc_ranges), pattern)])
            }
            ranges
        },

        get_parse_data = function(uri) {
            self$documents$get(uri, NULL)$parse_data
        },

        update_loaded_packages = function() {
            loaded_packages <- startup_packages
            for (doc in self$documents$values()) {
                loaded_packages <- union(loaded_packages, doc$loaded_packages)
            }
            self$loaded_packages <- loaded_packages
        },

        update_parse_data = function(uri, parse_data) {
            if (!is.null(parse_data$xml_data)) {
                parse_data$xml_doc <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
            }
            self$documents$get(uri)$update_parse_data(parse_data)
        }
    )
)
