#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#' @keywords internal
Workspace <- R6::R6Class("Workspace",
    private = list(
        global_env = list(nonfuncts = character(0),
                          functs = character(0),
                          signatures = list(),
                          formals = list(),
                          lazydata = character()),
        namespaces = list(),
        definitions = NULL,
        xml_docs = list()
    ),
    public = list(
        loaded_packages = c(
            "base", "stats", "methods", "utils", "graphics", "grDevices", "datasets"),

        initialize = function() {
            for (pkgname in self$loaded_packages) {
                private$namespaces[[pkgname]] <- Namespace$new(pkgname)
            }
            private$definitions <- DefinitionCache$new()
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

        guess_package = function(object) {
            logger$info("loaded_packages:", self$loaded_packages)

            for (pkgname in rev(self$loaded_packages)) {
                ns <- self$get_namespace(pkgname)
                if (!is.null(ns) && ns$exists(object)) {
                    return(pkgname)
                }
            }
            NULL
        },

        get_namespace = function(pkgname) {
            if (pkgname == "_workspace_") {
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
                if (funct %in% private$global_env$functs) {
                    return(private$global_env$signatures[[funct]])
                }
                pkgname <- self$guess_package(funct)
            }
            if (is.null(pkgname)) {
                NULL
            } else {
                ns <- self$get_namespace(pkgname)
                if (!is.null(ns)) {
                    ns$get_signature(funct)
                }
            }
        },

        get_formals = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                if (funct %in% private$global_env$functs) {
                    return(private$global_env$formals[[funct]])
                }
                pkgname <- self$guess_package(funct)
            }
            if (is.null(pkgname)) {
                NULL
            } else {
                ns <- self$get_namespace(pkgname)
                if (!is.null(ns)) {
                    ns$get_formals(funct)
                }
            }
        },

        get_help = function(topic, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(topic)
            }
            # note: the parantheses are neccessary
            if (is.null(pkgname)) {
                hfile <- utils::help((topic))
            } else {
                hfile <- utils::help((topic), (pkgname))
            }

            if (length(hfile) > 0) {
                enc2utf8(repr::repr_text(hfile))
            } else {
                NULL
            }
        },

        get_definition = function(token, pkg = NULL) {
            if (is.null(pkg)) {
                # look in file first
                definition <- private$definitions$get(token)
                if (is.null(definition)) {
                    definition <- self$find_definition_in_package(token)
                }
            } else {
                definition <- self$find_definition_in_package(token, pkg)
            }
            definition
        },

        get_definitions_for_uri = function(uri) {
            private$definitions$get_functs_for_uri(uri)
        },

        get_definitions_for_query = function(query) {
            private$definitions$filter(query)
        },

        find_definition_in_package = function(funct, pkg = NULL) {
            code <- self$get_code(funct, pkg)
            if (!is.null(code)) {
                # if the function exists in the workspace, write the code to a file
                tmp <- file.path(tempdir(), paste0(funct, ".R"))
                logger$info("tmp: ", tmp)
                readr::write_lines(code, tmp)
                nlines <- length(readr::read_lines(tmp)) + 1
                list(
                    uri = path_to_uri(tmp),
                    range = range(
                        start = position(line = 0, character = 0),
                        end = position(line = nlines, character = 0)
                    )
                )
            }
        },

        get_code = function(topic, pkg = NULL) {
            if (is.null(pkg) || is.na(pkg)) {
                logger$info("pkg guess !")
                pkg <- self$guess_package(topic)
            }
            logger$info("pkg:", pkg)

            if (!is.null(pkg) && length(find.package(pkg, quiet = TRUE))) {
                code <- utils::getFromNamespace(topic, pkg)
                if (length(code) > 0) {
                    code <- repr::repr_text(code)
                    # reorganize the code
                    code <- stringr::str_split(code, "\n")[[1]]
                    code[1] <- paste(topic, "<-", code[1])
                    enc2utf8(code[!grepl("<bytecode|<environment", code)])
                }
            }
        },

        get_xml_doc = function(uri) {
            private$xml_docs[[uri]]
        },

        update_parse_data = function(uri, parse_data) {
            self$load_packages(parse_data$packages)

            private$global_env$nonfuncts <- unique(
                c(private$global_env$nonfuncts, parse_data$nonfuncts))
            private$global_env$functs <- unique(
                c(private$global_env$functs, parse_data$functs))
            private$global_env$signatures <- merge_list(
                private$global_env$signatures, parse_data$signatures)
            private$global_env$formals <- merge_list(
                private$global_env$formals, parse_data$formals)

            private$definitions$update(uri, parse_data$definition_ranges)

            if (!is.null(parse_data$xml_data)) {
                private$xml_docs[[uri]] <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
            }
        }
    )
)
