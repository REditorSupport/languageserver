startup_packages <- c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#' @keywords internal
Workspace <- R6::R6Class("Workspace",
    public = list(
        root = NULL,
        namespaces = NULL,
        global_env = NULL,
        documents = NULL,

        # from NAMESPACE importFrom()
        imported_objects = NULL,
        # from NAMESPACE import()
        imported_packages = NULL,
        namespace_file_mt = NULL,

        startup_packages = NULL,
        loaded_packages = NULL,

        initialize = function(root) {
            self$root <- root
            self$documents <- collections::dict()
            self$imported_objects <- collections::dict()
            self$imported_packages <- character(0)
            self$global_env <- GlobalEnv$new(self$documents)
            self$namespaces <- collections::dict()
            self$startup_packages <- tryCatch(
                callr::r(resolve_attached_packages,
                    system_profile = TRUE, user_profile = TRUE, timeout = 3),
                error = function(e) {
                    logger$info("workspace initialize error: ", e)
                    startup_packages
                }
            )
            self$loaded_packages <- self$startup_packages
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
            if (!nzchar(object)) {
                return(NULL)
            }

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

            if (self$imported_objects$has(object)) {
                pkgname <- self$imported_objects$get(object)
                logger$info("object from:", pkgname)
                return(pkgname)
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
                pkgname <- self$guess_namespace(topic, isf = isf)
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
                pkgname <- self$guess_namespace(symbol, isf = FALSE)
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
            parse_data$definitions
        },

        get_definitions_for_query = function(pattern) {
            result <- list()
            for (doc in self$documents$values()) {
                parse_data <- doc$parse_data
                if (is.null(parse_data)) next
                symbols <- names(parse_data$definitions)
                matches <- symbols[fuzzy_find(symbols, pattern)]
                result <- c(result, lapply(
                    unname(parse_data$definitions[matches]),
                    function(def) c(
                        uri = doc$uri,
                        def
                    )
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

        update_parse_data = function(uri, parse_data) {
            if (!is.null(parse_data$xml_data)) {
                parse_data$xml_doc <- tryCatch(
                    xml2::read_xml(parse_data$xml_data), error = function(e) NULL)
            }
            self$documents$get(uri)$update_parse_data(parse_data)
        },

        load_all = function(langserver) {
            source_dir <- file.path(self$root, "R")
            files <- list.files(source_dir)
            for (f in files) {
                logger$info("load ", f)
                path <- file.path(source_dir, f)
                uri <- path_to_uri(path)
                doc <- Document$new(uri, NULL, stringi::stri_read_lines(path))
                self$documents$set(uri, doc)
                # TODO: move text_sync to Workspace!?
                langserver$text_sync(uri, document = doc, parse = TRUE)
            }
            self$import_from_namespace_file()
        },

        import_from_namespace_file = function() {
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
