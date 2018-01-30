Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        objects = NULL,
        exports = NULL,
        unexports = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            self$objects <- objects(ns)
            self$exports <- getNamespaceExports(ns)
            self$unexports <- setdiff(self$objects, self$exports)
        },

        exists = function(objname) {
            objname %in% self$objects
        },

        get_signature = function(fname) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(fname, envir = ns, inherits = FALSE)
            sig <- capture.output(str(fn))
            paste(trimws(sig, which = "left"), collapse = "")
        },

        get_formals = function(fname) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(fname, envir = ns, inherits = FALSE)
            formals(fn)
        },

        print = function() {
            cat(paste0("Namespace: ", self$package_name))
        }
    )
)

Workspace <- R6::R6Class("Workspace",
    public = list(
        loaded_packages = c("base", "stats", "methods", "utils", "graphics", "grDevices"),
        functs = list(),
        namespaces = list(),

        initialize = function() {
            for (pkgname in self$loaded_packages) {
                self$namespaces[[pkgname]] <- Namespace$new(pkgname)
            }
        },

        load_package = function(pkgname) {
            if (!(pkgname %in% self$loaded_packages)) {
                ns <- try(self$get_namespace(pkgname), silent = TRUE)
                logger$info("ns: ", ns)
                if (!inherits(ns, "try-error")) {
                    self$loaded_packages <- append(self$loaded_packages, pkgname)
                    logger$info("loaded_packages: ", self$loaded_packages)

                }
            }
        },

        get_namespace = function(pkgname) {
            if (pkgname %in% names(self$namespaces)) {
                self$namespaces[[pkgname]]
            } else {
                self$namespaces[[pkgname]] <- Namespace$new(pkgname)
                self$namespaces[[pkgname]]
            }
        },

        get_signature = function(fname, pkgname = NULL) {
            if (is.null(pkgname)){
                for (ns in rev(self$loaded_packages)) {
                    ns <- self$get_namespace(ns)
                    if (ns$exists(fname)) {
                        return(ns$get_signature(fname))
                    }
                }
            } else {
                tryCatch({
                        ns <- self$get_namespace(pkgname)
                        ns$get_signature(fname)
                    },
                    error = function(e) NULL)
            }
        },

        get_formals = function(fname, pkgname = NULL) {
            if (is.null(pkgname)){
                for (ns in rev(self$loaded_packages)) {
                    ns <- self$get_namespace(ns)
                    if (ns$exists(fname)) {
                        return(ns$get_formals(fname))
                    }
                }
            } else {
                tryCatch({
                        ns <- self$get_namespace(pkgname)
                        ns$get_formals(fname)
                    },
                    error = function(e) NULL)
            }
        }
    )
)

workspace_sync <- function(workspace, document) {
    result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
    for (j in seq_along(result)) {
        if (nrow(result[[j]]) >= 1) {
            logger$info("load package: ", result[[j]][1, 2])
            workspace$load_package(result[[j]][1, 2])
        }
    }
}
