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
            self$loaded_packages <- append(self$loaded_packages, pkgname)
            self$namespaces[[pkgname]] <- Namespace$new(pkgname)
        },

        get_namespace = function(nsname) {
            if (nsname %in% names(self$namespaces)) {
                self$namespaces[[nsname]]
            } else {
                self$load_package(nsname)
                self$namespaces[[nsname]]
            }
        },

        get_signature = function(fname, pkgname = NULL) {
            if (is.null(pkgname)){
                for (nsnames in rev(names(self$namespaces))) {
                    ns <- self$get_namespace(nsnames)
                    if (ns$exists(fname)) {
                        return(ns$get_signature(fname))
                    }
                }
            } else {
                ns <- self$get_namespace(pkgname)
                ns$get_signature(fname)
            }
        },

        get_formals = function(fname, pkgname = NULL) {
            if (is.null(pkgname)){
                for (nsnames in rev(names(self$namespaces))) {
                    ns <- self$get_namespace(nsnames)
                    if (ns$exists(fname)) {
                        return(ns$get_formals(fname))
                    }
                }
            } else {
                ns <- self$get_namespace(pkgname)
                ns$get_formals(fname)
            }
        }
    )
)
