#' A data structure for package namespaces
#'
#' A `Namespace` is initialized with a package name and builds the list of
#' objects defined in the package namespace.
#' @keywords internal
Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        exports = NULL,
        unexports = NULL,
        functs = NULL,
        nonfuncts = NULL,
        lazydata = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            objects <- sanitize_names(objects(ns))
            self$exports <- sanitize_names(getNamespaceExports(ns))
            self$unexports <- setdiff(objects, self$exports)
            isf <- vapply(self$exports, function(x) {
                        is.function(get(x, envir = ns))}, logical(1L), USE.NAMES = FALSE)
            self$functs <- self$exports[isf]
            self$nonfuncts <- setdiff(self$exports, self$functs)
            self$lazydata <- if (length(ns$.__NAMESPACE__.$lazydata))
                objects(ns$.__NAMESPACE__.$lazydata) else character()
        },

        exists = function(objname) {
            objname %in% self$exports
        },

        get_signature = function(funct) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            if (is.primitive(fn)) {
                NULL
            } else {
                sig <- utils::capture.output(print(args(fn)))
                sig <- sig[-length(sig)]
                paste0(trimws(sig, which = "left"), collapse = "\n")
            }
        },

        get_formals = function(funct) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            formals(fn)
        },

        print = function() {
            cat(paste0("Namespace: ", self$package_name))
        }
    )
)


resolve_package_dependencies <- function(pkgs) {
    if (length(pkgs)) {
        deps <- tryCatch(
            callr::r(
                function(pkgs) {
                    for (pkg in pkgs) {
                        if (paste0("package:", pkg) %in% search()) {
                            next
                        }
                        tryCatch(library(pkg, character.only = TRUE),
                            error = function(e) NULL
                        )
                    }
                    search()
                },
                list(pkgs = pkgs)
            ),
            error = function(e) NULL
        )
        if (!is.null(deps)) {
            deps <- deps[startsWith(deps, "package:")]
            deps <- gsub("package:", "", deps)
            deps <- deps[!deps %in% pkgs]
            pkgs <- c(pkgs, deps)
        }
    }
    pkgs
}
