#' A class for storing package information
#' @keywords internal
Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        exports = character(0),
        unexports = character(0),
        functs = character(0),
        nonfuncts = character(0),
        lazydata = character(0),

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

        exists_funct = function(funct) {
            funct %in% self$functs
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

WORKSPACE <- "_workspace_"

#' A class for storing global environment information
GlobalNameSpace <- R6::R6Class("GlobalNameSpace",
    inherit = Namespace,
    public = list(
        signatures = list(),
        formals = list(),
        definitions = list(),

        initialize = function() {
            self$package_name <- WORKSPACE
        },

        get_signature = function(funct) {
            self$signatures[[funct]]
        },

        get_formals = function(funct) {
            self$formals[[funct]]
        },

        update = function(nonfuncts, functs, signatures, formals) {
            self$nonfuncts <- unique(c(self$nonfuncts, nonfuncts))
            self$functs <- unique(c(self$functs, functs))
            self$exports <- unique(c(self$nonfuncts, self$functs))
            self$signatures <- merge_list(self$signatures, signatures)
            self$formals <- merge_list(self$formals, formals)
        }
    )
)


resolve_attached_packages <- function(pkgs) {
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
