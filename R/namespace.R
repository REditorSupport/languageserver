#' A class for storing package information
#' @keywords internal
Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        objects = character(0),
        functs = character(0),
        nonfuncts = character(0),
        exports = character(0),
        exported_functs = character(0),
        exported_nonfuncts = character(0),
        lazydata = character(0),

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            self$objects <- sanitize_names(objects(ns, all.names = TRUE))
            is_function <- vapply(self$objects, function(x) {
                        is.function(get(x, envir = ns))}, logical(1L), USE.NAMES = FALSE)
            is_exported <- self$objects %in% sanitize_names(getNamespaceExports(ns))
            self$functs <- self$objects[is_function]
            self$nonfuncts <- self$objects[!is_function]
            self$exports <- self$objects[is_exported]
            self$exported_functs <- self$objects[is_exported & is_function]
            self$exported_nonfuncts <- self$objects[is_exported & !is_function]
            self$lazydata <- if (length(ns$.__NAMESPACE__.$lazydata))
                objects(ns$.__NAMESPACE__.$lazydata) else character()
        },

        exists = function(objname, exported_only = TRUE) {
            if (exported_only) {
                objname %in% self$exports
            } else {
                objname %in% self$objects
            }
        },

        exists_funct = function(funct, exported_only = TRUE) {
            if (exported_only) {
                funct %in% self$exported_functs
            } else {
                funct %in% self$functs
            }
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
                paste0(trimws(sig, which = "left"), collapse = "")
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

        get_definition = function(symbol, exported_only = TRUE) {
            code <- self$get_body(symbol, exported_only = exported_only)
            if (is.null(code)) {
                return(NULL)
            }

            # if the function exists in the workspace, write the code to a file
            temp_file <- file.path(tempdir(), paste0(symbol, ".R"))
            readr::write_lines(code, temp_file)
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
GlobalNameSpace <- R6::R6Class("GlobalNameSpace",
    inherit = Namespace,
    public = list(
        signatures = list(),
        formals = list(),
        definitions = list(),
        uris = list(),

        initialize = function() {
            self$package_name <- WORKSPACE
        },

        get_signature = function(funct, exported_only = TRUE) {
            self$signatures[[funct]]
        },

        get_formals = function(funct, exported_only = TRUE) {
            self$formals[[funct]]
        },

        get_definition = function(symbol, exported_only = TRUE) {
            NULL
        },

        get_body = function(funct, exported_only = TRUE) {
            NULL
        },

        update = function(parse_data) {
            self$nonfuncts <- unique(unlist(lapply(parse_data, "[[", "nonfuncts"), use.names = FALSE))
            self$exported_nonfuncts <- self$nonfuncts
            self$functs <- unique(unlist(lapply(parse_data, "[[", "functs"), use.names = FALSE))
            self$exported_functs <- self$functs
            self$objects <- unique(c(self$nonfuncts, self$functs))
            self$exports <- self$objects
            self$signatures <- list()
            self$formals <- list()
            for (item in parse_data) {
                self$signatures <- merge_list(self$signatures, item$signatures)
                self$formals <- merge_list(self$formals, item$formals)
            }
        }
    )
)

#' A data structure to hold function definition locations
#'
#' The key reason for using this rather than a `list` is that this also cleans up
#' when functions are removed from files.
#' @keywords internal
DefinitionCache <- R6::R6Class("DefinitionCache",
    private = list(
        locations = list(),
        uris = list()
    ),
    public = list(
        get = function(funct) {
            private$locations[[funct]]
        },
        get_functs_for_uri = function(uri) {
            private$locations[private$uris[[uri]]]
        },
        filter = function(pattern) {
            private$locations[fuzzy_find(names(private$locations), pattern)]
        },
        update = function(uri, ranges) {
            functs <- names(ranges)
            removed_functs <- setdiff(private$uris[[uri]], functs)
            if (!is.null(removed_functs) && length(removed_functs) > 0) {
                private$locations[removed_functs] <- NULL
            }
            for (funct in functs) {
                private$locations[[funct]] <- location(
                    uri = uri,
                    range = ranges[[funct]]
                )
            }
            private$uris[[uri]] <- functs
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
