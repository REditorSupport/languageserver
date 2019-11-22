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
            if (!self$exists_funct(funct)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            args <- args(fn)
            if (!is.null(args)) {
                sig <- utils::capture.output(print(args))
                sig <- sig[-length(sig)]
                paste0(trimws(sig, which = "left"), collapse = "\n")
            }
        },

        get_formals = function(funct) {
            if (!self$exists_funct(funct)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            formals(fn)
        },

        get_definition = function(symbol) {
            code <- self$get_body(symbol)
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
                    end = position(line = stringr::str_count(code, "\n") + 1, character = 0)
                )
            )
        },

        get_body = function(funct) {
            if (!self$exists_funct(funct)) {
                return(NULL)
            }
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            code <- repr::repr_text(fn)
            # reorganize the code
            code <- stringr::str_split(code, "\n")[[1]]
            # we don't add  `<-` to avoid the parser pasrsing the file
            # code[1] <- paste(funct, "<-", code[1])
            paste0(code[!grepl("<bytecode|<environment", code)], collapse = "\n")
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

        get_signature = function(funct) {
            self$signatures[[funct]]
        },

        get_formals = function(funct) {
            self$formals[[funct]]
        },

        get_definition = function(symbol) {
            NULL
        },

        get_body = function(funct) {
            NULL
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
