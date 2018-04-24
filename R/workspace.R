Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        objects = NULL,
        exports = NULL,
        unexports = NULL,
        functs = NULL,
        nonfuncts = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            self$objects <- sanitize_names(objects(ns))
            self$exports <- sanitize_names(getNamespaceExports(ns))
            self$unexports <- setdiff(self$objects, self$exports)
            isf <- sapply(self$exports, function(x) {
                        is.function(get(x, envir = ns))})
            self$functs <- self$exports[isf]
            self$nonfuncts <- setdiff(self$exports, self$functs)
        },

        exists = function(objname) {
            objname %in% self$objects
        },

        get_signature = function(fname) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(fname, envir = ns)
            if (is.primitive(fn)) {
                NULL
            } else {
                sig <- capture.output(str(fn))
                paste(trimws(sig, which = "left"), collapse = "")
            }
        },

        get_formals = function(fname) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(fname, envir = ns)
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
                    error = function(e) list())
            }
        }
    )
)

#' Determine workspace information for a given file
#'
#' internal use only
#' @param uri the file path
#' @param lintfile the actual file to lint
#' @param run_lintr set \code{FALSE} to disable lintr diagnostics
#' @export
workspace_sync <- function(uri, lintfile = NULL, run_lintr = TRUE) {
    packages <- character(0)

    if (is.null(lintfile)) {
        lintfile <- path_from_uri(uri)
        document <- readLines(lintfile, warn = FALSE)

        # only check for packages when opening and saving files, i.e., when lintfile is NULL
        # TODO: check DESCRIPTION of an R Package

        result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
        for (j in seq_along(result)) {
            if (nrow(result[[j]]) >= 1) {
                packages <- append(packages, result[[j]][1, 2])
            }
        }

    }

    if (run_lintr) {
        diagnostics <- tryCatch({
            diagnose_file(lintfile)
        }, error = function(e) NULL)
    } else {
        diagnostics <- NULL
    }

    list(packages = packages, diagnostics = diagnostics)
}

process_sync_input_dict <- function(self) {
    sync_input_dict <- self$sync_input_dict
    sync_output_dict <- self$sync_output_dict

    for (uri in sync_input_dict$keys()) {
        if (sync_output_dict$has(uri)) {
            item <- sync_output_dict$pop(uri)
            process <- item$process
            if (process$is_alive()) try(process$kill())
            lintfile <- item$lintfile
            if (!is.null(lintfile) && file.exists(lintfile)) {
                file.remove(lintfile)
            }
        }

        document <- sync_input_dict$pop(uri)
        if (is.null(document)) {
            lintfile <- NULL
        } else {
            lintfile <- tempfile(fileext = ".R")
            write(document, file = lintfile)
        }

        sync_output_dict$set(
            uri,
            list(
                process = callr::r_bg(
                    function(uri, lintfile, run_lintr) {
                        languageserver::workspace_sync(uri, lintfile, run_lintr)
                    },
                    list(uri = uri, lintfile = lintfile, run_lintr = self$run_lintr)
                ),
                lintfile = lintfile
            )
        )
    }
}

process_sync_output_dict <- function(self) {
    for (uri in self$sync_output_dict$keys()) {
        item <- self$sync_output_dict$get(uri)
        process <- item$process

        if (!is.null(process) && !process$is_alive()) {
            result <- process$get_result()
            diagnostics <- result$diagnostics
            if (!is.null(diagnostics)) {
                self$deliver(
                    Notification$new(
                        method = "textDocument/publishDiagnostics",
                        params = list(
                            uri = uri,
                            diagnostics = diagnostics
                        )
                    )
                )
            }
            for (package in result$packages) {
                self$workspace$load_package(package)
            }

            # cleanup
            self$sync_output_dict$remove(uri)
            lintfile <- item$lintfile
            if (!is.null(lintfile) && file.exists(lintfile)) {
                file.remove(lintfile)
            }
        }
    }
}
