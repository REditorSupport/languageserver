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
            sig <- capture.output(str(fn))
            paste(trimws(sig, which = "left"), collapse = "")
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
#' @export
workspace_sync <- function(uri, lintfile) {
    packages <- character(0)

    if (is.null(lintfile)) {
        lintfile <- path_from_uri(uri)
        document <- readLines(lintfile)

        # only check for packages when opening and saving files, i.e., when lintfile is NULL
        # TODO: check DESCRIPTION of an R Package

        result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
        for (j in seq_along(result)) {
            if (nrow(result[[j]]) >= 1) {
                packages <- append(packages, result[[j]][1, 2])
            }
        }

    }

    diagnostics <- tryCatch({
        diagnose_file(lintfile)
    }, error = function(e) NULL)

    list(packages = packages, diagnostics = diagnostics)
}

process_sync_input_queue <- function(self) {
    sync_input_queue <- self$sync_input_queue
    sync_output_queue <- self$sync_output_queue

    for (i in seq_len(sync_input_queue$size())) {
        qinput <- sync_input_queue$get()
        uri <- qinput$id
        document <- qinput$item

        if (sync_output_queue$has(uri)) {
            qoutput <- sync_output_queue$get(uri)
            process <- qoutput$item$process
            lintfile <- qoutput$item$lintfile
            try({
                if (process$is_alive()) {
                    process$kill()
                }
            })
            if (!is.null(lintfile) && file.exists(lintfile)) {
                file.remove(lintfile)
            }
        }

        if (is.null(document)) {
            lintfile <- NULL
        } else {
            lintfile <- tempfile(fileext = ".R")
            write(document, file = lintfile)
        }

        sync_output_queue$put(
            uri,
            list(
                process = callr::r_bg(
                    function(uri, lintfile) {
                        languageserver::workspace_sync(uri, lintfile)
                    },
                    list(uri = uri, lintfile = lintfile)
                ),
                lintfile = lintfile
            )
        )
    }
}

process_sync_output_queue <- function(self) {
    for (i in seq_len(self$sync_output_queue$size())) {
        q <- self$sync_output_queue$get()
        uri <- q$id
        p <- q$item$process
        lintfile <- q$item$lintfile

        if (!is.null(p)) {
            if (p$is_alive()) {
                self$sync_output_queue$put(uri, q$item)
            } else {
                result <- p$get_result()
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
                if (!is.null(lintfile) && file.exists(lintfile)) {
                    file.remove(lintfile)
                }
            }
        }
    }
}
