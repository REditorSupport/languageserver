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

workspace_sync <- function(uri, document) {
    use_temp_file <- !is.null(document)
    packages <- character(0)

    if (use_temp_file) {
        lintfile <- tempfile(fileext = ".R")
        write(document, file = lintfile)
    } else {
        lintfile <- languageserver:::path_from_uri(uri)
        document <- readLines(lintfile)

        # only check for packages when opening and saving files, i.e., when document is NULL
        # TODO: check DESCRIPTION of an R Package

        result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
        for (j in seq_along(result)) {
            if (nrow(result[[j]]) >= 1) {
                packages <- append(packages, result[[j]][1, 2])
            }
        }

    }

    diagnostics <- tryCatch({
        languageserver:::diagnose_file(lintfile)
    }, error = function(e) NULL)

    if (use_temp_file) file.remove(lintfile)

    list(packages = packages, diagnostics = diagnostics)
}

process_sync_queue <- function(self){
    sync_queue <- self$sync_queue
    for (i in seq_len(sync_queue$size())) {
        dq <- sync_queue$get()
        uri <- dq$id
        document <- dq$x

        self$coroutine_queue$put(
            list(
                process = callr::r_bg(
                    workspace_sync,
                    list(uri = uri, document = document)
                ),
                callback = function(result) {
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
                }
            )
        )
    }
}
