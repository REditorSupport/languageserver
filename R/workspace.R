Namespace <- R6::R6Class("Namespace",
    public = list(
        package_name = NULL,
        exports = NULL,
        unexports = NULL,
        functs = NULL,
        nonfuncts = NULL,

        initialize = function(pkgname) {
            self$package_name <- pkgname
            ns <- asNamespace(pkgname)
            objects <- sanitize_names(objects(ns))
            self$exports <- sanitize_names(getNamespaceExports(ns))
            self$unexports <- setdiff(objects, self$exports)
            isf <- sapply(self$exports, function(x) {
                        is.function(get(x, envir = ns))})
            self$functs <- self$exports[isf]
            self$nonfuncts <- setdiff(self$exports, self$functs)
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
                sig <- sig[1:length(sig) - 1]
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

Workspace <- R6::R6Class("Workspace",
    public = list(
        loaded_packages = c("base", "stats", "methods", "utils", "graphics", "grDevices"),
        namespaces = list(),
        global_env = list(nonfuncts = character(0),
                          functs = character(0),
                          signatures = list(),
                          formals = list()),

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

        guess_package = function(object) {
            logger$info("loaded_packages:", self$loaded_packages)

            for (pkg in rev(self$loaded_packages)) {
                ns <- self$get_namespace(pkg)
                if (ns$exists(object)) {
                    return(pkg)
                }
            }
            NULL
        },

        get_namespace = function(pkg) {
            if (pkg == "_workspace_") {
                self$global_env
            } else if (pkg %in% names(self$namespaces)) {
                self$namespaces[[pkg]]
            } else {
                self$namespaces[[pkg]] <- Namespace$new(pkg)
                self$namespaces[[pkg]]
            }
        },

        get_signature = function(funct, pkg = NULL) {
            if (is.null(pkg)) {
                if (funct %in% names(self$global_env$signatures)) {
                    return(self$global_env$signatures[[funct]])
                }
                pkg <- self$guess_package(funct)
            }
            if (is.null(pkg)) {
                NULL
            } else {
                tryCatch({
                    ns <- self$get_namespace(pkg)
                    ns$get_signature(funct)
                    },
                    error = function(e) NULL
                )
            }

        },

        get_formals = function(funct, pkg = NULL) {
            if (is.null(pkg)) {
                if (funct %in% names(self$global_env$formals)) {
                    return(self$global_env$formals[[funct]])
                }
                pkg <- self$guess_package(funct)
            }
            if (is.null(pkg)) {
                NULL
            } else {
                tryCatch({
                        ns <- self$get_namespace(pkg)
                        ns$get_formals(funct)
                    },
                    error = function(e) list()
                )
            }
        },

        get_help = function(topic, pkg = NULL) {
            if (is.null(pkg) || is.na(pkg)) {
                pkg <- self$guess_package(topic)
            }
            if (is.null(pkg)) {
                hfile <- utils::help((topic))
            } else {
                hfile <- utils::help((topic), (pkg))
            }
            if (length(hfile) > 0) {
                enc2utf8(repr::repr_text(hfile))
            } else {
                NULL
            }
        },

        load_to_global = function(nonfuncts, functs, signatures, formals) {
            self$global_env$nonfuncts <- unique(c(self$global_env$nonfuncts, nonfuncts))
            self$global_env$functs <- unique(c(self$global_env$functs, functs))
            self$global_env$signatures <- merge_list(self$global_env$signatures, signatures)
            self$global_env$formals <- merge_list(self$global_env$formals, formals)
        }
    )
)

#' Determine workspace information for a given file
#'
#' internal use only
#' @param uri the file uri
#' @param temp_file the file to lint, determine from \code{uri} if \code{NULL}
#' @param run_lintr set \code{FALSE} to disable lintr diagnostics
#' @param parse set \code{FALSE} to disable parsing file
#' @export
workspace_sync <- function(uri, temp_file = NULL, run_lintr = TRUE, parse = FALSE) {
    packages <- character(0)
    functs <- character()
    nonfuncts <- character()
    formals <- list()
    signatures <- list()

    if (is.null(temp_file)) {
        path <- path_from_uri(uri)
    } else {
        path <- temp_file
    }

    if (parse) {
        expr <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
        if (length(expr)) {
            for (e in expr) {
                if (length(e) == 3L &&
                        is.symbol(e[[1L]]) &&
                        (e[[1L]] == "<-" || e[[1L]] == "=") &&
                        is.symbol(e[[2L]])) {
                    symbol <- as.character(e[[2L]])
                    objects <- c(objects, symbol)
                    if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                        functs <- c(functs, symbol)
                        func <- e[[3L]]
                        formals[[symbol]] <- func[[2L]]
                        signature <- func
                        signature[[3L]] <- quote(expr =)
                        signature <- utils::capture.output(print(signature))
                        signature <- paste0(trimws(signature, which = "left"), collapse = "\n")
                        signatures[[symbol]] <- signature
                    } else {
                        nonfuncts <- c(nonfuncts, symbol)
                    }
                } else if (length(e) == 2L &&
                            is.symbol(e[[1L]]) &&
                            (e[[1L]] == "library" || e[[1L]] == "require")) {

                    packages <- c(packages, as.character(e[[2L]]))
                }
            }
        }
    }

    if (run_lintr) {
        diagnostics <- tryCatch(diagnose_file(path), error = function(e) NULL)
    } else {
        diagnostics <- NULL
    }

    list(packages = packages,
         nonfuncts = nonfuncts, functs = functs, signatures = signatures, formals = formals,
         diagnostics = diagnostics)
}

process_sync_in <- function(self) {
    sync_in <- self$sync_in
    sync_out <- self$sync_out

    uris <- sync_in$keys()
    # avoid heavy cpu usage
    if (length(uris) > 8) {
        uris <- uris[1:8]
    }
    for (uri in uris) {
        parse <- FALSE
        if (sync_out$has(uri)) {
            item <- sync_out$pop(uri)
            process <- item$process
            parse <- item$parse
            if (process$is_alive()) try(process$kill())
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }

        item <- sync_in$pop(uri)
        run_lintr <- item$run_lintr && self$run_lintr
        parse <- parse || item$parse
        doc <- item$document
        if (is.null(doc)) {
            temp_file <- NULL
        } else {
            temp_file <- tempfile(fileext = ".R")
            write(item$document, file = temp_file)
        }

        sync_out$set(
            uri,
            list(
                process = callr::r_bg(
                    function(...) languageserver::workspace_sync(...),
                    list(
                        uri = uri,
                        temp_file = temp_file,
                        run_lintr = run_lintr,
                        parse = parse
                    ),
                    system_profile = TRUE, user_profile = TRUE
                ),
                parse = parse,
                temp_file = temp_file
            )
        )
    }
}

process_sync_out <- function(self) {
    for (uri in self$sync_out$keys()) {
        item <- self$sync_out$get(uri)
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
                logger$info("load package:", package)
                self$workspace$load_package(package)
            }

            self$workspace$load_to_global(
                result$nonfunct, result$funct, result$signatures, result$formals)

            # cleanup
            self$sync_out$remove(uri)
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }
    }
}
