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
            objname %in% self$exports
        },

        get_signature = function(funct) {
            pkgname <- self$package_name
            ns <- asNamespace(pkgname)
            fn <- get(funct, envir = ns)
            if (is.primitive(fn)) {
                NULL
            } else {
                sig <- capture.output(print(args(fn)))
                sig <- sig[1:length(sig) - 1]
                paste0(trimws(sig, which = "left"), collapse = "\n\t")
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
            if (pkg %in% names(self$namespaces)) {
                self$namespaces[[pkg]]
            } else {
                self$namespaces[[pkg]] <- Namespace$new(pkg)
                self$namespaces[[pkg]]
            }
        },

        get_signature = function(funct, pkg = NULL) {
            if (is.null(pkg)) {
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
        }
    )
)

#' Determine workspace information for a given file
#'
#' internal use only
#' @param uri the file path
#' @param run_lintr set \code{FALSE} to disable lintr diagnostics
#' @param parse_file set \code{FALSE} to disable parsing file
#' @param temp_file the actual file to lint
#' @export
workspace_sync <- function(uri, run_lintr = TRUE, parse_file = FALSE, temp_file = NULL) {
    packages <- character(0)

    if (parse_file) {
        if (is.null(temp_file)) {
            temp_file <- path_from_uri(uri)
        }
        document <- readLines(temp_file, warn = FALSE)

        # only check for packages when saving files, i.e., when temp_file is NULL
        # TODO: check DESCRIPTION of an R Package

        result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
        for (j in seq_along(result)) {
            if (nrow(result[[j]]) >= 1) {
                packages <- append(packages, result[[j]][1, 2])
            }
        }

    }

    if (run_lintr) {
        diagnostics <- tryCatch(diagnose_file(temp_file), error = function(e) NULL)
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
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }

        document <- sync_input_dict$pop(uri)
        if (isTRUE(document)) {
            parse_file <- TRUE
            temp_file <- NULL
        } else {
            parse_file <- FALSE
            temp_file <- tempfile(fileext = ".R")
            write(document, file = temp_file)
        }

        sync_output_dict$set(
            uri,
            list(
                process = callr::r_bg(
                    function(...) languageserver::workspace_sync(...),
                    list(
                        uri = uri,
                        run_lintr = self$run_lintr,
                        parse_file = parse_file,
                        temp_file = temp_file),
                    system_profile = TRUE, user_profile = TRUE
                ),
                temp_file = temp_file
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
                logger$info("load package:", package)
                self$workspace$load_package(package)
            }

            # cleanup
            self$sync_output_dict$remove(uri)
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }
    }
}
