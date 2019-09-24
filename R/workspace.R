#' A data structure for a session workspace
#'
#' A `Workspace` is initialized at the start of a session, when the language
#' server is started. Its goal is to contain the `Namespace`s of the packages
#' that are loaded during the session for quick reference.
#'
#' @section Methods:
#' + `load_package(pkgname)`: add a new `Namespace` for `pkgname` if necessary
#' and if possible
#' + `guess_package(object)`: returns the most recently loaded package in which
#' `object` can be found
#' + `get_namespace(pkgname)`:  return the [Namespace] of `pkgname`
#' + `get_signature(funct, pkgname = NULL)`: return the signature of `funct`
#' + `get_formals(funct, pkgname = NULL)`: return the [base::formals()] of `funct`
#' + `get_help(topic, pkgname = NULL)`: return the help text of `topic`
#' + `load_to_global(parse_result)`:
#'
#' @field pkgname a character, a package name
#' @field object a character, an object name
#' @field funct a character, a function name
#' @field topic a character, a help topic name
#' @field parse_result ?
Workspace <- R6::R6Class("Workspace",
    public = list(
        loaded_packages = c("base", "stats", "methods", "utils", "graphics", "grDevices"),
        namespaces = list(),
        global_env = list(nonfuncts = character(0),
                          functs = character(0),
                          signatures = list(),
                          formals = list(),
                          definitions = DefinitionCache$new()),

        initialize = function() {
            for (pkgname in self$loaded_packages) {
                self$namespaces[[pkgname]] <- Namespace$new(pkgname)
            }
        },

        load_package = function(pkgname) {
            if (!(pkgname %in% self$loaded_packages)) {
                ns <- tryCatch(self$get_namespace(pkgname), error = function(e) NULL)
                logger$info("ns: ", ns)
                if (!is.null(ns)) {
                    self$loaded_packages <- append(self$loaded_packages, pkgname)
                    logger$info("loaded_packages: ", self$loaded_packages)
                }
            }
        },

        guess_package = function(object) {
            logger$info("loaded_packages:", self$loaded_packages)

            for (pkgname in rev(self$loaded_packages)) {
                ns <- self$get_namespace(pkgname)
                if (ns$exists(object)) {
                    return(pkgname)
                }
            }
            NULL
        },

        get_namespace = function(pkgname) {
            if (pkgname == "_workspace_") {
                self$global_env
            } else if (pkgname %in% names(self$namespaces)) {
                self$namespaces[[pkgname]]
            } else {
                self$namespaces[[pkgname]] <- Namespace$new(pkgname)
                self$namespaces[[pkgname]]
            }
        },

        get_signature = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                if (funct %in% self$global_env$functs) {
                    return(self$global_env$signatures[[funct]])
                }
                pkgname <- self$guess_package(funct)
            }
            if (is.null(pkgname)) {
                NULL
            } else {
                tryCatch({
                    ns <- self$get_namespace(pkgname)
                    ns$get_signature(funct)
                    },
                    error = function(e) NULL
                )
            }

        },

        get_formals = function(funct, pkgname = NULL) {
            if (is.null(pkgname)) {
                if (funct %in% self$global_env$functs) {
                    return(self$global_env$formals[[funct]])
                }
                pkgname <- self$guess_package(funct)
            }
            if (is.null(pkgname)) {
                NULL
            } else {
                tryCatch({
                        ns <- self$get_namespace(pkgname)
                        ns$get_formals(funct)
                    },
                    error = function(e) list()
                )
            }
        },

        get_help = function(topic, pkgname = NULL) {
            if (is.null(pkgname)) {
                pkgname <- self$guess_package(topic)
            }
            # note: the parantheses are neccessary
            if (is.null(pkgname)) {
                hfile <- utils::help((topic))
            } else {
                hfile <- utils::help((topic), (pkgname))
            }

            if (length(hfile) > 0) {
                enc2utf8(repr::repr_text(hfile))
            } else {
                NULL
            }
        },

        get_definition = function(topic) {
            self$global_env$definitions$get(topic)
        },

        get_definitions_for_uri = function(uri) {
            self$global_env$definitions$get_functs_for_uri(uri)
        },

        get_definitions_for_query = function(query) {
            self$global_env$definitions$filter(query)
        },

        get_code = function(topic, pkg = NULL) {
            if (is.null(pkg) || is.na(pkg)) {
                logger$info("pkg guess !")
                pkg <- self$guess_package(topic)
            }
            logger$info("pkg:", pkg)

            if (is.null(pkg)) {
                return(NULL)
            } else {
                code <- utils::getFromNamespace(topic, pkg)
            }
            if (length(code) > 0) {
                code <- repr::repr_text(code)
                # reorganize the code
                code <- stringr::str_split(code, "\n")[[1]]
                code[1] <- paste(topic, "<-", code[1])
                enc2utf8(code[!grepl("<bytecode|<environment", code)])
            } else {
                NULL
            }
        },

        load_to_global = function(parse_result, uri) {
            self$global_env$nonfuncts <- unique(
                c(self$global_env$nonfuncts, parse_result$nonfuncts))
            self$global_env$functs <- unique(
                c(self$global_env$functs, parse_result$functs))
            self$global_env$signatures <- merge_list(
                self$global_env$signatures, parse_result$signatures)
            self$global_env$formals <- merge_list(
                self$global_env$formals, parse_result$formals)
            self$global_env$definitions$update(uri, parse_result$definition_ranges)
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
    if (is.null(temp_file)) {
        path <- path_from_uri(uri)
    } else {
        path <- temp_file
    }

    if (parse) {
        parse_result <- tryCatch(parse_document(path), error = function(e) NULL)
        # parse_result <- parse_document(path)
    } else {
        parse_result <- NULL
    }

    if (run_lintr) {
        diagnostics <- tryCatch(diagnose_file(path), error = function(e) NULL)
        # diagnostics <- diagnose_file(path)
    } else {
        diagnostics <- NULL
    }

    list(parse_result = parse_result, diagnostics = diagnostics)
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
            if (process$is_alive()) try(process$kill(), silent = TRUE)
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }

        item <- sync_in$pop(uri)
        run_lintr <- item$run_lintr && self$run_lintr
        parse <- parse || item$parse
        doc <- item$document
        path <- path_from_uri(uri)
        if (is.null(doc)) {
            temp_file <- NULL
        } else {
            temp_file <- tempfile(fileext = if (is_rmarkdown(path)) ".Rmd" else ".R")
            write(doc$content, file = temp_file)
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
            process_result <- process$get_result()
            diagnostics <- process_result$diagnostics
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
            parse_result <- process_result$parse_result
            if (!is.null(parse_result)) {
                for (package in parse_result$packages) {
                    logger$info("load package:", package)
                    self$workspace$load_package(package)
                }

                self$workspace$load_to_global(parse_result, uri)
            }

            # cleanup
            self$sync_out$remove(uri)
            temp_file <- item$temp_file
            if (!is.null(temp_file) && file.exists(temp_file)) {
                file.remove(temp_file)
            }
        }
    }
}
