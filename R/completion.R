# TODO: group the completions into different catagories according to
# https://github.com/wch/r-source/blob/trunk/src/library/utils/R/completion.R

CompletionItemKind <- list(
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25
)

constants <- c("TRUE", "FALSE", "NULL",
    "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
    "Inf", "NaN")

#' Complete language constants
#' @keywords internal
constant_completion <- function(token) {
    consts <- constants[startsWith(constants, token)]
    completions <- lapply(consts, function(const) {
        list(label = const, kind = CompletionItemKind$Constant,
            data = list(type = "constant"))
    })
}

#' Complete a package name
#' @keywords internal
package_completion <- function(token) {
    installed_packages <- rownames(utils::installed.packages())
    token_packages <- installed_packages[startsWith(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package, kind = CompletionItemKind$Module,
            data = list(type = "package"))
    })
    completions
}

#' Complete a function argument
#' @keywords internal
arg_completion <- function(workspace, token, funct, package = NULL, exported_only = TRUE) {
    if (is.null(package)) {
        package <- workspace$guess_namespace(funct, isf = TRUE)
    }
    if (!is.null(package)) {
        args <- names(workspace$get_formals(funct, package, exported_only = exported_only))
        if (is.character(args)) {
            token_args <- args[startsWith(args, token)]
            completions <- lapply(token_args, function(arg) {
                list(label = arg, kind = CompletionItemKind$Variable,
                    detail = "parameter",
                    preselect = TRUE,
                    data = list(
                        type = "parameter",
                        funct = funct,
                        package = package
                ))
            })
            completions
        }
    }
}

#' Complete any object in the workspace
#' @keywords internal
workspace_completion <- function(workspace, token, package = NULL, exported_only = TRUE) {
    completions <- list()

    if (is.null(package)) {
        packages <- c(WORKSPACE, workspace$loaded_packages)
    } else {
        packages <- c(package)
    }

    if (is.null(package) || exported_only) {
        for (nsname in packages) {
            ns <- workspace$get_namespace(nsname)
            if (is.null(ns)) {
                next
            }
            if (nsname == WORKSPACE) {
                tag <- "[workspace]"
            } else {
                tag <- paste0("{", nsname, "}")
            }
            functs <- ns$exported_functs[startsWith(ns$exported_functs, token)]
            functs_completions <- lapply(functs, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Function,
                     detail = tag,
                     data = list(
                         type = "function",
                         package = nsname
                     ))
            })
            nonfuncts <- ns$exported_nonfuncts[startsWith(ns$exported_nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     data = list(
                         type = "nonfunction",
                         package = nsname
                     ))
            })
            lazydata <- ns$lazydata[startsWith(ns$lazydata, token)]
            lazydata_completions <- lapply(lazydata, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     data = list(
                         type = "lazydata",
                         package = nsname
                     ))
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions,
                lazydata_completions)
        }
    } else {
        ns <- workspace$get_namespace(package)
        if (!is.null(ns)) {
            tag <- paste0("{", package, "}")
            functs <- ns$functs[startsWith(ns$functs, token)]
            functs_completions <- lapply(functs, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Function,
                     detail = tag,
                     data = list(
                         type = "function",
                         package = package
                     ))
            })
            nonfuncts <- ns$nonfuncts[startsWith(ns$nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     data = list(
                         type = "nonfunction",
                         package = package
                     ))
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions)
        }
    }

    completions
}

scope_completion_symbols_xpath <- paste(
    "FUNCTION/following-sibling::SYMBOL_FORMALS",
    "forcond/SYMBOL",
    "expr/LEFT_ASSIGN[not(following-sibling::expr/FUNCTION)]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "expr/RIGHT_ASSIGN[not(preceding-sibling::expr/FUNCTION)]/following-sibling::expr[count(*)=1]/SYMBOL",
    "equal_assign/EQ_ASSIGN[not(following-sibling::expr/FUNCTION)]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    sep = "|")

scope_completion_functs_xpath <- paste(
    "expr/LEFT_ASSIGN[following-sibling::expr/FUNCTION]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "expr/RIGHT_ASSIGN[preceding-sibling::expr/FUNCTION]/following-sibling::expr[count(*)=1]/SYMBOL",
    "equal_assign/EQ_ASSIGN[following-sibling::expr/FUNCTION]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    sep = "|")

scope_completion <- function(uri, workspace, token, point) {
    xdoc <- workspace$get_xml_doc(uri)
    if (is.null(xdoc)) {
        return(list())
    }

    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
        point$row + 1, point$col + 1)

    completions <- list()
    scope_symbols <- unique(xml_text(xml_find_all(enclosing_scopes, scope_completion_symbols_xpath)))
    scope_symbols <- scope_symbols[startsWith(scope_symbols, token)]
    completions <- c(completions, lapply(scope_symbols, function(symbol) {
        list(
            label = symbol,
            kind = CompletionItemKind$Field,
            detail = "[scope]"
        )
    }))

    scope_functs <- unique(xml_text(xml_find_all(enclosing_scopes, scope_completion_functs_xpath)))
    scope_functs <- scope_functs[startsWith(scope_functs, token)]
    completions <- c(completions, lapply(scope_functs, function(symbol) {
        list(
            label = symbol,
            kind = CompletionItemKind$Function,
            detail = "[scope]"
        )
    }))

    completions
}

#' The response to a textDocument/completion request
#' @keywords internal
completion_reply <- function(id, uri, workspace, document, point) {
    if (!check_scope(uri, document, point)) {
        return(Response$new(
            id,
            result = list(
                isIncomplete = FALSE,
                items = list()
            )))
    }
    completions <- list()
    token_result <- document$detect_token(point, forward = FALSE)

    full_token <- token_result$full_token
    token <- token_result$token
    package <- token_result$package

    if (nzchar(full_token)) {
        if (is.null(package)) {
            completions <- c(
                completions,
                constant_completion(token),
                package_completion(token),
                scope_completion(uri, workspace, token, point))
        }
        completions <- c(
            completions,
            workspace_completion(
                workspace, token, package, token_result$accessor == "::"))
    }

    call_result <- document$detect_call(point)
    if (nzchar(call_result$token)) {
        completions <- c(
            completions,
            arg_completion(workspace, token,
                call_result$token, call_result$package,
                exported_only = call_result$accessor != ":::"))
    }

    logger$info("completions: ", length(completions))

    Response$new(
        id,
        result = list(
            isIncomplete = FALSE,
            items = completions
        )
    )
}

#' The response to a completionItem/resolve request
#' @keywords internal
completion_item_resolve_reply <- function(id, workspace, params) {
    resolved <- FALSE
    if (is.null(params$data) || is.null(params$data$type)) {
    } else {
        if (params$data$type == "package") {
            if (length(find.package(params$label))) {
                desc <- utils::packageDescription(params$label, fields = c("Title", "Description"))
                description <- paste0(trimws(
                    strsplit(desc$Description, split = "\n")[[1]]), collapse = " ")
                params$documentation <- list(
                    kind = "markdown",
                    value = sprintf("**%s**\n\n%s", desc$Title, description)
                )
                resolved <- TRUE
            }
        } else if (params$data$type == "parameter") {
            doc <- workspace$get_documentation(params$data$funct, params$data$package, isf = TRUE)
            doc_string <- NULL
            if (is.list(doc)) {
                doc_string <- doc$arguments[[params$label]]
            }
            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown", value = doc_string)
                resolved <- TRUE
            }
        } else if (params$data$type %in% c("constant", "function", "nonfunction", "lazydata")) {
            doc <- workspace$get_documentation(params$label, params$data$package,
                isf = params$data$type == "function")
            doc_string <- NULL
            if (is.character(doc)) {
                doc_string <- paste0(doc, collapse = "\n\n")
            } else if (is.list(doc) && is.character(doc$description)) {
                doc_string <- doc$description
            }

            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown", value = doc_string)
                resolved <- TRUE
            }
        }
    }

    if (resolved) {
        params$data <- NULL
        Response$new(
            id,
            result = params
        )
    } else {
        Response$new(id)
    }
}
