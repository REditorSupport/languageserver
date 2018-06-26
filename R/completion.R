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
    Reference = 18
)

package_completion <- function(token) {
    installed_packages <- rownames(utils::installed.packages())
    token_packages <- installed_packages[startsWith(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package, kind = CompletionItemKind$Module)
    })
    completions
}

arg_completion <- function(workspace, token, closure) {
    args <- names(workspace$get_formals(closure$funct, closure$package))
    if (is.character(args)) {
        token_args <- args[startsWith(args, token)]
        completions <- lapply(token_args, function(arg) {
            list(label = arg, kind = CompletionItemKind$Variable)
        })
        completions
    }
}

workspace_completion <- function(workspace, full_token) {
    completions <- list()

    matches <- stringr::str_match(
        full_token, "(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?([a-zA-Z0-9_.]*)$")

    pkg <- matches[2]
    exported_only <- matches[3] == "::"
    token <- matches[4]

    if (is.na(pkg)) {
        packages <- workspace$loaded_packages
    } else {
        packages <- c(pkg)
    }

    if (is.na(pkg) || exported_only) {
        for (nsname in packages) {
            ns <- workspace$get_namespace(nsname)
            functs <- ns$functs[startsWith(ns$functs, token)]
            functs_completions <- lapply(functs, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Function,
                     detail = paste0("{", nsname, "}"))
            })
            nonfuncts <- ns$nonfuncts[startsWith(ns$nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = paste0("{", nsname, "}"))
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions)
        }
    } else {
        ns <- workspace$get_namespace(pkg)
        unexports <- ns$unexports[startsWith(ns$unexports, token)]
        unexports_completion <- lapply(unexports, function(object) {
            list(label = object,
                 detail = paste0("{", pkg, "}"))
        })
        completions <- c(completions, unexports_completion)
    }

    completions
}

completion_reply <- function(id, uri, workspace, document, position) {
    line <- position$line
    character <- position$character

    if (!check_scope(uri, document, line)) {
        return(Response$new(id))
    }

    token <- detect_token(document, line, character)
    logger$info("token: ", token)
    closure <- detect_closure(document, line, character)
    logger$info("closure: ", closure)

    completions <- list()

    expr <- attr(document, "expr")

    if (nzchar(token)) {
        if (length(expr)) {
            variables <- character()
            closures <- character()
            for (e in expr) {
                if (length(e) == 3L &&
                        (e[[1L]] == "<-" || e[[1L]] == "=") &&
                        is.symbol(e[[2L]]) &&
                        startsWith(symbol <- as.character(e[[2L]]), token)) {
                    if (is.call(e[[3L]]) && e[[3L]][[1L]] == "function") {
                        closures <- union(closures, symbol)
                    } else {
                        variables <- union(variables, symbol)
                    }
                }
            }
            completions <- c(completions, lapply(variables, function(symbol) {
                list(label = symbol, kind = CompletionItemKind$Variable, detail = basename(uri))
            }))
            completions <- c(completions, lapply(closures, function(symbol) {
                list(label = symbol, kind = CompletionItemKind$Function, detail = basename(uri))
            }))
        }
        completions <- c(
            completions,
            package_completion(token),
            workspace_completion(workspace, token))
    }

    if (length(closure) > 0) {
        if (length(expr)) {
            closure_args <- NULL
            for (e in expr) {
                if (length(e) == 3L &&
                        (e[[1L]] == "<-" || e[[1L]] == "=") &&
                        is.symbol(e[[2L]]) &&
                        e[[2L]] == closure &&
                        is.call(e[[3L]]) &&
                        e[[3L]][[1L]] == "function") {
                    closure_args <- names(e[[3L]][[2L]])
                    break
                }
            }
            completions <- c(completions, lapply(closure_args, function(symbol) {
                list(label = symbol, kind = CompletionItemKind$Variable)
            }))
        }
        completions <- c(
            completions,
            arg_completion(workspace, token, closure))
    }

    logger$info("completions: ", length(completions))

    Response$new(
        id,
        result = list(
            isIncomplete = TRUE,
            items = completions
        )
    )
}
