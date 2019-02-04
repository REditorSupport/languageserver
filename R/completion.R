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

#' complete a package name
#'
#' @param token a character, the start of the package name to identify
#'
#' @return a list of candidates
package_completion <- function(token) {
    installed_packages <- rownames(utils::installed.packages())
    token_packages <- installed_packages[startsWith(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package, kind = CompletionItemKind$Module)
    })
    completions
}

#' complete a function argument
#'
#' @param workspace a [Workspace] object
#' @param token a character, the start of the argument to identify
#' @param closure a closure object, the function for which `token` may be an argument
#'
#' @return a list of candidates
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

#' complete any object in the workspace
#'
#' This function works by first checking if `full_token` is of the form
#' `object`, `package::object` or `package:::object`. In the first two cases,
#' it will look into all exported objects of loaded packages (or just `package`
#' for the second case) and return any mathching objects. For the last case,
#' it will look at the unexported objects of `package.`
#'
#' @param workspace a [Workspace] object
#' @param full_token a character, the object to identify
#'
#' @return a list of candidates
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
        for (nsname in c("_workspace_", packages)) {
            ns <- workspace$get_namespace(nsname)
            functs <- ns$functs[startsWith(ns$functs, token)]
            if (nsname == "_workspace_") {
                tag <- "[workspace]"
            } else {
                tag <- paste0("{", nsname, "}")
            }
            functs_completions <- lapply(functs, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Function,
                     detail = tag)
            })
            nonfuncts <- ns$nonfuncts[startsWith(ns$nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag)
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

#' the response to a completion request
#'
#' @template reply-parameters
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

    if (nzchar(token)) {
        completions <- c(
            completions,
            package_completion(token),
            workspace_completion(workspace, token))
    }

    if (length(closure) > 0) {
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
