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
#' @param funct a character, the function name
#' @param package a character, the optional package name
#'
#' @return a list of candidates
arg_completion <- function(workspace, token, funct, package = NULL) {
    args <- names(workspace$get_formals(funct, package))
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
#' @param workspace a [Workspace] object
#' @param token a character, the object to identify
#' @param package a character
#' @param exported_only a boolean
#'
#' @return a list of candidates
workspace_completion <- function(workspace, token, package = NULL, exported_only = TRUE) {
    completions <- list()

    if (is.null(package)) {
        packages <- workspace$loaded_packages
    } else {
        packages <- c(package)
    }

    if (is.null(package) || exported_only) {
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
        ns <- workspace$get_namespace(package)
        unexports <- ns$unexports[startsWith(ns$unexports, token)]
        unexports_completion <- lapply(unexports, function(object) {
            list(label = object,
                 kind = CompletionItemKind$Field,
                 detail = paste0("{", package, "}"))
        })
        completions <- c(completions, unexports_completion)
    }

    completions
}


#' the response to a textDocument/completion request
#'
#' @template id
#' @template uri
#' @template workspace
#' @template document
#' @template position
#'
#' @return a [Response] object
completion_reply <- function(id, uri, workspace, document, position) {
    if (!check_scope(uri, document, position)) {
        return(Response$new(
            id,
            result = list(
                isIncomplete = FALSE,
                items = list()
            )))
    }
    completions <- list()
    token_result <- document$detect_token(position, forward = FALSE)

    full_token <- token_result$full_token
    token <- token_result$token
    package <- token_result$package

    if (nzchar(full_token)) {
        if (is.null(package)) {
            completions <- c(
                completions,
                package_completion(token))
        }
        completions <- c(
            completions,
            workspace_completion(
                workspace, token, package, token_result$accessor == "::"))
    }

    call_result <- document$detect_call(position)
    if (nzchar(call_result$token)) {
        completions <- c(
            completions,
            arg_completion(workspace, token, call_result$token, call_result$package))
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
