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
    completions <- list()

    for (package in installed_packages) {
        if (startsWith(package, token)) {
            completions <- append(completions, list(list(
                label = package,
                kind = CompletionItemKind$Module
            )))
        }
    }
    completions
}

arg_completion <- function(workspace, token, closure) {
    completions <- list()

    args <- workspace$get_formals(closure$funct, closure$package)
    for (arg in names(args)) {
        if (startsWith(arg, token)) {
            completions <- append(completions, list(list(
                label = arg,
                kind = CompletionItemKind$Variable
            )))
        }
    }
    completions
}

workspace_complection <- function(workspace, full_token) {
    completions <- list()

    matches <- stringr::str_match(
        full_token, "(?:([a-zA-Z][a-zA-Z0-9]+)(:::?))?([a-zA-Z0-9_.]*)$")

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
            for (object in ns$functs) {
                if (startsWith(object, token)) {
                    completions <- append(completions, list(list(
                        label = object,
                        kind = CompletionItemKind$Function,
                        detail = paste0("{", nsname, "}")
                    )))
                }
            }
            for (object in ns$nonfuncts) {
                if (startsWith(object, token)) {
                    completions <- append(completions, list(list(
                        label = object,
                        kind = CompletionItemKind$Field,
                        detail = paste0("{", nsname, "}")
                    )))
                }
            }
        }
    } else {
        ns <- workspace$get_namespace(pkg)
        for (object in ns$unexports) {
            if (startsWith(object, token)) {
                completions <- append(completions, list(list(
                    label = object,
                    detail = paste0("{", pkg, "}")
                )))
            }
        }
    }

    completions
}

completion_reply <- function(id, uri, workspace, document, position) {
    line <- position$line
    character <- position$character

    if (!check_scope(uri, document, line)) {
        Response$new(
            id,
            result = list(
                items = NULL
            )
        )
        return(invisible(NULL))
    }

    token <- detect_token(document, line, character)
    logger$info("token: ", token)
    closure <- detect_closure(document, line, character)
    logger$info("closure: ", closure)

    completions <- list()

    if (nchar(token) > 0) {
        completions <- c(
            completions,
            package_completion(token),
            workspace_complection(workspace, token))
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
