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
    installed_packages <- rownames(installed.packages())
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

arg_completion <- function(workspace, funct, package = NULL) {
    list()
}

workspace_complection <- function(workspace, full_token) {
    completions <- list()

    matches <- stringr::str_match(
        full_token, "(?:([a-zA-Z][a-zA-Z0-9]+)(:::?))?([a-zA-Z0-9_.]*)$")

    pkgname <- matches[2]
    exported_only <- matches[3] == "::"
    token <- matches[4]

    if (is.na(pkgname)) {
        packages <- workspace$loaded_packages
    } else {
        packages <- c(pkgname)
    }

    if (is.na(pkgname) || exported_only) {
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
        ns <- workspace$get_namespace(pkgname)
        for (object in ns$unexports) {
            if (startsWith(object, token)) {
                completions <- append(completions, list(list(
                    label = object,
                    detail = paste0("{", pkgname, "}")
                )))
            }
        }
    }

    completions
}

completion_reply <- function(id, workspace, document, position) {
    line <- position$line
    character <- position$character

    token <- detect_token(document, line, character)
    closure <- detect_closure(document, line, character)

    completions <- c(
        workspace_complection(workspace, token),
        package_completion(token)
    )

    logger$info("completions: ", length(completions))

    Response$new(
        id,
        result = list(
            isIncomplete = TRUE,
            items = completions
        )
    )
}
