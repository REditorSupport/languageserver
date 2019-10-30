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
        list(label = const, kind = CompletionItemKind$Constant)
    })
}

#' Complete a package name
#' @keywords internal
package_completion <- function(token) {
    installed_packages <- rownames(utils::installed.packages())
    token_packages <- installed_packages[startsWith(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package, kind = CompletionItemKind$Module)
    })
    completions
}

#' Complete a function argument
#' @keywords internal
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

#' Complete any object in the workspace
#' @keywords internal
workspace_completion <- function(workspace, token, package = NULL, exported_only = TRUE) {
    completions <- list()

    if (is.null(package)) {
        packages <- c("_workspace_", workspace$loaded_packages)
    } else {
        packages <- c(package)
    }

    if (is.null(package) || exported_only) {
        for (nsname in packages) {
            ns <- workspace$get_namespace(nsname)
            if (is.null(ns)) {
                next
            }
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
            lazydata <- ns$lazydata[startsWith(ns$lazydata, token)]
            lazydata_completions <- lapply(lazydata, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag)
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions,
                lazydata_completions)
        }
    } else {
        ns <- workspace$get_namespace(package)
        if (!is.null(ns)) {
            unexports <- ns$unexports[startsWith(ns$unexports, token)]
            unexports_completion <- lapply(unexports, function(object) {
                list(
                    label = object,
                    kind = CompletionItemKind$Field,
                    detail = paste0("{", package, "}")
                )
            })
            completions <- c(completions, unexports_completion)
        }
    }

    completions
}

find_enclosing_scopes <- function(x, line, col) {
    xpath <- sprintf("//expr[(@line1 < %d or (@line1 = %d and @col1 <= %d)) and
        (@line2 > %d or (@line2 = %d and @col2 >= %d))]",
        line, line, col, line, line, col)
    xml2::xml_find_all(x, xpath)
}

scope_completion <- function(uri, workspace, token, position) {
    xml_doc <- workspace$get_parse_data(uri)$xml_doc
    if (is.null(xml_doc)) {
        return(list())
    }

    enclosing_scopes <- find_enclosing_scopes(xml_doc,
        position$line + 1, position$character + 1)

    symbol_formals <- xml2::xml_text(xml2::xml_find_all(enclosing_scopes,
        "expr[FUNCTION]/SYMBOL_FORMALS/text()"))
    left_assign_symbols <- xml2::xml_text(xml2::xml_find_all(enclosing_scopes,
        "expr/LEFT_ASSIGN/preceding-sibling::expr/SYMBOL/text()"))
    right_assign_symbols <- xml2::xml_text(xml2::xml_find_all(enclosing_scopes,
        "expr/RIGHT_ASSIGN/following-sibling::expr/SYMBOL/text()"))
    equal_assign_symbols <- xml2::xml_text(xml2::xml_find_all(enclosing_scopes,
        "equal_assign/expr[1]/SYMBOL/text()"))
    for_symbols <- xml2::xml_text(xml2::xml_find_all(enclosing_scopes,
        "forcond/SYMBOL/text()"))
    scope_symbols <- unique(c(symbol_formals, left_assign_symbols,
        right_assign_symbols, equal_assign_symbols, for_symbols))

    scope_symbols <- scope_symbols[startsWith(scope_symbols, token)]
    completions <- lapply(scope_symbols, function(symbol) {
        list(
            label = symbol,
            kind = CompletionItemKind$Field,
            detail = "[scope]"
        )
    })
}

#' The response to a textDocument/completion request
#' @keywords internal
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
                constant_completion(token),
                package_completion(token),
                scope_completion(uri, workspace, token, position))
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
