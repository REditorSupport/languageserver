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

.CompletionEnv <- new.env()

guess_token <- function(line, end) {
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(end)
    token <- utils:::.guessTokenFromLine()
    logger$info("token: ", token)
    token
}

default_completion <- function(token) {
    # token <- utils:::.guessTokenFromLine(update = FALSE)

    completions <- list()

    logger$info("completing: ", token)
    utils:::.completeToken()
    comps <- utils:::.retrieveCompletions()
    comps <- stringr::str_replace(comps, "=", " = ")
    comps <- stringr::str_replace(comps, "<-", " <- ")

    for (i in seq_along(comps)) {
        completions[[i]] <- list(label = comps[i])
    }

    logger$info("comps: ", comps)

    completions
}

load_packaages <- function(document) {
    result <- stringr::str_match_all(document, "^(?:library|require)\\(['\"]?(.*?)['\"]?\\)")
    for (j in seq_along(result)) {
        if (nrow(result[[j]]) >= 1) {
            logger$info("load package: ", result[[j]][1, 2])

            tryCatch({
                suppressPackageStartupMessages(library(result[[j]][1, 2], character = TRUE))
                },
                error = function(e) NULL)
        }
    }
}

package_completion <- function(token) {
    installed_packages <- rownames(installed.packages())
    completions <- list()

    for (package in installed_packages) {
        if (startsWith(package, token)) {
            completions <- append(completions, list(list(
                label = paste0(package, "::"),
                kind = CompletionItemKind$Module
            )))
        }
    }
    completions
}

completion_reply <- function(id, document, position) {
    load_packaages(document)
    line <- document_line(document, position$line + 1)
    token <- guess_token(line, position$character)

    providers <- c(
        default_completion,
        package_completion
    )

    completions <- list()

    for (provider in providers) {
        provider_completion <- provider(token)
        if (length(provider_completion) > 0) {
            completions <- append(completions, provider_completion)
        }
    }

    logger$info("completions: ", completions)


    Response$new(
        id,
        result = list(
            isIncomplete = TRUE,
            items = completions
        )
    )
}
