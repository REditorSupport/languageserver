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

find_token <- function(line) {
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(nchar(line))
    token <- utils:::.guessTokenFromLine()
    logger$info("token: ", token)
    token
}

default_completion <- function(token) {
    logger$info("completing: ", token)
    # token <- utils:::.guessTokenFromLine(update = FALSE)
    utils:::.completeToken()
    comps <- utils:::.retrieveCompletions()
    comps <- stringr::str_replace(comps, "=", " = ")
    comps <- stringr::str_replace(comps, "<-", " <- ")

    logger$info("comps: ", comps)

    completions <- list()
    for (i in seq_along(comps)) {
        completions[[i]] <- list(label = comps[i])
    }

    completions
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
    line <- document_line(document, position$line + 1)
    character <- position$character
    if (nchar(line) > character) {
        line <- stringr::str_sub(line, end = character)
    }

    token <- find_token(line)

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
