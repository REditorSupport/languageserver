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

completion_reply <- function(id, document, position) {
    line <- document_line(document, position$line + 1)
    character <- position$character
    if (nchar(line) > character) {
        line <- stringr::str_sub(line, end = character)
    }

    logger$info("completing: ", line)
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(nchar(line))
    token <- utils:::.guessTokenFromLine()
    # token <- utils:::.guessTokenFromLine(update = FALSE)
    utils:::.completeToken()
    logger$info("token: ", token)
    comps <- utils:::.retrieveCompletions()

    completions <- list()
    for (i in seq_along(comps)) {
        completions[[i]] <- list(label = comps[i])
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
