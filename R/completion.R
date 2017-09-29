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

response_completion <- function(self, id, uri, position) {
    path <- parse_uri(uri)
    lineno <- position$line + 1
    character <- position$character
    lines <- self$document_cache$get(uri)
    logger$info("position: ", position)

    if (lineno < length(lines)) {
        line <- lines[[lineno]]
    } else {
        line <- ""
    }
    logger$info("line: ", line)

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

    self$deliver(Response$new(
        id,
        result = list(
            isIncomplete = TRUE,
            items = completions
        )
    ))
}
