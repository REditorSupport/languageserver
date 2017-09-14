#' @export
run <- function(debug = FALSE, stdin = "stdin", stdout = "stdout") {
    logger$set_mode(debug = debug)
    langserver <- LanguageServer$new(stdout = stdout)
    content <- file(stdin)
    open(content, blocking = TRUE)
    while (TRUE) {
        ret <- try({
            header <- readLines(content, n = 1)
            if (str_empty(header))
                next
            logger$info("received: ", header)

            matches <- stringr::str_match(header[1], "Content-Length: ([0-9]+)")
            if (is.na(matches[2]))
                stop("Unexpected input: ", header)

            empty_line <- readLines(content, n = 1)
            if (!str_empty(empty_line))
                stop("Unexpected non-empty line")

            data <- readChar(content, as.numeric(matches[2]), useBytes = TRUE)
            langserver$handle_raw(data)
        })
        if (inherits(ret, "error") || isTRUE(langserver$will_exit)) {
            logger$error("exiting")
            break
        }
    }
    close(content)
}
