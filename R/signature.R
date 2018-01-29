signature_reply <- function(id, workspace, document, position) {
    SignatureInformation <- list()
    activeSignature <- -1
    line <- position$line
    character <- position$character
    if (character > 0) {
        loc <- .Call(
            "document_backward_search", PACKAGE = "languageserver",
            document, line, character - 1, "(")
    } else {
        loc <- c(-1, -1)
    }
    logger$info("loc: ", loc)

    if (loc[1] >= 0 && loc[2] >= 0) {
        line <- document_line(document, loc[1] + 1)
        trim_line <- trimws(substr(line, 1, loc[2] + 1))
        logger$info("trim_line: ", trim_line)

        matches <- stringr::str_match(
            trim_line,
            "(?:([a-zA-Z][a-zA-Z0-9]+)::)?([a-zA-Z0-9.][a-zA-Z0-9_.]+)\\($")
        logger$info("func_name: ", matches)
        try({
            if (is.na(matches[2])) {
                sig <- workspace$get_signature(matches[3])
            } else {
                sig <- workspace$get_signature(matches[3], mathches[2])
            }
            sig <- trimws(gsub("function ", matches[3], sig))
            SignatureInformation <- list(list(label = sig))
            activeSignature <- 0
        }, silent = TRUE)
    }
    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = activeSignature
        )
    )
}
