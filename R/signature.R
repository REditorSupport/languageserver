signature_cache <- list()

get_signature <- function(fn) {
    sig <- utils::capture.output(print(base::args(fn)))
    sig <- sig[1:length(sig) - 1]
    sig <- paste(gsub("^\\s+", "", sig), collapse = "")
    trimws(gsub("function ", fn, sig))
}

signature_reply <- function(id, document, position) {
    SignatureInformation <- list()
    activeSignature <- -1

    line <- document_line(document, position$line + 1)
    character <- position$character
    trim_line <- trimws(substr(line, 1, character))
    prechar <- substr(trim_line, nchar(trim_line), nchar(trim_line))
    logger$info("prechar: ", prechar)

    if (character > 1 && prechar == "(") {
        func_name <- str_extract_match(
            "([a-zA-Z][a-zA-Z0-9]+::)?[a-zA-Z0-9.][a-zA-Z0-9_.]+(?=\\($)",
            trim_line)
        logger$info("func_name: ", func_name)
        try({
            sig <- get_signature(func_name)
            SignatureInformation <- list(list(label = sig))
            activeSignature <- 0
        }, silent = TRUE)
    } else if (prechar == ",") {

    } else {

    }
    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = activeSignature
        )
    )
}
