signature_cache <- list()

get_signature <- function(fn) {
    sig <- utils::capture.output(print(base::args(fn)))
    sig <- sig[1:length(sig) - 1]
    sig <- paste(gsub("^\\s+", "", sig), collapse = "")
    trimws(gsub("function ", fn, sig))
}

signature_reply <- function(id, document, position) {


    line <- document_line(document, position$line + 1)
    character <- position$character
    prechar <- substr(line, character, character)
    if (character > 1 && prechar == "(") {
        func_name <- str_extract_match(
            "([a-zA-Z][a-zA-Z0-9]+::)?[a-zA-Z0-9.][a-zA-Z0-9_.]+(?=\\($)",
            substr(line, 1, character))
        logger$info("func_name: ", func_name)

        SignatureInformation <- list(list(label = get_signature(func_name)))
    } else if (prechar == ",") {
        SignatureInformation <- list()
    } else {
        SignatureInformation <- list()
    }

    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = 0
        )
    )
}
