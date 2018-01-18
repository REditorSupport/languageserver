signature_cache <- list()

get_signature <- function(obj) {
    sig <- utils::capture.output(print(base::args(obj)))
    sig <- sig[1:length(sig) - 1]
    sig <- gsub("^\\s+", "", sig)
    trimws(paste0(sig))
}

signature_reply <- function(id, document, position) {
    line <- document_line(document, position$line + 1)
    character <- position$character

    if (character > 1 && substr(line, character, character) == "(") {
        func_name <- guess_token(line, character - 1)
        logger$info("func_name: ", func_name)
    } else if (substr(line, character, character) == ",") {
        # TODO
    } else {
        # should not be reached
    }

    SignatureInformation <- list()

    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = 0
        )
    )
}
