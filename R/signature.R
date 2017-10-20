get_signature <- function(obj) {
   sig <- utils::capture.output(print(base::args(obj)))
   sig <- sig[1:length(sig) - 1]
   sig <- gsub("^\\s+", "", sig)
   trimws(paste0(sig))
}

signature_reply <- function(id, document, position) {
    line <- document_line(document, position$line + 1)
    character <- position$character

    SignatureInformation <- list()

    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = 0
        )
    )
}
