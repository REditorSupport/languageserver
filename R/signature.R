signature_reply <- function(id, uri, workspace, document, position) {
    line <- position$line
    character <- position$character

    if (!check_scope(uri, document, line)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    closure <- detect_closure(document, line, character)

    SignatureInformation <- list()
    activeSignature <- -1


    if (!is.null(closure$funct)) {
        expr <- attr(document, "expr")
        doc_closure <- expr$closures[[closure$funct]]
        if (is.null(doc_closure)) {
            if (is.null(closure$package)) {
                sig <- workspace$get_signature(closure$funct)
            } else {
                sig <- workspace$get_signature(closure$funct, closure$package)
            }
        } else {
            sig <- doc_closure$signature
        }

        logger$info("sig: ", sig)
        if (!is.null(sig)) {
            sig <- trimws(gsub("^function\\s?", closure$funct, sig))
            SignatureInformation <- list(list(label = sig))
            activeSignature <- 0
        }
    }

    Response$new(
        id,
        result = list(
            signatures = SignatureInformation,
            activeSignature = activeSignature
        )
    )
}
