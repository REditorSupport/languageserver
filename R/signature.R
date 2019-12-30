#' the response to a textDocument/signatureHelp Request
#'
#' If the symbol at the current position is a function, return its arguments
#' (as with [base::args()]).
#'
#' @keywords internal
signature_reply <- function(id, uri, workspace, document, point) {

    if (!check_scope(uri, document, point)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    result <- document$detect_call(point)

    SignatureInformation <- list()
    activeSignature <- -1

    if (nzchar(result$token)) {
        sig <- workspace$get_signature(result$token, result$package,
            exported_only = result$accessor != ":::")
        logger$info("sig: ", sig)
        if (!is.null(sig)) {
            doc <- workspace$get_documentation(result$token, result$package, isf = TRUE)
            doc_string <- ""
            if (is.character(doc)) {
                doc_string <- doc
            } else if (is.list(doc) && is.character(doc$description)) {
                doc_string <- doc$description
            }
            documentation <- list(kind = "markdown", value = doc_string)

            SignatureInformation <- list(list(
                label = sig,
                documentation = documentation
            ))
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
