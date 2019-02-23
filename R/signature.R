#' the response to a textDocument/signatureHelp Request
#'
#' If the symbol at the current position is a function, return its arguments
#' (as with [base::args()]).
#'
#' @template id
#' @template uri
#' @template workspace
#' @template document
#' @template position
#'
#' @return a [Response] object
signature_reply <- function(id, uri, workspace, document, position) {

    if (!check_scope(uri, document, position)) {
        return(Response$new(id, list(signatures = NULL)))
    }

    closure <- detect_closure(document, position)

    SignatureInformation <- list()
    activeSignature <- -1


    if (!is.null(closure$funct)) {
        if (is.null(closure$package)) {
            sig <- workspace$get_signature(closure$funct)
        } else {
            sig <- workspace$get_signature(closure$funct, closure$package)
        }

        logger$info("sig: ", sig)
        if (!is.null(sig)) {
            sig <- trimws(gsub("function\\s*", closure$funct, sig))
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
