#' Bounded generated documents owned by one language server
#' @noRd
TextDocumentContent <- R6::R6Class("TextDocumentContent",
    private = list(
        documents = NULL,
        refresh = NULL
    ),
    public = list(
        scheme = NULL,
        initialize = function(refresh = NULL, max_bytes = 8 * 1024^2,
            max_entries = 128L) {
            # VS Code registers content providers by scheme. Different R
            # workspaces and sessions must not replace one another's provider.
            self$scheme <- tolower(basename(tempfile(
                paste0("r-lsp-", Sys.getpid(), "-"))))
            private$documents <- ByteLruCache$new(max_bytes, max_entries)
            private$refresh <- refresh
        },
        set = function(uri, text) {
            if (!is.character(uri) || length(uri) != 1L || is.na(uri) ||
                    !startsWith(uri, paste0(self$scheme, ":/")) ||
                    !is.character(text) || length(text) != 1L || is.na(text)) {
                return(FALSE)
            }
            previous <- private$documents$get(uri)
            private$documents$set(uri, text)
            if (!private$documents$has(uri)) return(FALSE)
            # Refresh is URI-scoped. Also refresh a newly restored cache entry:
            # the editor might still have an older copy open after eviction.
            if (!identical(previous, text) && is.function(private$refresh)) {
                private$refresh(uri)
            }
            TRUE
        },
        get = function(uri) {
            # Only exact URIs issued by definition replies are allowed. Do not
            # interpret a client URI as a package name or filesystem path.
            if (!is.character(uri) || length(uri) != 1L || is.na(uri)) return(NULL)
            private$documents$get(uri)
        },
        clear = function() private$documents$clear(),
        size = function() private$documents$size(),
        bytes = function() private$documents$bytes()
    )
)

#' Negotiate the generated-document provider after initialization settings
#' @noRd
configure_text_document_content <- function(self, server_capabilities) {
    client <- self$ClientCapabilities$workspace$textDocumentContent
    if (!is.list(client) ||
            isFALSE(server_capabilities$workspace$textDocumentContent)) {
        self$text_document_content <- NULL
        server_capabilities$workspace$textDocumentContent <- NULL
        return(server_capabilities)
    }
    self$text_document_content <- TextDocumentContent$new(refresh = function(uri) {
        if (isTRUE(self$exit_flag)) return(invisible(NULL))
        self$deliver(self$request("workspace/textDocumentContent/refresh", list(uri = uri)),
            callback = function(self, result, error = NULL) invisible(NULL))
    })
    server_capabilities$workspace$textDocumentContent <- list(
        schemes = list(self$text_document_content$scheme))
    server_capabilities
}

#' A URI segment preserved by editor URI converters
#' @noRd
text_document_content_segment <- function(value) {
    if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", value)) return(value)
    # Percent escapes for operators can be normalized differently by clients.
    # A reserved prefix and UTF-8 hex retain exact cache identity without that
    # ambiguity, and cannot collide with the plain-identifier branch above.
    paste0("~", paste(format(charToRaw(enc2utf8(value))), collapse = ""))
}

#' Serve a previously generated read-only package definition
#' @noRd
workspace_text_document_content <- function(self, id, params) {
    uri <- if (is.list(params)) params$uri
    if (!is.character(uri) || length(uri) != 1L || is.na(uri) || !nzchar(uri)) {
        return(self$deliver(ResponseErrorMessage$new(id, "InvalidParams",
                    "A text document URI is required")))
    }
    content <- self$text_document_content
    text <- if (!is.null(content)) content$get(uri)
    if (is.null(text)) {
        return(self$deliver(ResponseErrorMessage$new(id, "RequestFailed",
                    "Generated document is unavailable; request its definition again")))
    }
    self$deliver(Response$new(id, list(text = text)))
}
