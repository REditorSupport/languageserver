CodeActionKind <- list(
    Empty = "",
    QuickFix = "quickfix",
    Refactor = "refactor",
    RefactorExtract = "refactor.extract",
    RefactorInline = "refactor.inline",
    RefactorRewrite = "refactor.rewrite",
    Source = "source",
    SourceOrganizeImports = "source.organizeImports"
)

#' The response to a textDocument/codeAction Request
#'
#' @keywords internal
document_code_action_reply <- function(id, uri, workspace, document, range, context) {
    # result <- NULL

    # if (is.null(document$diagnostics) ||
    #   (!is.null(document$diagnostics$version) && document$diagnostics$version != document$version)) {
    #   return(NULL)
    # }

    # diagnostics <- document$diagnostics$data

    result <- list()

    for (item in context$diagnostics) {
        if (length(item$data$fix) == 0) next
        item_range <- list(
            start = document$from_lsp_position(item$range$start),
            end = document$from_lsp_position(item$range$end)
        )
        logger$info(item_range)
        text <- get_range_text(
            document$content,
            item_range$start$row + 1, item_range$start$col + 1,
            item_range$end$row + 1, item_range$end$col
        )
        changes <- list(
            list(
                text_edit(range = item$range, new_text = item$data$fix)
            )
        )
        names(changes) <- uri
        action <- list(
            title = sprintf("Replace '%s' with '%s'",
                text,
                item$data$fix
            ),
            kind = CodeActionKind$QuickFix,
            edit = list(
                changes = changes
            )
        )
        result <- c(result, list(action))
    }

    logger$info("document_code_action_reply: ", list(
        uri = uri,
        range = range,
        context = context,
        result = result
    ))

    Response$new(id, result = result)
}
