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
    result <- list()

    for (item in context$diagnostics) {
        item_range <- list(
            start = document$from_lsp_position(item$range$start),
            end = document$from_lsp_position(item$range$end)
        )
        logger$info(item_range)

        if (item_range$start$row == item_range$end$row &&
            item_range$start$col < item_range$end$col) {
            position <- document$to_lsp_position(
                item_range$end$row,
                nchar(document$line(item_range$end$row + 1))
            )
            changes <- list(
                list(
                    text_edit(range = range(
                        start = position,
                        end = position
                    ), " # nolint")
                )
            )
            names(changes) <- uri
            action <- list(
                title = "Disable all linters for this line",
                kind = CodeActionKind$QuickFix,
                edit = list(
                    changes = changes
                )
            )

            result <- c(result, list(action))
        }
    }

    logger$info("document_code_action_reply: ", list(
        uri = uri,
        range = range,
        context = context,
        result = result
    ))

    Response$new(id, result = result)
}
