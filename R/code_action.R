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
    listed_linters <- character()

    for (item in context$diagnostics) {
        item_range <- list(
            start = document$from_lsp_position(item$range$start),
            end = document$from_lsp_position(item$range$end)
        )

        if (item_range$start$row == item_range$end$row &&
            item_range$start$col < item_range$end$col) {
            line <- document$line(item_range$end$row + 1)

            position <- document$to_lsp_position(
                item_range$end$row,
                nchar(line)
            )

            logger$info("document_code_action_reply:", list(
                line = line,
                position = position
            ))

            if (!("*" %in% listed_linters)) {
                if (grepl("#\\s*nolint\\s*:", line)) {
                    # modify existing nolint directives
                    nolint_start <- regexec("#\\s*nolint\\s*:", line)[[1]] - 1
                    edit <- text_edit(range = range(
                        start = position(
                            item_range$end$row,
                            nolint_start
                        ),
                        end = position(
                            item_range$end$row,
                            nchar(line)
                        )
                    ), "# nolint")
                } else {
                    position <- document$to_lsp_position(
                        item_range$end$row,
                        nchar(line)
                    )
                    edit <- text_edit(range = range(
                        start = position,
                        end = position
                    ), " # nolint")
                }
                changes <- list(
                    list(edit)
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
                listed_linters <- c(listed_linters, "*")
            }

            if (!(item$code %in% listed_linters)) {
                if (grepl("#\\s*nolint\\s*:.+\\.", line)) {
                    # modify existing nolint directives
                    nolint_start <- regexec("#\\s*nolint\\s*:.+\\.", line)[[1]] - 1
                    nolint_end <- nolint_start + attr(nolint_start, "match.length") - 1
                    edit <- text_edit(range = range(
                        start = position(
                            item_range$end$row,
                            nolint_end
                        ),
                        end = position(
                            item_range$end$row,
                            nolint_end
                        )
                    ), sprintf(", %s", item$code))
                } else {
                    position <- document$to_lsp_position(
                        item_range$end$row,
                        nchar(line)
                    )
                    edit <- text_edit(range = range(
                        start = position,
                        end = position
                    ), sprintf(" # nolint: %s.", item$code))
                }
                changes <- list(
                    list(edit)
                )
                names(changes) <- uri
                action <- list(
                    title = sprintf("Disable %s for this line", item$code),
                    kind = CodeActionKind$QuickFix,
                    edit = list(
                        changes = changes
                    )
                )

                result <- c(result, list(action))
                listed_linters <- c(listed_linters, item$code)
            }
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
