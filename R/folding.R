FoldingRangeKind <- list(
  Comment = "comment",
  Imports = "imports",
  Region = "region"
)

#' Get all the folding ranges in the document
#' @keywords internal
document_folding_range_reply <- function(id, uri, workspace, document) {
    result <- NULL
    section_symbols <- NULL
    section_lines <- seq_len(document$nline)
    section_lines <- section_lines[
        grep("^\\#+\\s*(.+)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            document$content[section_lines])]
    if (length(section_lines)) {
        section_names <- trimws(gsub("^\\#+\\s*(.+)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            "\\1", document$content[section_lines]))
        section_end_lines <- c(section_lines[-1] - 1, document$nline)
        section_folding_ranges <- .mapply(function(name, start_line, end_line) {
            list(
              startLine = start_line - 1,
              endLine = end_line - 1,
              kind = FoldingRangeKind$Region
            )
        }, list(section_names, section_lines, section_end_lines), NULL)
        result <- c(result, section_folding_ranges)
    }

    if (length(result) == 0) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
