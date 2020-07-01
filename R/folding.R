FoldingRangeKind <- list(
  Comment = "comment",
  Imports = "imports",
  Region = "region"
)

#' Get all the folding ranges in the document
#' @keywords internal
document_folding_range_reply <- function(id, uri, workspace, document) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    result <- NULL
    sections <- get_document_sections(uri, document, type = c("section", "chunk"))
    section_folding_ranges <- lapply(sections, function(section) {
        list(
            startLine = section$start_line - 1,
            endLine = section$end_line - 1,
            kind = FoldingRangeKind$Region
        )
    })
    result <- c(result, section_folding_ranges)

    if (length(result) == 0) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
