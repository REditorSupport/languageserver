FoldingRangeKind <- list(
  Comment = "comment",
  Imports = "imports",
  Region = "region"
)

#' Get all the folding ranges in the document
#' @keywords internal
document_folding_range_reply <- function(id, uri, workspace, document) {
    result <- NULL

    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    xdoc <- parse_data$xml_doc
    if (!is.null(xdoc)) {
        exprs <- xml_find_all(xdoc, "//expr[@line2 > @line1]")
        expr_line1 <- as.integer(xml_attr(exprs, "line1"))
        expr_line2 <- as.integer(xml_attr(exprs, "line2"))
        expr_folding_ranges <- .mapply(function(line1, line2) {
            list(
                startLine = line1 - 1,
                endLine = line2 - 1,
                kind = FoldingRangeKind$Region
            )
        }, list(expr_line1, expr_line2), NULL)
        result <- c(result, expr_folding_ranges)
    }

    sections <- get_document_sections(uri, document, type = c("section", "chunk"))
    if (length(sections)) {
        section_folding_ranges <- lapply(sections, function(section) {
            list(
                startLine = section$start_line - 1,
                endLine = section$end_line - 1,
                kind = FoldingRangeKind$Region
            )
        })
        result <- c(result, section_folding_ranges)
    }

    result <- unique(result)
    if (length(result) == 0) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
