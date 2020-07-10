FoldingRangeKind <- list(
  Comment = "comment",
  Imports = "imports",
  Region = "region"
)

get_expr_folding_ranges <- function(xdoc) {
    exprs <- xml_find_all(xdoc, "//expr[@line2 > @line1]")
    if (length(exprs) == 0) {
        return(NULL)
    }
    expr_line1 <- as.integer(xml_attr(exprs, "line1"))
    expr_line2 <- as.integer(xml_attr(exprs, "line2"))
    expr_folding_ranges <- .mapply(function(line1, line2) {
        list(
            startLine = line1 - 1,
            endLine = line2 - 1,
            kind = FoldingRangeKind$Region
        )
    }, list(expr_line1, expr_line2), NULL)
    expr_folding_ranges
}

get_comment_folding_ranges <- function(xdoc) {
    comments <- xml_find_all(xdoc, "//COMMENT")
    if (length(comments) == 0) {
        return(NULL)
    }
    comm_line1 <- as.integer(xml_attr(comments, "line1"))
    comm_col1 <- as.integer(xml_attr(comments, "col1"))
    comm_runs <- c(1L, diff(comm_col1) != 0)
    comm_groups <- cumsum(comm_runs)
    comm_max_group <- comm_groups[[length(comm_groups)]]
    comm_ranges <- lapply(seq_len(comm_max_group), function(i) {
        lines <- comm_line1[comm_groups == i]
        sub_groups <- cumsum(c(0L, diff(lines)) != 1L)
        max_sub_group <- sub_groups[[length(sub_groups)]]
        lapply(seq_len(max_sub_group), function(j) {
            lns <- lines[sub_groups == j]
            start_line <- lns[1]
            end_line <- lns[length(lns)]
            if (end_line > start_line) {
                c(start_line, end_line)
            }
        })
    })
    comm_ranges <- unlist(comm_ranges, recursive = FALSE, use.names = FALSE)
    comm_ranges <- comm_ranges[vapply(comm_ranges, length, integer(1)) > 0]
    comm_folding_ranges <- lapply(comm_ranges, function(item) {
        list(
            startLine = item[1] - 1,
            endLine = item[2] - 1,
            kind = FoldingRangeKind$Comment
        )
    })
    comm_folding_ranges
}

get_document_section_folding_ranges <- function(uri, document) {
    sections <- get_document_sections(uri, document, type = c("section", "chunk"))
    if (length(sections) == 0) {
        return(NULL)
    }
    section_folding_ranges <- lapply(sections, function(section) {
        list(
            startLine = section$start_line - 1,
            endLine = section$end_line - 1,
            kind = FoldingRangeKind$Region
        )
    })
    section_folding_ranges
}

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
        result <- c(result,
            get_expr_folding_ranges(xdoc),
            get_comment_folding_ranges(xdoc)
        )
    }

    result <- c(result, get_document_section_folding_ranges(uri, document))

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
