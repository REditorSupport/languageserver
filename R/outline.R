#' Syntax-only facts shared by document outline providers
#' @noRd
outline_parse_data <- function(syntax) {
    comments <- syntax$order[syntax$token[syntax$order] == "COMMENT"]
    opening <- which(syntax$token %in% c("'('", "'['", "'{'") &
            syntax$parent_row > 0L)
    closing <- which(syntax$token %in% c("')'", "']'", "'}'") &
            syntax$parent_row > 0L)
    # Each expression's first direct opener/closer is exactly the pair selected
    # by the XML union. LBB ([[) deliberately retains the legacy exclusion.
    first <- last <- integer(length(syntax$token))
    opening <- opening[order(syntax$order_rank[opening])]
    closing <- closing[order(syntax$order_rank[closing])]
    opening <- opening[!duplicated(syntax$parent_row[opening])]
    closing <- closing[!duplicated(syntax$parent_row[closing])]
    first[syntax$parent_row[opening]] <- opening
    last[syntax$parent_row[closing]] <- closing
    rows <- syntax$order[syntax$token[syntax$order] == "expr"]
    rows <- rows[first[rows] > 0L & last[rows] > 0L]
    rows <- rows[syntax$line1[rows] < syntax$line2[rows] &
            syntax$line1[first[rows]] < syntax$line1[last[rows]]]
    blocks <- list(syntax$line1[first[rows]], syntax$line1[last[rows]] - 1L)
    list(
        comments = list(text = syntax$text[comments],
            line = syntax$line1[comments], col = syntax$col1[comments]),
        blocks = blocks, block_index = outline_block_index(blocks)
    )
}

#' Build interval containment once instead of comparing every pair of blocks
#' @noRd
outline_block_index <- function(blocks) {
    if (is.null(blocks) || !length(blocks[[1L]])) {
        return(list(blocks = blocks, children = list()))
    }
    pairs <- unique(cbind(blocks[[1L]], blocks[[2L]]))
    blocks <- list(pairs[, 1L], pairs[, 2L])
    n <- nrow(pairs)
    parent <- integer(n)
    stack <- integer(n)
    depth <- 0L
    for (i in order(blocks[[1L]], -blocks[[2L]])) {
        while (depth > 0L && blocks[[1L]][[i]] > blocks[[2L]][[stack[[depth]]]]) {
            depth <- depth - 1L
        }
        if (depth > 0L) {
            enclosing <- stack[[depth]]
            # Ranges are normally laminar. Retain the original containment
            # implementation for a crossing interval from non-parser XML.
            if (blocks[[2L]][[i]] > blocks[[2L]][[enclosing]]) return(NULL)
            parent[[i]] <- enclosing
        }
        depth <- depth + 1L
        stack[[depth]] <- i
    }
    list(blocks = blocks, children = split(seq_len(n), parent))
}

#' Preserve recursive section ordering using indexed direct block children
#' @noRd
outline_document_ranges <- function(content, index, symbol) {
    blocks <- index$blocks
    visit <- function(start_line, end_line, children) {
        if (start_line > end_line) return(NULL)
        children <- children[blocks[[1L]][children] >= start_line &
                blocks[[2L]][children] <= end_line &
                !(blocks[[1L]][children] == start_line & blocks[[2L]][children] == end_line)]
        ranges <- lapply(children, function(i) {
            visit(blocks[[1L]][[i]], blocks[[2L]][[i]], index$children[[as.character(i)]])
        })
        highest <- if (length(children)) lapply(blocks, `[`, children) else NULL
        lines <- outline_lines_out_blocks(start_line, end_line, highest)
        list(
            sections = c(get_r_document_section_ranges(content, lines),
                unlist(lapply(ranges, `[[`, "sections"), recursive = FALSE, use.names = FALSE)),
            binary_ranges = c(if (!symbol) get_r_document_binary_ranges(content, lines, highest),
                unlist(lapply(ranges, `[[`, "binary_ranges"), recursive = FALSE, use.names = FALSE))
        )
    }
    visit(1L, length(content), index$children[["0"]])
}

#' Materialize uncovered line runs without expanding every nested block
#' @noRd
outline_lines_out_blocks <- function(start_line, end_line, blocks) {
    if (is.null(blocks) || !length(blocks[[1L]])) return(seq.int(start_line, end_line))
    ordered <- order(blocks[[1L]], blocks[[2L]])
    starts <- blocks[[1L]][ordered]
    ends <- blocks[[2L]][ordered]
    pieces <- vector("list", length(starts) + 1L)
    cursor <- start_line
    for (i in seq_along(starts)) {
        if (cursor < starts[[i]]) pieces[[i]] <- seq.int(cursor, starts[[i]] - 1L)
        cursor <- max(cursor, ends[[i]] + 1L)
    }
    if (cursor <= end_line) pieces[[length(pieces)]] <- seq.int(cursor, end_line)
    # The final line is included even if it lies inside a block, as in the
    # original section implementation.
    unique(c(unlist(pieces, use.names = FALSE), end_line))
}
