#' Build serializable indexes for viewport providers in the parse worker
#' @noRd
range_provider_parse_data <- function(data, content) {
    empty <- list(
        variables = list(
            name = character(), line = integer(), col = integer(),
            end_line = integer(), end_col = integer()
        ),
        calls = list(
            name = character(), package = character(),
            first_argument = integer(), last_argument = integer()
        ),
        arguments = list(
            call = integer(), line = integer(), col = integer(),
            name = character(), symbol = character(), present = logical()
        ),
        argument_order = integer(),
        argument_lines = integer(),
        strings = list(line = integer(), col1 = integer(), col2 = integer())
    )
    if (is.null(data) || !nrow(data)) return(empty)

    index <- .Call("range_provider_index_c", data, PACKAGE = "languageserver")
    variable_rows <- index$variable
    variable_rows <- variable_rows[order(
        data$line1[variable_rows], data$col1[variable_rows]
    )]
    empty$variables <- list(
        name = data$text[variable_rows],
        line = as.integer(data$line1[variable_rows] - 1L),
        col = as.integer(data$col1[variable_rows] - 1L),
        end_line = as.integer(data$line2[variable_rows] - 1L),
        end_col = data$col2[variable_rows]
    )

    # Store UTF-16 positions once. Different columns on the same Unicode line
    # are converted together rather than rescanning that line per occurrence.
    non_ascii <- nchar(content, type = "bytes") != nchar(content, type = "chars")
    convert_columns <- function(lines, cols) {
        convert <- which(non_ascii[lines + 1L] & !is.na(lines))
        by_line <- split(convert, lines[convert])
        for (indices in by_line) {
            cols[indices] <- code_point_to_unit(content[[lines[indices[[1L]]] + 1L]], cols[indices])
        }
        cols
    }
    empty$variables$col <- convert_columns(empty$variables$line, empty$variables$col)
    empty$variables$end_col <- convert_columns(empty$variables$end_line, empty$variables$end_col)

    text_at <- function(rows) {
        result <- rep(NA_character_, length(rows))
        present <- rows > 0L
        result[present] <- data$text[rows[present]]
        result
    }
    empty$calls <- list(
        name = data$text[index[["function"]]],
        package = text_at(index$package),
        first_argument = index$first_argument,
        last_argument = index$last_argument
    )
    first <- index$argument_first
    present <- first > 0L
    line <- col <- rep(NA_integer_, length(first))
    line[present] <- data$line1[first[present]] - 1L
    col[present] <- data$col1[first[present]] - 1L
    empty$arguments <- list(
        call = index$argument_call,
        line = line,
        col = convert_columns(line, col),
        name = text_at(index$argument_name),
        symbol = text_at(index$argument_symbol),
        present = present
    )
    argument_order <- which(present)
    argument_order <- argument_order[order(line[argument_order], col[argument_order])]
    empty$argument_order <- argument_order
    empty$argument_lines <- line[argument_order]
    strings <- which(data$token == "STR_CONST" & data$line1 == data$line2 &
            data$col2 > data$col1 + 1L & data$col2 <= data$col1 + 256L)
    empty$strings <- list(
        line = data$line1[strings], col1 = data$col1[strings], col2 = data$col2[strings]
    )
    empty
}

#' Select an inclusive interval of a sorted line index without a full scan
#' @noRd
range_line_indices <- function(lines, first, last) {
    bounds <- .Call("range_line_bounds_c", lines, first, last, PACKAGE = "languageserver")
    if (bounds[[1L]] > bounds[[2L]]) integer() else seq.int(bounds[[1L]], bounds[[2L]])
}

#' Select positions in the half-open range used by LSP requests
#' @noRd
range_position_selected <- function(line, col, request_range) {
    start <- request_range$start
    end <- request_range$end
    (line > start$line | line == start$line & col >= start$character) &
        (line < end$line | line == end$line & col < end$character)
}
