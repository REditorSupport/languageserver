#' position as defined in the LSP
#'
#' Position in a text document expressed as zero-based line and zero-based character offset. 
position <- function(line, character) {

    if (!is.numeric(line) | !is.numeric(character)) {
        stop("`position` requires numeric arguments!")
    }

    structure(
        list(
            line = line,
            character = character
        ),
        class = "lsp_position"
    )
}

print.lsp_position <- function(x, start_char = "", ...) {
    cat(start_char, "<Position> Line:", x$line, " | Character:", x$character)
}

#' range as defined in the LSP
#'
#' A range in a text document expressed as (zero-based) start and end positions. 
#' A range is comparable to a selection in an editor.
range <- function(start, end) {

    if (!inherits(start, "lsp_position") | !inherits(end, "lsp_position")) {
        stop("`range` requires 'lsp_position' parameters!")
    }

    structure(
        list(
            start = start,
            end = end
        ),
        class = "lsp_range"
    )
}

print.lsp_range <- function(x, start_char = "", ...) {
    cat(start_char, "<Range>\n")
    print(x$start, start_char = paste0(start_char, "\t"))
    cat("\n")
    print(x$end, start_char = paste0(start_char, "\t"))
}

#' location as defined in the LSP
#'
#' Represents a location inside a resource, such as a line inside a text file.
location  <- function(uri, range) {

    if (!inherits(range, "lsp_range")) {
        stop("`location` requires an 'lsp_range' parameter!")
    }

    if (!inherits(uri, "lsp_documenturi")) {
        stop("`location` requires an 'lsp_documenturi' parameter!")
    }

    structure(
        list(
            document_uri = uri,
            range = range
        ),
        class = "lsp_location"
    )
}

print.lsp_location <- function(x, ...) {
    cat("<Location>\n")
    print(x$document_uri, start_char = "\t")
    cat("\n")
    print(x$range, start_char = "\t")
}

document_uri  <- function(uri) {

    if (!is.character(uri)) {
        stop("`document_uri` requires a character parameter!")
    }

    structure(
        uri,
        class = "lsp_documenturi"
    )
}

print.lsp_documenturi <- function(x, start_char = "", ...) {
    cat(start_char, "<DocumentURI>", unclass(x))
}
