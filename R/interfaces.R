#' a position in a text document
#'
#' @param line an integer
#' @param character an integer
position <- function(line, character) {
  structure(
    list(
      line = line,
      character = character
    ),
    class = "position"
  )
}

#' a range in a text document
#'
#' @param start a [position]
#' @param end a [position]
range <- function(start, end) {
  structure(
    list(
      start = start,
      end   = end
    ),
    class = "range"
  )
}

#' a location inside a resource
#'
#' @template uri
#' @param range a [range]
location <- function(uri, range) {
  structure(
    list(
      uri   = uri,
      range = range
    ),
    class = "location"
  )
}

#' a textual edit applicable to a text document
#'
#' @param range a [range], the part of the document to replace
#' @param new_text a character, the text to replace
text_edit <- function(range, new_text) {
  structure(
    list(
      range    = range,
      new_text = new_text
    ),
    class = "text_edit"
  )
}
