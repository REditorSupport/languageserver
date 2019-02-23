# base interfaces ---------------------------------------------------------

#' a position in a text document
#'
#' Position in a text document expressed as zero-based line and zero-based
#' character offset.
#'
#' @param line an integer
#' @param character an integer
position <- function(line, character) {

  if (!is.numeric(line) | !is.numeric(character)) {
    stop("`position` requires numeric arguments!")
  }

  structure(
    list(
      line = line,
      character = character
    ),
    class = "position"
  )
}

print.position <- function(x, start_char = "", ...) {
  cat(start_char, "<Position> Line:", x$line, " | Character:", x$character)
}

#' a range in a text document
#'
#' A range in a text document expressed as start and end [position]s.
#' A range is comparable to a selection in an editor.
#'
#' @param start a [position]
#' @param end a [position]
range <- function(start, end) {

  if (!inherits(start, "position") | !inherits(end, "position")) {
    stop("`range` requires 'position' parameters!")
  }

  structure(
    list(
      start = start,
      end   = end
    ),
    class = c("range", "list")
  )
}

print.range <- function(x, start_char = "", ...) {
  cat(start_char, "<Range>\n")
  print(x$start, start_char = paste0(start_char, "\t"))
  cat("\n")
  print(x$end, start_char = paste0(start_char, "\t"))
}


#' a location inside a resource
#'
#' Represents a location inside a resource, such as a line inside a text file.
#'
#' @template uri
#' @param range a [range]
location <- function(uri, range) {

  if (!inherits(range, "range")) {
    stop("`location` requires a 'range' parameter!")
  }

  # if (!inherits(uri, "document_uri")) {
  #   stop("`location` requires an 'document_uri' parameter!")
  # }

  structure(
    list(
      uri   = uri,
      range = range
    ),
    class = "location"
  )
}


print.location <- function(x, ...) {
  cat("<Location>\n")
  print(x$document_uri, start_char = "\t")
  cat("\n")
  print(x$range, start_char = "\t")
}

#' a document URI
#'
#' A Uniform Resource Identifier as defined by [RFC 3986](http://tools.ietf.org/html/rfc3986).
#'
#' @param uri a character
document_uri  <- function(uri) {

  if (!is.character(uri)) {
    stop("`document_uri` requires a character parameter!")
  }

  structure(
    uri,
    class = "document_uri"
  )
}

print.document_uri <- function(x, start_char = "", ...) {
  cat(start_char, "<DocumentURI>", unclass(x))
}

#' a textual edit applicable to a text document
#'
#' @param range a [range], the part of the document to replace
#' @param new_text a character, the text to replace
text_edit <- function(range, new_text) {
  structure(
    list(
      range    = range,
      newText = new_text
    ),
    class = "text_edit"
  )
}

#' a text document and a position inside that document
#'
#' @template uri
#' @template position
text_document_position_params <- function(uri, position) {
  structure(
    list(
      textDocument = uri,
      position      = position
    ),
    class = "text_document_position_params"
  )
}

# Request parameters ------------------------------------------------------

#' parameters for completion requests
#'
#' @template uri
#' @template position
#' @param context a named list
completion_params <- function(uri, position, context = NULL) {
  structure(
    list(
      textDocument = uri,
      position      = position,
      context       = context
    ),
    class = "completion_params"
  )
}

#' parameters for reference requests
#'
#' @template uri
#' @template position
#' @param context a named list
reference_params <- function(uri, position, context = NULL) {
  structure(
    list(
      textDocument = uri,
      position      = position,
      context       = context
    ),
    class = "reference_params"
  )
}

#' parameters for document symbol requests
#'
#' @template uri
document_symbol_params <- function(uri) {
  structure(
    list(
      textDocument = uri
    ),
    class = "document_symbol_params"
  )
}

#' parameters for code action requests
#'
#' @template uri
#' @param range a [range] object
#' @param context a named list
code_action_params <- function(uri, range, context = NULL) {
  structure(
    list(
      textDocument = uri,
      position      = position,
      context       = context
    ),
    class = "code_action_params"
  )
}

#' parameters for code lens requests
#'
#' @template uri
code_lens_params <- function(uri) {
  structure(
    list(
      textDocument = uri
    ),
    class = "code_lens_params"
  )
}

#' parameters for document link requests
#'
#' @template uri
document_link_params <- function(uri) {
  structure(
    list(
      textDocument = uri
    ),
    class = "document_link_params"
  )
}

#' parameters for document formatting requests
#'
#' @template uri
#' @param options a named list
document_formatting_params <- function(uri, options) {
  structure(
    list(
      textDocument = uri,
      options      = options
    ),
    class = "document_formatting_params"
  )
}

#' parameters for document range formatting requests
#'
#' @template uri
#' @param range a [range] object
#' @param options a named list
document_range_formatting_params <- function(uri, range, options) {
  structure(
    list(
      textDocument = uri,
      range        = range,
      options      = options
    ),
    class = "document_range_formatting_params"
  )
}

#' parameters for document on type formatting requests
#'
#' @template uri
#' @template position
#' @param character a single character
#' @param options a named list
document_on_type_formatting_params <- function(uri, position, character, options) {
  structure(
    list(
      textDocument = uri,
      position     = position,
      character    = character,
      options      = options
    ),
    class = "document_on_type_formatting_params"
  )
}

#' parameters for rename requests
#'
#' @template uri
#' @template position
#' @param new_name a character
rename_params <- function(uri, position, new_name) {
  structure(
    list(
      textDocument = uri,
      position     = position,
      newName      = new_name
    ),
    class = "rename_params"
  )
}

# Notification parameters -------------------------------------------------

#' parameters for didOpen notifications
#'
#' @template uri
did_open_text_document_params <- function(uri) {
  structure(
    list(
      textDocument = uri
    ),
    class = "did_open_text_document_params"
  )
}

#' parameters for didChange notifications
#'
#' @template uri
#' @param changes a list of text_document_content_change_event
did_change_text_document_params <- function(uri, changes) {
  structure(
    list(
      textDocument   = uri,
      contentChanges = changes
    ),
    class = "did_change_text_document_params"
  )
}

#' parameters for willSave notifications
#'
#' @template uri
#' @param reason an integer, see TextDocumentSaveReason
will_save_text_document_params <- function(uri, reason) {
  structure(
    list(
      textDocument   = uri,
      reason         = reason
    ),
    class = "will_save_text_document_params"
  )
}

TextDocumentSaveReason <- list(
  Manual = 1,
  AfterDelay = 2,
  FocusOut = 3
)

#' parameters for didSave notifications
#'
#' @template uri
#' @param text a character
did_save_text_document_params <- function(uri, text) {
  structure(
    list(
      textDocument   = uri,
      text           = text
    ),
    class = "did_save_text_document_params"
  )
}

#' parameters for didClose notifications
#'
#' @template uri
did_close_text_document_params <- function(uri) {
  structure(
    list(
      textDocument   = uri
    ),
    class = "did_close_text_document_params"
  )
}

#' parameters for workspace/didChangeConfiguration notifications
#'
#' @param settings a named list
did_change_configuration_params <- function(settings) {
  structure(
    settings,
    class = "did_change_configuration_params"
  )
}
