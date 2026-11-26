roxygen_bracket_labels <- c("any", "atomic", "listish", "expressions")

# A marker is split into nominal types before any of its candidates are used.
# Keep source offsets in code points, including whitespace in a wrapped span.
roxygen_type_atom <- function(text, class_phrase) {
    markdown <- FALSE
    if (startsWith(text, "(`")) {
        open <- "(`"
        close <- "`)"
    } else if (startsWith(text, "[`")) {
        open <- "[`"
        close <- "`]"
        markdown <- TRUE
    } else if (startsWith(text, "\\[`")) {
        open <- "\\[`"
        close <- "`]"
    } else if (startsWith(text, "[\\code{")) {
        open <- "[\\code{"
        close <- "}]"
    } else if (class_phrase && startsWith(text, "`")) {
        open <- close <- "`"
    } else if (class_phrase && startsWith(text, "\\code{")) {
        open <- "\\code{"
        close <- "}"
    } else if (class_phrase && startsWith(text, "[")) {
        open <- "["
        close <- "]"
        markdown <- TRUE
    } else {
        return(NULL)
    }
    prefix <- nchar(open)
    body <- substring(text, prefix + 1L)
    close_at <- regexpr(close, body, fixed = TRUE)[[1L]]
    if (close_at < 1L) return(NULL)
    span <- substr(body, 1L, close_at - 1L)
    if (!nzchar(trimws(span)) || endsWith(trimws(span), "|")) return(NULL)
    terms <- strsplit(span, "|", fixed = TRUE)[[1L]]
    types <- trimws(terms)
    if (length(unique(types)) > max_type_hint_alternatives || !all(grepl(roxygen_nominal_type, types, perl = TRUE))) return(NULL)
    term_start <- c(0L, cumsum(nchar(terms) + 1L))[-(length(terms) + 1L)]
    terminal <- sub("^.*::", "", types)
    positions <- prefix + term_start + nchar(terms) - nchar(trimws(terms, "left")) + nchar(types) - nchar(terminal)
    # Several packages use these words for input constraints in square
    # brackets. A bare storage constraint cannot establish an S3 class.
    bracket <- startsWith(open, "[") || startsWith(open, "\\[")
    verify <- markdown | (bracket & types %in% roxygen_bracket_labels)
    list(types = types, starts = positions, verify = verify, markdown = markdown, width = prefix + close_at - 1L + nchar(close))
}

roxygen_leading_types <- function(text) {
    pos <- 0L
    types <- character()
    starts <- integer()
    links <- logical()
    class_phrase <- FALSE
    # Limit a sequence of separate markers as well as the number of types in
    # its union. A long repeated or malformed alternative cannot run unbounded.
    for (i in seq_len(max_type_hint_alternatives)) {
        tail <- substring(text, pos + 1L)
        object <- FALSE
        if (substr(tail, 1L, 1L) %in% c("A", "a", "T", "t")) {
            phrase <- regexpr(r"(^(?i:an\s+object\s+of\s+class)\s+)", tail, perl = TRUE)
            if (phrase[[1L]] == 1L) {
                class_phrase <- TRUE
            } else {
                phrase <- regexpr(r"(^(?i:an?|the)\s+(?=\[`))", tail, perl = TRUE)
                if (phrase[[1L]] != 1L) return(NULL)
                object <- TRUE
            }
            width <- attr(phrase, "match.length")[[1L]]
            pos <- pos + width
            tail <- substring(tail, width + 1L)
        }
        atom <- roxygen_type_atom(tail, class_phrase)
        if (is.null(atom)) return(NULL)
        types <- c(types, atom$types)
        starts <- c(starts, pos + atom$starts)
        links <- c(links, atom$verify)
        pos <- pos + atom$width
        tail <- substring(text, pos + 1L)
        if (object) {
            suffix <- regexpr(r"(^\s+(?i:object)\b)", tail, perl = TRUE)
            if (suffix[[1L]] != 1L) return(NULL)
            width <- attr(suffix, "match.length")[[1L]]
            pos <- pos + width
            tail <- substring(tail, width + 1L)
        }
        # An ordinary description word ends the declaration immediately. The
        # remaining cases can be another atom, punctuation or a link target.
        if (nzchar(tail) && !grepl(r"(^\s+(?!(?i:or|and)\b)[[:alnum:]_])", tail, perl = TRUE)) {
            if (!object && atom$markdown && grepl(r"(^\s*(?:\(|:\s*(?:<|https?://|mailto:)))", tail, perl = TRUE)) return(NULL)
            separator <- roxygen_type_separator(tail)
            if (separator) {
                pos <- pos + separator
                next
            }
            if (!roxygen_complete_type_prefix(tail)) return(NULL)
        }
        unique_types <- unique(types)
        if (length(unique_types) > max_type_hint_alternatives) return(NULL)
        evidence <- setdiff(unique(types[links]), types[!links])
        if (length(evidence)) attr(unique_types, "roxygen_class_links") <- evidence
        return(list(types = types, unique = unique_types, starts = starts))
    }
    NULL
}

# Return the consumed separator width. A comma starts another alternative only
# when followed by a marked atom, an explicit list continuation or the end of
# an unfinished list. A comma introducing ordinary prose terminates the prefix.
roxygen_type_separator <- function(text) {
    separator <- regexpr(r"(^(?:\s*\|\s*|\s+(?i:or)\b\s*))", text, perl = TRUE)
    if (separator[[1L]] == 1L) return(attr(separator, "match.length")[[1L]])
    comma <- regexpr(r"(^\s*,\s*)", text, perl = TRUE)
    if (comma[[1L]] != 1L) return(0L)
    width <- attr(comma, "match.length")[[1L]]
    next_atom <- substring(text, width + 1L)
    conjunction <- regexpr(r"(^(?i:or)\b\s*)", next_atom, perl = TRUE)
    if (conjunction[[1L]] == 1L) return(width + attr(conjunction, "match.length")[[1L]])
    marked <- r"(^(?:\\?\[|\(`|`|\\code\{|\||/|(?i:an\s+object\s+of\s+class\b|(?:an?|the)\s+\[`|and\b)))"
    unmarked_list <- r"((*UCP)^[[:alnum:]_.:]+\s*(?:\||,|(?i:\bor\b)))"
    if (!nzchar(next_atom) || grepl(marked, next_atom, perl = TRUE) || grepl(unmarked_list, next_atom, perl = TRUE)) width else 0L
}

roxygen_complete_type_prefix <- function(tail) {
    if (!nzchar(tail)) return(TRUE)
    if (!grepl(r"(^(?:\s|[.,:;!?](?=\s|$)|\\cr(?=\s|$)))", tail, perl = TRUE)) return(FALSE)
    remaining <- trimws(sub(r"(^\\cr\b)", "", tail, perl = TRUE), "left")
    if (startsWith(remaining, ",") || startsWith(remaining, ";")) remaining <- trimws(substring(remaining, 2L), "left")
    !grepl(r"(^(?:\||/|(?i:or|and)\b|\\?\[|\((?=`)))", remaining, perl = TRUE)
}
