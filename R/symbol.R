# as defined by the language server protocol
SymbolKind <- list(
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26
)

get_r_document_section_symbols <- function(uri, document) {
    section_symbols <- NULL
    section_lines <- seq_len(document$nline)
    section_lines <- section_lines[
        grep("^\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            document$content[section_lines], perl = TRUE)]
    logger$info("document sections found: ", length(section_lines))
    if (length(section_lines)) {
        section_names <- gsub("^\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            "\\1", document$content[section_lines], perl = TRUE)
        section_end_lines <- c(section_lines[-1] - 1, document$nline)
        section_symbols <- .mapply(function(name, start_line, end_line) {
            symbol_information(
                name = name,
                kind = SymbolKind$String,
                location = list(
                    uri = uri,
                    range = range(
                        start = document$to_lsp_position(row = start_line - 1, col = 0),
                        end = document$to_lsp_position(row = end_line - 1, col = nchar(document$line(end_line)))
                    )
                )
            )
        }, list(section_names, section_lines, section_end_lines), NULL)
    }

    subsection_symbols <- NULL
    subsection_lines <- seq_len(document$nline)
    subsection_lines <- subsection_lines[
        grep("^\\s+\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            document$content[subsection_lines], perl = TRUE)]
    logger$info("document subsections found: ", length(subsection_lines))
    if (length(subsection_lines)) {
        subsection_names <- gsub("^\\s+\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
            "\\1", document$content[subsection_lines], perl = TRUE)
        subsection_symbols <- .mapply(function(name, line) {
            symbol_information(
                name = name,
                kind = SymbolKind$String,
                location = list(
                    uri = uri,
                    range = range(
                        start = document$to_lsp_position(row = line - 1, col = 0),
                        end = document$to_lsp_position(row = line - 1, col = nchar(document$line(line)))
                    )
                )
            )
        }, list(subsection_names, subsection_lines), NULL)
    }

    c(section_symbols, subsection_symbols)
}

get_rmd_document_section_symbols <- function(uri, document) {
    content <- document$content
    if (length(content) == 0) {
        return(NULL)
    }

    block_lines <- grep("^\\s*```", content)
    if (length(block_lines) %% 2 != 0) {
        return(NULL)
    }

    if (grepl("^---\\s*$", content[[1]])) {
        front_start <- 1L
        front_end <- 2L
        while (front_end <= document$nline) {
            if (grepl("^---\\s*$", content[[front_end]])) {
                block_lines <- c(front_start, front_end, block_lines)
                break
            }
            front_end <- front_end + 1L
        }
    }

    section_lines <- grepl("^#+\\s+\\S+", content)
    for (i in seq_len(length(block_lines) / 2)) {
        section_lines[seq.int(block_lines[[2 * i - 1]], block_lines[[2 * i]])] <- FALSE
    }

    section_lines <- which(section_lines)
    section_num <- length(section_lines)
    section_texts <- content[section_lines]
    section_hashes <- gsub("^(#+)\\s+.+$", "\\1", section_texts)
    section_levels <- nchar(section_hashes)
    section_names <- gsub("^#+\\s+(.+?)(\\s+#+)?\\s*$", "\\1", section_texts, perl = TRUE)

    section_symbols <- lapply(seq_len(section_num), function(i) {
        start_line <- section_lines[[i]]
        end_line <- document$nline
        level <- section_levels[[i]]
        j <- i + 1
        while (j <= section_num) {
            if (section_levels[[j]] <= level) {
                end_line <- section_lines[[j]] - 1
                break
            }
            j <- j + 1
        }
        symbol_information(
            name = section_names[[i]],
            kind = SymbolKind$String,
            location = list(
                uri = uri,
                range = range(
                    start = document$to_lsp_position(row = start_line - 1, col = 0),
                    end = document$to_lsp_position(row = end_line - 1, col = nchar(document$line(end_line)))
                )
            )
        )
    })

    unnamed_chunks <- 0
    chunk_symbols <- lapply(seq_len(length(block_lines) / 2), function(i) {
        start_line <- block_lines[[2 * i - 1]]
        end_line <- block_lines[[2 * i]]
        name <- NULL

        args_text <- stringi::stri_match_first_regex(document$line(start_line),
            "^\\s*```+\\s*\\{([a-zA-Z0-9_]+( *[ ,].*)?)\\}\\s*$")[1, 3]
        if (!is.na(args_text)) {
            name <- args_text
        }

        if (is.null(name)) {
            unnamed_chunks <<- unnamed_chunks + 1
            name <- sprintf("unnamed-chunk-%d", unnamed_chunks)
        }

        symbol_information(
            name = name,
            kind = SymbolKind$String,
            location = list(
                uri = uri,
                range = range(
                    start = document$to_lsp_position(row = start_line - 1, col = 0),
                    end = document$to_lsp_position(row = end_line - 1, col = nchar(document$line(end_line)))
                )
            )
        )
    })

    c(section_symbols, chunk_symbols)
}

get_document_section_symbols <- function(uri, document) {
    if (document$is_rmarkdown) {
        get_rmd_document_section_symbols(uri, document)
    } else {
        get_r_document_section_symbols(uri, document)
    }
}

#' Get all the symbols in the document
#' @keywords internal
document_symbol_reply <- function(id, uri, workspace, document, capabilities) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    defns <- workspace$get_definitions_for_uri(uri)
    logger$info("document definitions found: ", length(defns))
    definition_symbols <- lapply(names(defns),
        function(funct) {
            symbol_information(
                name = funct,
                kind = SymbolKind$Function,
                location = location(
                    uri = uri,
                    range = defns[[funct]]
                )
            )
    })
    result <- definition_symbols

    if (isTRUE(capabilities$hierarchicalDocumentSymbolSupport)) {
        section_symbols <- get_document_section_symbols(uri, document)
        result <- c(result, section_symbols)
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

#' Get all the symbols in the workspace matching a query
#' @keywords internal
workspace_symbol_reply <- function(id, workspace, query) {
    defns <- workspace$get_definitions_for_query(query)
    logger$info("workspace symbols found: ", length(defns))
    result <- lapply(names(defns),
        function(funct) {
            symbol_information(
                name = funct,
                kind = SymbolKind[["Function"]],
                location = defns[[funct]]
            )
    })
    if (is.null(result)) {
        Response$new(id)
    } else {
        Response$new(
            id,
            result = result
        )
    }
}
