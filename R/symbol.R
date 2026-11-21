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

get_document_symbol_kind <- function(type) {
    if (is.character(type) && length(type) == 1) {
        switch(type,
            logical = SymbolKind$Boolean,
            integer = SymbolKind$Number,
            double = SymbolKind$Number,
            complex = SymbolKind$Number,
            character = SymbolKind$String,
            array = SymbolKind$Array,
            list = SymbolKind$Struct,
            `function` = SymbolKind$Function,
            `NULL` = SymbolKind$Null,
            `class` = SymbolKind$Class,
            R6 = SymbolKind$Class,
            S4 = SymbolKind$Class,
            S7 = SymbolKind$Class,
            RefClass = SymbolKind$Class,
            SymbolKind$Field
        )
    } else {
        SymbolKind$Field
    }
}

#' Main util function to get document symbols
#' @noRd
get_document_symbols <- function(document, xdoc, outline = NULL) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections(document$content)
    } else {
        get_r_document_sections_and_blocks(
            content = document$content, xdoc = xdoc, symbol = TRUE, outline = outline
        )
    }
}

#' Make document symbols for the named functions within one definition
#' @noRd
nested_function_symbols <- function(document, syntax, rows) {
    names <- reference_symbol_names(syntax$text[rows])
    assignments <- syntax$parent_row[syntax$parent_row[rows]]
    lapply(seq_along(rows), function(i) {
        document_symbol(names[[i]], SymbolKind$Function,
            range = navigation_range(document, syntax, assignments[[i]]),
            selectionRange = navigation_range(document, syntax, rows[[i]]))
    })
}

#' Sort enclosing symbols before their descendants
#' @noRd
document_symbol_order <- function(symbols) {
    order(
        vapply(symbols, function(symbol) symbol$range$start$line, numeric(1L)),
        vapply(symbols, function(symbol) symbol$range$start$character, numeric(1L)),
        -vapply(symbols, function(symbol) symbol$range$end$line, numeric(1L)),
        -vapply(symbols, function(symbol) symbol$range$end$character, numeric(1L))
    )
}

#' Build the containment tree required by hierarchical DocumentSymbol clients
#' @noRd
document_symbol_tree <- function(symbols, normalize_children = TRUE) {
    if (normalize_children) {
        supplied <- which(vapply(symbols, function(symbol) length(symbol$children) > 0L, logical(1L)))
        for (i in supplied) {
            symbols[[i]]$children <- document_symbol_tree(symbols[[i]]$children)
        }
    }
    if (length(symbols) < 2L) return(symbols)
    symbols <- symbols[document_symbol_order(symbols)]
    starts <- lapply(symbols, function(symbol) symbol$range$start)
    ends <- lapply(symbols, function(symbol) symbol$range$end)
    parent <- integer(length(symbols))
    stack <- integer(length(symbols))
    depth <- 0L
    for (i in seq_along(symbols)) {
        while (depth > 0L) {
            enclosing <- stack[[depth]]
            contains_end <- ends[[i]]$line < ends[[enclosing]]$line ||
                (ends[[i]]$line == ends[[enclosing]]$line &&
                        ends[[i]]$character <= ends[[enclosing]]$character)
            same_range <- starts[[i]]$line == starts[[enclosing]]$line &&
                starts[[i]]$character == starts[[enclosing]]$character &&
                ends[[i]]$line == ends[[enclosing]]$line &&
                ends[[i]]$character == ends[[enclosing]]$character
            if (contains_end && !same_range) break
            depth <- depth - 1L
        }
        if (depth > 0L) parent[[i]] <- stack[[depth]]
        depth <- depth + 1L
        stack[[depth]] <- i
    }

    children <- list2env(split(seq_along(symbols), parent), parent = emptyenv(), hash = TRUE)
    # Build inferred descendants bottom-up, reusing normalized branches. Only
    # merge a sibling group again when supplied members meet new sections;
    # keeping that merge local preserves explicit class ownership.
    for (i in rev(seq_along(symbols))) {
        nested <- children[[as.character(i)]]
        if (length(nested)) {
            members <- c(symbols[[i]]$children, symbols[nested])
            symbols[[i]]$children <- if (length(symbols[[i]]$children)) {
                document_symbol_tree(members, normalize_children = FALSE)
            } else {
                members
            }
        }
    }
    symbols[children[["0"]]]
}

#' Get all the symbols in the document
#' @noRd
document_symbol_reply <- function(id, uri, workspace, document, capabilities) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
            (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(NULL)
    }

    cache_key <- if (isTRUE(capabilities$hierarchicalDocumentSymbolSupport)) {
        "hierarchical"
    } else {
        "flat"
    }
    succession <- lsp_settings$get("nline_to_break_succession")
    cached <- parse_data$document_symbols_cache[[cache_key]]
    if (!is.null(cached) && identical(cached$content, document$content) &&
            identical(cached$succession, succession)) {
        return(Response$new(id, result = cached$result))
    }

    defns <- workspace$get_definitions_for_uri(uri)
    classes <- parse_data$class_data$declarations
    # Retain the definition selected by the ordinary binding index after a reassignment.
    classes <- Filter(function(x) {
        !x$binding || (!is.null(defns[[x$name]]$range) && equal_range(defns[[x$name]]$range, x$range))
    }, classes)
    class_by_name <- list()
    for (cls in classes) if (cls$binding) class_by_name[[cls$name]] <- cls
    logger$info("document definitions found: ", length(defns))
    
    if (isTRUE(capabilities$hierarchicalDocumentSymbolSupport)) {
        # Use hierarchical DocumentSymbol format
        nested <- parse_data$outline_data$nested_functions[names(defns)]
        definition_symbols <- lapply(seq_along(defns), function(i) {
            def <- defns[[i]]
            name <- names(defns)[[i]]
            
            children <- NULL
            kind <- get_document_symbol_kind(def$type)
            selection <- def$range
            cls <- class_by_name[[name]]
            if (!is.null(cls)) {
                kind <- SymbolKind$Class
                selection <- cls$selectionRange
                children <- cls$children
            } else if (length(nested[[i]])) {
                kind <- SymbolKind$Function
                children <- nested_function_symbols(document, parse_data$syntax_data, nested[[i]])
            } else if (!is.null(def$type) && def$type %in% c("R6", "S4", "RefClass")) {
                tryCatch({
                    children <- class_member_symbols(document, parse_data, def)
                }, error = function(e) {
                    # Silently handle extraction errors
                })
            }
            
            document_symbol(
                name = name,
                kind = kind,
                range = def$range,
                selectionRange = selection,
                children = children
            )
        })
        
        sections <- get_document_symbols(
            document,
            xdoc = if (is.null(parse_data$outline_data)) parse_data_xml(parse_data) else NULL,
            outline = parse_data$outline_data
        )
        section_symbols <- lapply(sections, function(section) {
            document_symbol(
                name = section$name,
                kind = switch(section$type,
                    section = SymbolKind$String,
                    chunk = SymbolKind$Key,
                    SymbolKind$String
                ),
                range = range(
                    start = document$to_lsp_position(
                        row = section$start_line - 1,
                        col = 0
                    ),
                    end = document$to_lsp_position(
                        row = section$end_line - 1,
                        col = nchar(document$line(section$end_line))
                    )
                ),
                selectionRange = range(
                    start = document$to_lsp_position(
                        row = section$start_line - 1,
                        col = 0
                    ),
                    end = document$to_lsp_position(
                        row = section$start_line - 1,
                        col = nchar(document$line(section$start_line))
                    )
                )
            )
        })
        
        standalone <- lapply(Filter(function(x) !x$binding, classes), function(x) {
            document_symbol(x$name, x$kind, x$range, x$selectionRange, children = x$children)
        })
        result <- document_symbol_tree(c(definition_symbols, standalone, section_symbols))
    } else {
        # Use flat SymbolInformation format for backward compatibility
        definition_symbols <- lapply(seq_along(defns), function(i) {
            def <- defns[[i]]
            name <- names(defns)[[i]]
            symbol_information(
                name = name,
                kind = if (is.null(class_by_name[[name]])) get_document_symbol_kind(def$type) else SymbolKind$Class,
                location = location(
                    uri = uri,
                    range = def$range
                )
            )
        })
        
        sections <- get_document_symbols(
            document,
            xdoc = if (is.null(parse_data$outline_data)) parse_data_xml(parse_data) else NULL,
            outline = parse_data$outline_data
        )
        section_symbols <- lapply(sections, function(section) {
            symbol_information(
                name = section$name,
                kind = switch(section$type,
                    section = SymbolKind$String,
                    chunk = SymbolKind$Key,
                    SymbolKind$String
                ),
                location = list(
                    uri = uri,
                    range = range(
                        start = document$to_lsp_position(
                            row = section$start_line - 1,
                            col = 0
                        ),
                        end = document$to_lsp_position(
                            row = section$end_line - 1,
                            col = nchar(document$line(section$end_line))
                        )
                    )
                )
            )
        })
        
        standalone <- lapply(Filter(function(x) !x$binding, classes), function(x) {
            symbol_information(x$name, x$kind, location(uri, x$selectionRange))
        })
        members <- unlist(lapply(classes, class_member_information, uri = uri), recursive = FALSE)
        result <- c(definition_symbols, standalone, members, section_symbols)
    }

    # Parse data is replaced after an edit. Keep the immutable result with
    # that parse so repeated outline requests only construct the response.
    parse_data$document_symbols_cache[[cache_key]] <- list(
        content = document$content,
        succession = succession,
        result = result
    )
    Response$new(id, result = result)
}

#' Get all the symbols in the workspace matching a query
#' @noRd
workspace_symbol_reply <- function(id, workspaces, query) {
    defns <- list()
    for (workspace in workspaces) {
        defns <- c(defns, workspace$get_definitions_for_query(query))
    }
    logger$info("workspace symbols found: ", length(defns))
    result <- lapply(defns, function(def) {
        item <- symbol_information(
            name = def$name,
            kind = if (is.null(def$kind)) get_document_symbol_kind(def$type) else def$kind,
            location = location(
                uri = def$uri,
                range = def$range
            )
        )
        item$containerName <- def$containerName
        item
    })

    Response$new(id, result = result)
}

class_member_information <- function(cls, uri) {
    lapply(cls$children, function(child) {
        item <- symbol_information(child$name, child$kind, location(uri, child$selectionRange))
        item$containerName <- cls$name
        item
    })
}

# Symbol search may return several members with the same spelling. Keep the
# search records separate from the lexical definition table used by navigation.
class_search_definitions <- function(definitions, classes) {
    if (!length(classes)) return(definitions)
    bindings <- match(vapply(classes, `[[`, character(1L), "name"), names(definitions))
    extra <- vector("list", length(classes))
    for (i in seq_along(classes)) {
        cls <- classes[[i]]
        own <- list()
        if (cls$binding) {
            row <- bindings[[i]]
            if (is.na(row)) next
            previous <- definitions[[row]]
            if (is.null(previous$range) || !equal_range(previous$range, cls$range)) next
            definitions[[row]]$kind <- cls$kind
        } else {
            own[[cls$name]] <- list(name = cls$name, kind = cls$kind, range = cls$selectionRange)
        }
        children <- lapply(cls$children, function(child) {
            list(name = child$name, kind = child$kind, containerName = cls$name, range = child$selectionRange)
        })
        names(children) <- vapply(cls$children, `[[`, character(1L), "name")
        extra[[i]] <- c(own, children)
    }
    c(definitions, unlist(extra, recursive = FALSE, use.names = TRUE))
}
