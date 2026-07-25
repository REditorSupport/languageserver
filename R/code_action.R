CodeActionKind <- list(
    Empty = "",
    QuickFix = "quickfix",
    Refactor = "refactor",
    RefactorExtract = "refactor.extract",
    RefactorInline = "refactor.inline",
    RefactorRewrite = "refactor.rewrite",
    Source = "source",
    SourceOrganizeImports = "source.organizeImports",
    SourceFixAll = "source.fixAll"
)

#' Create a workspace edit for one document
#' @noRd
code_action_workspace_edit <- function(uri, edits) {
    changes <- list(edits)
    names(changes) <- uri
    list(changes = changes)
}

#' Test whether a code action kind was requested by the client
#' @noRd
code_action_kind_requested <- function(kind, only) {
    if (is.null(only) || !length(only)) return(TRUE)
    any(vapply(only, function(parent) {
        identical(kind, parent) || startsWith(kind, paste0(parent, "."))
    }, logical(1L)))
}

#' Convert a diagnostic range to code-point offsets
#' @noRd
code_action_diagnostic_range <- function(document, diagnostic) {
    list(
        start = document$from_lsp_position(diagnostic$range$start),
        end = document$from_lsp_position(diagnostic$range$end)
    )
}

#' Create an edit from code-point offsets
#' @noRd
code_action_text_edit <- function(document, row, start, end, new_text) {
    line_length <- nchar(document$line0(row))
    start <- max(0L, min(start, line_length))
    end <- max(start, min(end, line_length))
    text_edit(
        range = range(
            start = document$to_lsp_position(row, start),
            end = document$to_lsp_position(row, end)
        ),
        new_text = new_text
    )
}

#' Return a character at a zero-based offset
#' @noRd
code_action_character <- function(text, offset) {
    if (offset < 0L || offset >= nchar(text)) return("")
    substr(text, offset + 1L, offset + 1L)
}

#' Expand a range over adjacent horizontal whitespace
#' @noRd
code_action_expand_whitespace <- function(text, start, end,
    before = TRUE, after = TRUE) {
    if (before) {
        while (start > 0L && grepl("[ \t]", code_action_character(text, start - 1L))) {
            start <- start - 1L
        }
    }
    if (after) {
        while (end < nchar(text) && grepl("[ \t]", code_action_character(text, end))) {
            end <- end + 1L
        }
    }
    c(start, end)
}

#' Add spaces around a token only where the line has adjacent code
#' @noRd
code_action_spaced_token <- function(text, bounds, token) {
    paste0(
        if (bounds[[1L]] > 0L) " " else "",
        token,
        if (bounds[[2L]] < nchar(text)) " " else ""
    )
}

#' Find the nearest occurrence of a character to a diagnostic range
#' @noRd
code_action_nearest_character <- function(text, character, start, end) {
    locations <- gregexpr(character, text, fixed = TRUE)[[1L]]
    if (locations[[1L]] == -1L) return(NULL)
    locations <- locations - 1L
    distance <- ifelse(
        locations < start,
        start - locations,
        ifelse(locations >= end, locations - end + 1L, 0L)
    )
    locations[[which.min(distance)]]
}

#' Extract text covered by an edit
#' @noRd
code_action_edit_text <- function(document, edit) {
    start <- document$from_lsp_position(edit$range$start)
    end <- document$from_lsp_position(edit$range$end)
    if (start$row == end$row) {
        return(substr(document$line0(start$row), start$col + 1L, end$col))
    }
    lines <- document$content[(start$row:end$row) + 1L]
    lines[[1L]] <- substr(lines[[1L]], start$col + 1L, nchar(lines[[1L]]))
    lines[[length(lines)]] <- substr(lines[[length(lines)]], 1L, end$col)
    paste(lines, collapse = "\n")
}

#' Create a direct fix for a lintr diagnostic
#' @noRd
code_action_direct_fix <- function(document, diagnostic) {
    code <- if (is.null(diagnostic$code)) "" else as.character(diagnostic$code)
    message <- if (is.null(diagnostic$message)) "" else diagnostic$message
    item_range <- code_action_diagnostic_range(document, diagnostic)
    row <- item_range$start$row
    if (row < 0L || row >= document$nline) return(NULL)

    line <- document$line0(row)
    start <- max(0L, min(item_range$start$col, nchar(line)))
    end <- max(start, min(item_range$end$col, nchar(line)))
    priority <- 10L

    if (identical(code, "assignment_linter")) {
        token <- substr(line, start + 1L, end)
        if (!identical(token, "=")) return(NULL)
        bounds <- code_action_expand_whitespace(line, start, end)
        edit <- code_action_text_edit(document, row, bounds[[1L]], bounds[[2L]], " <- ")
        title <- "Replace `=` with `<-`"
        priority <- 100L
    } else if (identical(code, "infix_spaces_linter")) {
        token <- trimws(substr(line, start + 1L, end))
        if (!nzchar(token)) return(NULL)
        bounds <- code_action_expand_whitespace(line, start, end)
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]],
            code_action_spaced_token(line, bounds, token))
        title <- sprintf("Add spaces around `%s`", token)
        priority <- 20L
    } else if (identical(code, "commas_linter")) {
        comma <- code_action_nearest_character(line, ",", start, end)
        if (is.null(comma)) return(NULL)
        bounds <- code_action_expand_whitespace(line, comma, comma + 1L)
        replacement <- paste0(",", if (bounds[[2L]] < nchar(line)) " " else "")
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]], replacement)
        title <- "Normalize spacing around `,`"
        priority <- 30L
    } else if (identical(code, "indentation_linter")) {
        expected <- regmatches(message, regexec(
            "Indentation should be ([0-9]+) spaces", message))[[1L]]
        if (length(expected) != 2L) return(NULL)
        leading <- attr(regexpr("^[ \t]*", line), "match.length")
        edit <- code_action_text_edit(
            document, row, 0L, leading, strrep(" ", as.integer(expected[[2L]])))
        title <- "Fix indentation"
        priority <- 40L
    } else if (identical(code, "pipe_consistency_linter")) {
        desired <- regmatches(message, regexec(
            "^Use the (.+) pipe operator instead of", message))[[1L]]
        if (length(desired) != 2L) return(NULL)
        bounds <- code_action_expand_whitespace(line, start, end)
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]],
            code_action_spaced_token(line, bounds, desired[[2L]]))
        title <- sprintf("Replace pipe with `%s`", desired[[2L]])
        priority <- 80L
    } else if (identical(code, "T_and_F_symbol_linter")) {
        token <- code_action_character(line, start)
        replacement <- switch(token, T = "TRUE", F = "FALSE", NULL)
        if (is.null(replacement)) return(NULL)
        edit <- code_action_text_edit(document, row, start, start + 1L, replacement)
        title <- sprintf("Replace `%s` with `%s`", token, replacement)
        priority <- 80L
    } else if (identical(code, "trailing_whitespace_linter")) {
        trailing <- regexpr("[ \t]+$", line)
        if (trailing[[1L]] < 0L) return(NULL)
        edit <- code_action_text_edit(
            document, row, trailing[[1L]] - 1L, nchar(line), "")
        title <- "Remove trailing whitespace"
        priority <- 70L
    } else if (identical(code, "trailing_blank_lines_linter")) {
        if (any(nzchar(document$content[(row + 1L):document$nline]))) return(NULL)
        edit <- text_edit(
            range = range(
                start = document$to_lsp_position(row, 0L),
                end = document$to_lsp_position(
                    document$nline - 1L, nchar(document$line(document$nline)))
            ),
            new_text = ""
        )
        title <- "Remove trailing blank lines"
        priority <- 70L
    } else if (identical(code, "semicolon_linter")) {
        semicolon <- code_action_nearest_character(line, ";", start, end)
        if (is.null(semicolon)) return(NULL)
        bounds <- code_action_expand_whitespace(line, semicolon, semicolon + 1L)
        indentation <- sub("^([ \t]*).*", "\\1", line)
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]], paste0("\n", indentation))
        title <- "Replace `;` with a newline"
        priority <- 60L
    } else if (identical(code, "spaces_left_parentheses_linter")) {
        parenthesis <- code_action_nearest_character(line, "(", start, end)
        if (is.null(parenthesis)) return(NULL)
        bounds <- code_action_expand_whitespace(
            line, parenthesis, parenthesis + 1L, after = FALSE)
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]], " (")
        title <- "Add space before `(`"
        priority <- 30L
    } else if ((identical(code, "brace_linter") &&
                grepl("space before an opening curly brace", message, fixed = TRUE)) ||
            identical(code, "paren_body_linter")) {
        brace <- code_action_nearest_character(line, "{", start, end)
        if (is.null(brace)) return(NULL)
        bounds <- code_action_expand_whitespace(
            line, brace, brace + 1L, after = FALSE)
        edit <- code_action_text_edit(
            document, row, bounds[[1L]], bounds[[2L]], " {")
        title <- "Add space before `{`"
        priority <- 30L
    } else if (identical(code, "equals_na_linter")) {
        source <- substr(line, start + 1L, end)
        expression <- tryCatch(parse(text = source, keep.source = FALSE)[[1L]],
            error = function(e) NULL)
        if (is.null(expression) || !is.call(expression) || length(expression) != 3L ||
                !as.character(expression[[1L]]) %in% c("==", "!=")) return(NULL)
        is_na <- function(x) {
            length(x) == 1L && is.atomic(x) && is.na(x)
        }
        if (is_na(expression[[2L]])) {
            operand <- expression[[3L]]
        } else if (is_na(expression[[3L]])) {
            operand <- expression[[2L]]
        } else {
            return(NULL)
        }
        operand <- paste(deparse(operand, width.cutoff = 500L), collapse = " ")
        replacement <- sprintf("is.na(%s)", operand)
        if (identical(as.character(expression[[1L]]), "!=")) {
            replacement <- paste0("!", replacement)
        }
        edit <- code_action_text_edit(document, row, start, end, replacement)
        title <- "Replace comparison with `is.na()`"
        priority <- 90L
    } else {
        return(NULL)
    }

    if (identical(code_action_edit_text(document, edit), edit$newText)) return(NULL)
    list(
        title = title,
        edit = edit,
        diagnostics = list(diagnostic),
        priority = priority
    )
}

#' Return a stable key for a text edit
#' @noRd
code_action_edit_key <- function(edit) {
    paste(
        edit$range$start$line, edit$range$start$character,
        edit$range$end$line, edit$range$end$character,
        edit$newText,
        sep = ":"
    )
}

#' Combine direct fixes that produce the same edit
#' @noRd
code_action_direct_fixes <- function(document, diagnostics) {
    fixes <- lapply(diagnostics, function(diagnostic) {
        if (!is.null(diagnostic$source) && !identical(diagnostic$source, "lintr")) {
            return(NULL)
        }
        code_action_direct_fix(document, diagnostic)
    })
    fixes <- Filter(Negate(is.null), fixes)
    if (!length(fixes)) return(list())

    result <- list()
    keys <- character()
    for (fix in fixes) {
        key <- code_action_edit_key(fix$edit)
        index <- match(key, keys)
        if (is.na(index)) {
            result[[length(result) + 1L]] <- fix
            keys <- c(keys, key)
        } else {
            result[[index]]$diagnostics <- c(
                result[[index]]$diagnostics, fix$diagnostics)
            if (fix$priority > result[[index]]$priority) {
                result[[index]]$title <- fix$title
                result[[index]]$priority <- fix$priority
            }
        }
    }
    result
}

#' Test whether two LSP text edit ranges overlap
#' @noRd
code_action_edits_overlap <- function(left, right) {
    compare_position(left$range$end, right$range$start) > 0L &&
        compare_position(right$range$end, left$range$start) > 0L
}

#' Choose a non-overlapping set of direct fixes
#' @noRd
code_action_non_overlapping_fixes <- function(fixes) {
    if (!length(fixes)) return(list())
    fixes <- fixes[order(
        -vapply(fixes, function(fix) fix$priority, integer(1L)),
        vapply(fixes, function(fix) fix$edit$range$start$line, numeric(1L)),
        vapply(fixes, function(fix) fix$edit$range$start$character, numeric(1L))
    )]
    selected <- list()
    for (fix in fixes) {
        overlaps <- vapply(selected, function(other) {
            code_action_edits_overlap(fix$edit, other$edit)
        }, logical(1L))
        if (!length(overlaps) || !any(overlaps)) {
            selected[[length(selected) + 1L]] <- fix
        }
    }
    selected[order(
        vapply(selected, function(fix) fix$edit$range$start$line, numeric(1L)),
        vapply(selected, function(fix) fix$edit$range$start$character, numeric(1L))
    )]
}

#' Create an edit that adds or broadens an inline nolint directive
#' @noRd
code_action_nolint_edit <- function(document, row, linter = NULL) {
    line <- document$line0(row)
    specific <- regexec("#\\s*nolint\\s*:\\s*([^.]*?)\\s*\\.", line, perl = TRUE)[[1L]]
    all <- regexpr("#\\s*nolint\\s*$", line, perl = TRUE)

    if (specific[[1L]] > 0L) {
        start <- specific[[1L]] - 1L
        match_length <- attr(specific, "match.length")[[1L]]
        if (is.null(linter)) {
            return(code_action_text_edit(
                document, row, start, start + match_length, "# nolint"))
        }
        codes <- substr(
            line,
            specific[[2L]],
            specific[[2L]] + attr(specific, "match.length")[[2L]] - 1L
        )
        codes <- trimws(strsplit(codes, ",", fixed = TRUE)[[1L]])
        if (linter %in% codes) return(NULL)
        dot <- start + match_length - 1L
        return(code_action_text_edit(
            document, row, dot, dot, sprintf(", %s", linter)))
    }
    if (all[[1L]] > 0L) return(NULL)

    trailing <- regexpr("[ \t]+$", line)
    content_end <- if (trailing[[1L]] > 0L) trailing[[1L]] - 1L else nchar(line)
    prefix <- if (content_end > 0L) " " else ""
    directive <- if (is.null(linter)) {
        "# nolint"
    } else {
        sprintf("# nolint: %s.", linter)
    }
    code_action_text_edit(
        document, row, content_end, nchar(line), paste0(prefix, directive))
}

#' Create grouped actions for suppressing lintr diagnostics
#' @noRd
code_action_suppression_actions <- function(uri, document, diagnostics) {
    diagnostics <- Filter(function(item) {
        (is.null(item$source) || identical(item$source, "lintr")) &&
            !is.null(item$code) && nzchar(as.character(item$code))
    }, diagnostics)
    if (!length(diagnostics)) return(list())

    rows <- vapply(diagnostics, function(item) {
        code_action_diagnostic_range(document, item)$end$row
    }, numeric(1L))
    valid <- rows >= 0L & rows < document$nline
    diagnostics <- diagnostics[valid]
    rows <- rows[valid]
    if (!length(diagnostics)) return(list())

    result <- list()
    unique_rows <- unique(rows)
    edits <- Filter(Negate(is.null), lapply(unique_rows, function(row) {
        code_action_nolint_edit(document, row)
    }))
    if (length(edits)) {
        title <- if (length(unique_rows) == 1L) {
            "Disable all linters for this line"
        } else {
            "Disable all linters for these lines"
        }
        result[[length(result) + 1L]] <- list(
            title = title,
            kind = CodeActionKind$QuickFix,
            diagnostics = diagnostics,
            edit = code_action_workspace_edit(uri, edits)
        )
    }

    codes <- unique(vapply(diagnostics, function(item) as.character(item$code), character(1L)))
    for (code in codes) {
        matching <- vapply(diagnostics, function(item) {
            identical(as.character(item$code), code)
        }, logical(1L))
        code_diagnostics <- diagnostics[matching]
        code_rows <- unique(rows[matching])
        edits <- Filter(Negate(is.null), lapply(code_rows, function(row) {
            code_action_nolint_edit(document, row, code)
        }))
        if (!length(edits)) next
        title <- if (length(code_rows) == 1L) {
            sprintf("Disable %s for this line", code)
        } else {
            sprintf("Disable %s for these lines", code)
        }
        result[[length(result) + 1L]] <- list(
            title = title,
            kind = CodeActionKind$QuickFix,
            diagnostics = code_diagnostics,
            edit = code_action_workspace_edit(uri, edits)
        )
    }
    result
}

#' The response to a textDocument/codeAction Request
#'
#' @keywords internal
document_code_action_reply <- function(id, uri, workspace, document, range, context) {
    diagnostics <- context$diagnostics
    if (is.null(diagnostics)) diagnostics <- list()
    only <- context$only

    direct_fixes <- code_action_direct_fixes(document, diagnostics)
    result <- lapply(direct_fixes, function(fix) {
        list(
            title = fix$title,
            kind = CodeActionKind$QuickFix,
            diagnostics = fix$diagnostics,
            isPreferred = TRUE,
            edit = code_action_workspace_edit(uri, list(fix$edit))
        )
    })
    result <- c(result, code_action_suppression_actions(uri, document, diagnostics))

    if (code_action_kind_requested(CodeActionKind$SourceFixAll, only) &&
            length(only) && length(direct_fixes)) {
        selected <- code_action_non_overlapping_fixes(direct_fixes)
        result[[length(result) + 1L]] <- list(
            title = "Fix all auto-fixable lintr problems",
            kind = CodeActionKind$SourceFixAll,
            diagnostics = unlist(lapply(selected, function(fix) fix$diagnostics),
                recursive = FALSE),
            isPreferred = TRUE,
            edit = code_action_workspace_edit(
                uri, lapply(selected, function(fix) fix$edit))
        )
    }

    result <- Filter(function(action) {
        code_action_kind_requested(action$kind, only)
    }, result)

    logger$info("document_code_action_reply: ", list(
        uri = uri,
        range = range,
        context = context,
        result = result
    ))

    Response$new(id, result = result)
}
