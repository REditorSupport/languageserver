# Decode the wire representation independently of the provider's source map.
glue_lsp_tokens <- function(result, legend) {
    raw <- matrix(as.integer(result$data), ncol = 5L, byrow = TRUE)
    rows <- cumsum(raw[, 1L])
    starts <- raw[, 2L]
    for (idx in split(seq_along(rows), rows)) starts[idx] <- cumsum(starts[idx])
    data.frame(line = rows, start = starts, end = starts + raw[, 3L],
        type = unlist(legend$tokenTypes, use.names = FALSE)[raw[, 4L] + 1L], modifiers = raw[, 5L])
}

glue_lsp_token_at <- function(tokens, lines, line, text) {
    offset <- regexpr(text, lines[[line + 1L]], fixed = TRUE)[[1L]] - 1L
    stopifnot(offset >= 0L)
    at <- code_point_to_unit(lines[[line + 1L]], offset)
    tokens[tokens$line == line & tokens$start <= at & at < tokens$end, , drop = FALSE]
}

test_that("semantic tokens follow glue configuration, edits and UTF-16 positions over LSP", {
    skip_on_cran()
    directory <- withr::local_tempdir()
    path <- file.path(directory, "interpolation.qmd")
    expression <- "sum(value, na.rm = TRUE) + 1L"
    lines <- c("Prose {value}", "```{r}", expression,
        paste0('glue::glue("', intToUtf8(0x1f98a), " total {", expression, '} done {{literal}}")'),
        'f <- function() { glue <- function(...) NULL; glue("shadow {value}") }',
        'custom("{skip}", R"--[configured {value}]--")',
        'other::say(text = "label {value}", title = "{skip}")', "```")
    writeLines(lines, path, useBytes = TRUE)
    capabilities <- list(workspace = list(semanticTokens = list(refreshSupport = TRUE)),
        textDocument = list(semanticTokens = list(
            requests = list(full = list(delta = TRUE), range = TRUE), tokenTypes = names(SemanticTokenTypes),
            tokenModifiers = names(SemanticTokenModifiers), formats = list("relative"))))
    client <- language_client(working_dir = directory, capabilities = capabilities)
    refreshes <- 0L
    client$request_handlers[["workspace/semanticTokens/refresh"]] <- function(self, id, params) {
        refreshes <<- refreshes + 1L
        self$deliver(Response$new(id, NULL))
    }
    did_open(client, path)
    legend <- client$ServerCapabilities$semanticTokensProvider$legend
    full <- function() respond_semantic_tokens_full(client, path)
    get <- function(result, line, text) glue_lsp_token_at(glue_lsp_tokens(result, legend), lines, line, text)
    set <- function(enabled, functions = NULL) {
        settings <- list(glue_highlighting = enabled)
        if (!is.null(functions)) settings$glue_highlight_functions <- functions
        notify(client, "workspace/didChangeConfiguration", list(settings = list(r = list(lsp = settings))))
    }
    check_delta <- function(previous, current) {
        delta <- respond_semantic_tokens_delta(client, path, previous$resultId)
        data <- as.integer(previous$data)
        if (!is.null(delta$data)) {
            data <- as.integer(delta$data)
        } else {
            for (edit in rev(delta$edits)) {
                data <- c(head(data, edit$start), as.integer(edit$data), tail(data, length(data) - edit$start - edit$deleteCount))
            }
        }
        expect_identical(delta$resultId, current$resultId)
        expect_identical(data, as.integer(current$data))
    }

    initial <- full()
    tokens <- glue_lsp_tokens(initial, legend)
    expect_false(any(tokens$line == 0L))
    for (word in c("sum", "value", "na.rm", "TRUE", "+", "1L")) {
        plain <- get(initial, 2L, word)
        embedded <- get(initial, 3L, word)
        expect_equal(nrow(plain), 1L, info = word)
        expect_identical(embedded$type, plain$type, info = word)
        expect_identical(embedded$modifiers, plain$modifiers, info = word)
        expect_equal(embedded$end - embedded$start, nchar(word), info = word)
    }
    expect_identical(get(initial, 3L, "total")$type, "string")
    expect_identical(get(initial, 3L, "literal")$type, "string")
    expect_identical(get(initial, 4L, "value")$type, "string")
    expect_identical(get(initial, 5L, "value")$type, "string")
    expect_identical(get(initial, 6L, "value")$type, "string")
    viewport <- respond_semantic_tokens_range(client, path, c(3L, 0L), c(4L, 0L))
    expected <- tokens[tokens$line == 3L, , drop = FALSE]
    rownames(expected) <- NULL
    expect_identical(glue_lsp_tokens(viewport, legend), expected)

    set(FALSE)
    disabled <- full()
    expect_equal(refreshes, 1L)
    expect_false(identical(initial$resultId, disabled$resultId))
    for (word in c("total", "sum", "value", "literal")) expect_identical(get(disabled, 3L, word)$type, "string")
    check_delta(initial, disabled)

    set(TRUE, list(custom = 2L, `other::say` = "text"))
    enabled <- full()
    expect_equal(refreshes, 2L)
    expect_false(identical(disabled$resultId, enabled$resultId))
    expect_identical(get(enabled, 3L, "sum")$type, "function")
    for (line in c(5L, 6L)) {
        expect_identical(get(enabled, line, "value")$type, "variable")
        expect_identical(get(enabled, line, "skip")$type, "string")
    }
    check_delta(disabled, enabled)

    at <- get(enabled, 3L, "value")$start
    notify(client, "textDocument/didChange", list(textDocument = list(uri = path_to_uri(path), version = 2L),
            contentChanges = list(list(range = range(position(3L, at), position(3L, at + 5L)), text = "42L"))))
    lines[[4L]] <- sub("value", "42L", lines[[4L]], fixed = TRUE)
    edited <- full()
    expect_identical(get(edited, 3L, "42L")$type, "number")
    expect_identical(get(edited, 3L, "42L")$start, at)
    check_delta(enabled, edited)

    # The boolean and document version stay the same. Replacing the rule map
    # alone must invalidate both custom colors and a disabled builtin.
    set(TRUE, list(`glue::glue` = FALSE))
    removed <- full()
    expect_equal(refreshes, 3L)
    expect_false(identical(edited$resultId, removed$resultId))
    expect_identical(get(removed, 3L, "42L")$type, "string")
    for (line in c(5L, 6L)) expect_identical(get(removed, line, "value")$type, "string")
    check_delta(edited, removed)
})
