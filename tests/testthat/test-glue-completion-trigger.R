test_that("a typed glue opener starts a completion list that refines on the next keystroke", {
    skip_on_cran()
    directory <- withr::local_tempdir()
    file <- file.path(directory, "template.R")
    prefix <- paste0('glue::glue("', intToUtf8(0x1f98a), " Total ")
    lines <- c("local_total <- 41", paste0(prefix, '")'), 'plain <- "{}"',
        "f <- function(value) {}", 'glue::glue("{{literal}}")')
    writeLines(lines, file, useBytes = TRUE)
    client <- language_client(working_dir = directory)
    uri <- path_to_uri(file)
    expect_true("{" %in% unlist(client$ServerCapabilities$completionProvider$triggerCharacters))
    did_open(client, file)

    completion <- function(line, col, context) {
        respond(client, "textDocument/completion", list(textDocument = list(uri = uri),
                position = list(line = line, character = col), context = context), retry = FALSE)
    }
    insert <- function(version, col, text) {
        at <- list(line = 1L, character = col)
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = list(list(range = list(start = at, end = at), text = text))))
    }
    # VS Code inserts both braces and leaves the caret between them.
    col <- code_point_to_unit(prefix, nchar(prefix))
    insert(2L, col, "{}")
    brace <- list(triggerKind = 2L, triggerCharacter = "{")
    opened <- completion(1L, col + 1L, brace)
    expect_true(opened$isIncomplete)
    labels <- vapply(opened$items, `[[`, "", "label")
    expect_true("local_total" %in% labels)
    edit <- opened$items[[match("local_total", labels)]]$textEdit
    start <- list(line = 1L, character = col + 1L)
    expect_equal(edit$range, list(start = start, end = start))

    insert(3L, col + 1L, "local_t")
    end <- list(line = 1L, character = col + 1L + nchar("local_t"))
    refined <- completion(1L, end$character, list(triggerKind = 3L))
    labels <- vapply(refined$items, `[[`, "", "label")
    expect_true("local_total" %in% labels)
    expect_false("local_t" %in% labels)
    expect_equal(refined$items[[match("local_total", labels)]]$textEdit$range, list(start = start, end = end))
    insert(4L, end$character, "otal")
    complete <- completion(1L, end$character + 4L, list(triggerKind = 3L))
    expect_true("local_total" %in% vapply(complete$items, `[[`, "", "label"))

    for (line in 2L:4L) {
        last_brace <- utils::tail(gregexpr("{", lines[[line + 1L]], fixed = TRUE)[[1L]], 1L)
        expect_length(completion(line, last_brace, brace)$items, 0L)
    }
})
