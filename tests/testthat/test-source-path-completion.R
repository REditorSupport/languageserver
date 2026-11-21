source_path_fixture <- function(lines, workspace, uri = path_to_uri(file.path(workspace$root, "R", "main.R")),
    language = "r") {
    marked <- which(grepl("|", lines, fixed = TRUE))
    stopifnot(length(marked) == 1L)
    col <- regexpr("|", lines[[marked]], fixed = TRUE)[[1L]] - 1L
    lines[[marked]] <- sub("|", "", lines[[marked]], fixed = TRUE)
    doc <- Document$new(uri, version = 1L, language = language, content = lines)
    p <- parse_document(uri, lines, is_rmarkdown = doc$is_rmarkdown)
    p$version <- 1L
    doc$update_parse_data(p)
    point <- list(row = marked - 1L, col = col)
    list(doc = doc, point = point)
}

test_that("source path edits preserve directory spelling and quote boundaries", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    dir.create(file.path(root, "sub"))
    dir.create(file.path(root, "R", "sub"))
    file.create(file.path(root, "sub", c("help.R", "h'elp.R")))
    file.create(file.path(root, "R", "sub", c("help.R", "hello.R")))
    dir.create(file.path(root, "sub", "helpers"))
    workspace <- list(root = root)
    complete <- function(lines) {
        f <- source_path_fixture(lines, workspace)
        completion_reply(1L, f$doc$uri, workspace, f$doc, f$point, list())$result
    }
    items <- complete('"🐈"; source("sub/he|BAD")')$items
    expect_equal(map_chr(items, "label"), c("hello.R", "help.R", "helpers/"))
    found <- items[[2L]]
    expect_equal(found$kind, CompletionItemKind$File)
    expect_equal(normalizePath(found$detail), normalizePath(file.path(root, "sub", "help.R")))
    expect_equal(found$textEdit$range, range(position(0L, 18L), position(0L, 23L)))
    expect_equal(found$textEdit$newText, "help.R")
    expect_equal(items[[3L]]$kind, CompletionItemKind$Folder)
    items <- complete("sys.source(file = 'sub/h|")$items
    expect_equal(Filter(function(x) x$label == "h'elp.R", items)[[1L]]$textEdit$newText, "h\\'elp.R")
    folder <- complete('source("sub/he|BAD/nested.R")')$items
    expect_equal(map_chr(folder, "label"), "helpers/")
    expect_equal(folder[[1L]]$textEdit$newText, "helpers")
    expect_equal(map_chr(complete('base::source(r"-(sub/hel|)-")')$items, "label"),
        c("hello.R", "help.R", "helpers/"))
    expect_equal(map_chr(complete(c("base::source(", '    local = TRUE, file = "sub/he|'))$items, "label"),
        c("hello.R", "help.R", "helpers/"))
})

test_that("only actual unshadowed file arguments offer path choices", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    file.create(file.path(root, "helper.R"))
    workspace <- list(root = root)
    choices <- function(lines, language = "r") {
        f <- source_path_fixture(lines, workspace, language = language)
        source_path_completion(f$doc, f$point, workspace)
    }
    for (lines in list(
        'other::source("he|")', 'x$source("he|")', 'message("he|")', 'source(local = "he|")',
        'source("he|", file = "another.R")', 'source("he|", file = "another.R"', 'source(paste("he|", "llo"))', '# source("he|")',
        c('source <- function(x) "mine"', 'source("he|'), 'f <- function(source) source("he|',
        c("f <- function() {", "  source <- function(x) x", '  source("he|'))) {
        expect_null(choices(lines))
    }
    expect_null(choices(c('A source("he|") example.', "```{r}", "1", "```"), language = "quarto"))
    expect_equal(map_chr(choices(c('source <- function(x) "mine"', 'base::source("he|'))$items, "label"), "helper.R")
    previous <- lsp_settings$get("max_completions")
    withr::defer(lsp_settings$set("max_completions", previous))
    lsp_settings$set("max_completions", 1L)
    file.create(file.path(root, "hello.R"))
    limited <- choices('source("he|')
    expect_true(limited$isIncomplete)
    expect_length(limited$items, 1L)
})

test_that("typing the opening source quote requests files from the language server", {
    skip_on_cran()
    root <- withr::local_tempdir()
    main <- file.path(root, "main.R")
    writeLines("entry <- TRUE", file.path(root, "entry.R"))
    writeLines('source("', main)
    client <- language_client(root)
    did_open(client, main)
    res <- respond(client, "textDocument/completion", list(textDocument = list(uri = path_to_uri(main)),
            position = list(line = 0L, character = 8L), context = list(triggerKind = 2L, triggerCharacter = '"')))
    names <- map_chr(res$items, "label")
    expect_true("entry.R" %in% names)
    item <- res$items[[match("entry.R", names)]]
    expect_equal(item$kind, CompletionItemKind$File)
    expect_equal(item$textEdit$newText, "entry.R")
})
