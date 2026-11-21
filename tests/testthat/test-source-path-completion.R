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

test_that("path completion and links identify the actual unshadowed base file argument", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    file.create(file.path(root, "R", c("helper.R", "joined_child.R")))
    dir.create(file.path(root, "R", "helpers"))
    file.create(file.path(root, "helpers"))
    ws <- list(root = root)
    choices <- function(code) {
        row <- grep("CURSOR", code, fixed = TRUE)
        col <- regexpr("CURSOR", code[[row]], fixed = TRUE)[[1L]] - 1L
        code[[row]] <- sub("CURSOR", "", code[[row]], fixed = TRUE)
        doc <- Document$new(path_to_uri(file.path(root, "R", "main.R")), language = "r", version = 1L, content = code)
        data <- parse_document(doc$uri, code)
        data$version <- 1L
        doc$update_parse_data(data)
        result <- source_path_completion(doc, list(row = row - 1L, col = col), ws)
        vapply(result$items, `[[`, "", "label")
    }
    for (code in list('obj $ source("heCURSOR', 'obj $ source("heCURSOR")', 'other :: source("heCURSOR',
            c("obj $", 'source("heCURSOR'), 'x %>% source("heCURSOR")')) {
        expect_length(choices(code), 0L)
    }
    expect_true("helper.R" %in% choices('source(fil = "heCURSOR")'))
    expect_identical(choices('source("helCURSOR/nested.R")'), "helpers/")
    if (getRversion() >= "4.1.0") {
        expect_length(choices('input |> source("heCURSOR")'), 0L)
        expect_true("helper.R" %in% choices('input |> base::source(file = "heCURSOR")'))
    }

    links <- function(code) {
        uri <- path_to_uri(file.path(root, "R", "main.R"))
        doc <- Document$new(uri, language = "r", version = 1L, content = code)
        parsed <- parse_document(uri, code)
        parsed$version <- 1L
        doc$update_parse_data(parsed)
        workspace <- list(root = root, get_parse_data = function(...) parsed)
        result <- document_link_reply(1L, uri, workspace, doc, root)$result
        vapply(result, function(link) basename(link$data$path), "")
    }
    expect_length(links(c("source <- function(...) NULL", 'source(file.path("helper.R"))')), 0L)
    expect_length(links('f <- function(file.path) base::source(file.path("helper.R"))'), 0L)
    expect_identical(links('base::source(fil="helper.R")'), "helper.R")
    expect_identical(links('"helper.R" %>% source(file=.)'), "helper.R")
    expect_identical(links('base::source(file.path("joined", "child.R", fsep="_"))'), "joined_child.R")
    if (getRversion() >= "4.1.0") {
        expect_length(links('"absent.R" |> source("helper.R")'), 0L)
        expect_identical(links('"helper.R" |> source()'), "helper.R")
    }
})

test_that("source quote requests after a long prefix use the current parse", {
    skip_on_cran()
    root <- withr::local_tempdir()
    path <- file.path(root, "main.R")
    writeLines("ok <- TRUE", file.path(root, "helper.R"))
    client <- language_client(root)
    lines <- c(rep("ok <- 1", 180L), "base::source(")
    did_open(client, path, text = lines)
    notify(client, "workspace/didChangeConfiguration", list(settings = list(parse_delay = 0.5)))
    uri <- path_to_uri(path)
    complete <- function(line, version) {
        lines[[length(lines)]] <- line
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = list(list(text = paste(lines, collapse = "\n")))))
        result <- respond(client, "textDocument/completion", list(textDocument = list(uri = uri),
                position = list(line = length(lines) - 1L, character = nchar(line)),
                context = list(triggerKind = 2L, triggerCharacter = '"')), retry = FALSE)
        vapply(result$items, `[[`, "", "label")
    }
    expect_true("helper.R" %in% complete('base::source("', 2L))
    expect_length(complete('obj $ source("', 3L), 0L)
})

test_that("source path recovery checks later arguments while the parse is stale", {
    root <- withr::local_tempdir()
    file.create(file.path(root, "helper.R"))
    workspace <- list(root = root)
    choices <- function(lines) {
        fixture <- source_path_fixture(lines, workspace)
        fixture$doc$parse_data$version <- 0L
        source_path_completion(fixture$doc, fixture$point, workspace)
    }
    selected <- choices(c('base::source("he|",', "    local = TRUE)", "later <- 1"))
    expect_identical(map_chr(selected$items, "label"), "helper.R")
    expect_null(choices(c('base::source("he|",', '    file = "other.R")')))
    expect_null(choices(c('base::source("he|",', paste0("#", strrep("a", 16500L)), ")")))
    expect_null(choices(c(rep("# older lines", 130L), 'base::source("he|")')))
    expect_null(choices('base::source("https://example.org/he|")'))
})
