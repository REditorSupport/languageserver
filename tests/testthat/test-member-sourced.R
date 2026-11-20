test_that("member lookup follows visible literal source calls and the current target buffer", {
    root <- index_normalize_path(withr::local_tempdir())
    workspace <- Workspace$new(root)
    open <- function(name, lines) {
        path <- file.path(root, name)
        writeLines(lines, path)
        uri <- path_to_uri(path)
        doc <- Document$new(uri, language = "r", version = 1L, content = lines)
        workspace$documents$set(uri, doc)
        parsed <- parse_document(uri, lines)
        parsed$version <- doc$version
        workspace$update_parse_data(uri, parsed)
        doc
    }
    api <- open("api.R", c("obj <- new.env()", "obj$fun <- function(external) NULL"))
    open("other.R", "obj <- list(fun = function(unrelated) NULL)")
    main <- open("main.R", c("obj <- list(fun = function(original) NULL)", "obj$fun()",
            'source("api.R")', "obj$fun()", "obj$fun <- function(replacement) NULL", "obj$fun("))
    signature <- function(row) {
        result <- signature_reply(1L, main$uri, workspace, main, list(row = row, col = 8L))$result
        if (length(result$signatures)) result$signatures[[1L]]$label
    }
    definition <- function(row) {
        definition_reply(1L, main$uri, workspace, main, list(row = row, col = 5L))$result
    }
    expect_identical(signature(1L), "fun(original)")
    expect_identical(signature(3L), "fun(external)")
    expect_identical(signature(5L), "fun(replacement)")
    expect_equal(definition(3L), list(uri = api$uri, range = range(position(1L, 4L), position(1L, 7L))))
    expect_equal(definition(5L), list(uri = main$uri, range = range(position(4L, 4L), position(4L, 7L))))

    api$set_content(2L, c("# edited", "obj <- list(fun = function(updated) NULL)"))
    updated <- parse_document(api$uri, api$content)
    updated$version <- api$version
    workspace$update_parse_data(api$uri, updated)
    expect_identical(signature(3L), "fun(updated)")
    expect_equal(definition(3L), list(uri = api$uri, range = range(position(1L, 12L), position(1L, 15L))))

    api$set_content(3L, c("stats::", "", "obj <- list(fun = function(partial) NULL)"))
    updated <- parse_document(api$uri, api$content)
    updated$version <- api$version
    workspace$update_parse_data(api$uri, updated)
    expect_true(updated$parse_error)
    expect_identical(signature(3L), "fun(partial)")
    expect_equal(definition(3L), list(uri = api$uri, range = range(position(2L, 12L), position(2L, 15L))))

    main <- open("shadow.R", c("source <- function(...) NULL", 'source("api.R")', "obj$fun("))
    expect_null(signature(2L))
    main <- open("isolated.R", c('source("api.R", local = new.env())', "obj$fun("))
    expect_null(signature(1L))

    open("mid.R", 'source("api.R")')
    main <- open("transitive.R", c('source("mid.R")', "obj$fun("))
    expect_identical(signature(1L), "fun(partial)")
    extension <- open("extend.R", 'obj[["fun"]] <- function(extra) NULL')
    main <- open("extend-main.R", c("obj <- new.env()", 'source("extend.R")', "obj$fun("))
    expect_identical(signature(2L), "fun(extra)")
    expect_equal(definition(2L), list(uri = extension$uri, range = range(position(0L, 5L), position(0L, 10L))))

    open("rebind.R", "alias <- new.env()")
    main <- open("alias-main.R", c("obj <- new.env()", "obj$fun <- function(original) NULL",
            "alias <- obj", 'source("rebind.R")', "alias$fun <- function(other) NULL", "obj$fun("))
    expect_identical(signature(5L), "fun(original)")
    open("mask.R", "source <- function(...) NULL")
    open("mid.R", 'source("mask.R")')
    main <- open("mask-main.R", c('source("mid.R")', 'source("api.R")', "obj$fun("))
    expect_null(signature(2L))
})

test_that("an incomplete edit updates proven source edges and retains the last valid fallback", {
    root <- index_normalize_path(withr::local_tempdir())
    paths <- file.path(root, c("main.R", "first.R", "second.R", "third.R"))
    for (path in paths) writeLines("", path)
    uris <- vapply(paths, path_to_uri, character(1L), USE.NAMES = FALSE)
    index <- WorkspaceIndex$new(root)
    update <- function(lines) {
        summary <- index$update_content(uris[[1L]], lines, parse_data = parse_document(uris[[1L]], lines))
        expect_identical(summary, index$summaries$get(uris[[1L]]))
        expect_true(index_source_candidates_current(summary))
        index$source_edges$get(uris[[1L]])
    }
    expect_identical(update('source("first.R")'), uris[[2L]])
    # Reuse the ordinary index collector for every complete statement, even
    # when its syntax falls outside member inference's unconditional source().
    expect_setequal(update(c('if (ok) base::sys.source(file.path("second.R"), envir = env)', "obj$fun(")),
        uris[c(2L, 3L)])
    expect_setequal(update(c('source("third.R")', "obj$fun(")), uris[c(2L, 4L)])
    expect_false(uris[[1L]] %in% index$candidate_reverse_edges$get(uris[[3L]], character()))
    expect_identical(update("source("), uris[[2L]])
    expect_identical(update('source("third.R")'), uris[[4L]])
    expect_identical(update("source("), uris[[4L]])
})

test_that("first member definition loads the current closed source after incomplete edits", {
    skip_on_cran()
    root <- index_normalize_path(withr::local_tempdir())
    main <- file.path(root, "main.R")
    api <- file.path(root, "api.R")
    other <- file.path(root, "other.R")
    writeLines(c("obj <- new.env()", "obj$fun <- function(x) x"), api)
    writeLines("obj <- list(fun = function(y) y)", other)
    lines <- c('source("api.R")', "obj$fun(")
    writeLines(lines, main)
    uri <- path_to_uri(main)
    client <- language_client(root)
    notify(client, "textDocument/didOpen", list(textDocument = list(uri = uri,
                languageId = "r", version = 1L, text = paste(lines, collapse = "\n"))))
    first_definition <- function() respond_definition(client, main, c(1L, 5L), retry = FALSE)
    expect_identical(first_definition()$uri, path_to_uri(api))
    edit <- function(line, version) {
        notify(client, "textDocument/didChange", list(textDocument = list(uri = uri, version = version),
                contentChanges = list(list(text = paste(line, "obj$fun(", sep = "\n")))))
    }
    edit('source("other.R")', 2L)
    expect_identical(first_definition()$uri, path_to_uri(other))
    edit('source("missing.R")', 3L)
    expect_length(first_definition(), 0L)
    edit('source("api.R")', 4L)
    expect_identical(first_definition()$uri, path_to_uri(api))
})
