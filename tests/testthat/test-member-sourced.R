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

    main <- open("shadow.R", c("source <- function(...) NULL", 'source("api.R")', "obj$fun("))
    expect_null(signature(2L))
    main <- open("isolated.R", c('source("api.R", local = new.env())', "obj$fun("))
    expect_null(signature(1L))

    open("mid.R", 'source("api.R")')
    main <- open("transitive.R", c('source("mid.R")', "obj$fun("))
    expect_identical(signature(1L), "fun(updated)")
    open("extend.R", "obj$fun <- function(extra) NULL")
    main <- open("extend-main.R", c("obj <- new.env()", 'source("extend.R")', "obj$fun("))
    expect_identical(signature(2L), "fun(extra)")

    open("rebind.R", "alias <- new.env()")
    main <- open("alias-main.R", c("obj <- new.env()", "obj$fun <- function(original) NULL",
            "alias <- obj", 'source("rebind.R")', "alias$fun <- function(other) NULL", "obj$fun("))
    expect_identical(signature(5L), "fun(original)")
    open("mask.R", "source <- function(...) NULL")
    open("mid.R", 'source("mask.R")')
    main <- open("mask-main.R", c('source("mid.R")', 'source("api.R")', "obj$fun("))
    expect_null(signature(2L))
})
