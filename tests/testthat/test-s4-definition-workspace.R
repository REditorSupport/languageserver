test_that("definition follows sourced S4 declarations and uses the current editor buffer", {
    root <- withr::local_tempdir()
    paths <- file.path(root, c("main.R", "helper.R", "generic.R", "unrelated.R"))
    uris <- vapply(paths, path_to_uri, character(1L), USE.NAMES = FALSE)
    main <- c('source("helper.R")', 'source("generic.R")', "compileStanModel(object)")
    generic <- 'methods::setGeneric("compileStanModel", function(x) standardGeneric("compileStanModel"))'
    method <- 'methods::setMethod("compileStanModel", "A", function(x) x)'
    Map(writeLines, list(main, method, generic, method), paths)
    workspace <- Workspace$new(root)
    for (path in paths) workspace$index$update_path(path)
    doc <- Document$new(uris[[1L]], content = main)
    doc$did_open()
    workspace$documents$set(doc$uri, doc)
    workspace$update_parse_data(doc$uri, parse_document(doc$uri, main))
    definition <- function() definition_reply(1L, doc$uri, workspace, doc, list(row = 2L, col = 3L))$result

    targets <- definition()
    expect_identical(vapply(targets, `[[`, "", "uri"), uris[c(3L, 2L)])
    expect_equal(targets[[2L]]$range$start, position(0L, 0L))

    # The raw URI in the editor may differ from the canonical disk index key.
    retained <- Document$new(uris[[2L]], content = method)
    workspace$documents$set(retained$uri, retained)
    workspace$update_parse_data(retained$uri, parse_document(retained$uri, method))
    alias <- sub("helper[.]R$", "%68elper.R", uris[[2L]])
    edited <- Document$new(alias, version = 1L, content = c("", method))
    edited$did_open()
    workspace$documents$set(alias, edited)
    expect_identical(definition()$uri, uris[[3L]])
    parsed <- parse_document(alias, edited$content)
    parsed$version <- 1L
    workspace$update_parse_data(alias, parsed)
    targets <- definition()
    expect_identical(vapply(targets, `[[`, "", "uri"), c(uris[[3L]], alias))
    expect_equal(targets[[2L]]$range$start, position(1L, 0L))
    edited$set_content(2L, "1")
    expect_identical(definition()$uri, uris[[3L]])
    parsed <- parse_document(alias, edited$content)
    parsed$version <- 2L
    workspace$update_parse_data(alias, parsed)
    expect_identical(definition()$uri, uris[[3L]])
    workspace$index$remove(uris[[3L]])
    expect_length(workspace_s4_definitions(workspace, doc$uri, "compileStanModel"), 0L)
})

test_that("S4 methods stay within their package root and settle after failed parses", {
    root <- withr::local_tempdir()
    workspace <- Workspace$new(root)
    paths <- file.path(root, c("one", "two"), "R", "method.R")
    for (path in paths) {
        dir.create(dirname(path), recursive = TRUE)
        writeLines(c("Package: example", "Version: 1.0"), file.path(dirname(dirname(path)), "DESCRIPTION"))
        writeLines('setMethod("action", "Item", function(x) x)', path)
        workspace$index$update_path(path)
    }
    uris <- vapply(paths, path_to_uri, character(1L), USE.NAMES = FALSE)
    expect_setequal(workspace$index$s4_declaration_uris("action"), uris)
    for (uri in uris) {
        targets <- workspace_s4_definitions(workspace, uri, "action")
        expect_identical(vapply(targets, `[[`, "", "uri"), uri)
    }
    workspace$index$fail_parse(uris[[1L]], discard_summary = TRUE)
    expect_identical(workspace$index$s4_declaration_uris("action"), uris[[2L]])
    expect_length(workspace_s4_definitions(workspace, uris[[1L]], "action"), 0L)
})

test_that("unindexed S4 definitions track added and removed document declarations", {
    workspace <- Workspace$new(NULL)
    uri <- "untitled:registration"
    query <- "untitled:caller"
    doc <- Document$new(uri, content = 'setMethod("action", "X", function(x) x)')
    workspace$documents$set(uri, doc)
    workspace$documents$set(query, Document$new(query))
    workspace$update_parse_data(uri, parse_document(uri, doc$content))
    targets <- workspace_s4_definitions(workspace, query, "action")
    expect_identical(vapply(targets, `[[`, "", "uri"), uri)

    doc$set_content(NULL, "1")
    workspace$update_parse_data(uri, parse_document(uri, doc$content))
    testthat::local_mocked_bindings(workspace_document_uris = function(...) stop("unnecessary scope lookup"))
    expect_length(workspace_s4_definitions(workspace, query, "action"), 0L)
    expect_length(workspace_s4_definitions(workspace, query, "ordinary"), 0L)
})
