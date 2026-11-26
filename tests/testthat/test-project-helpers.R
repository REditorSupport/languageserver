test_that("Go to Definition limits project helpers to the configured callers and workspace", {
    skip_on_cran()
    package <- withr::local_tempdir()
    scripts <- file.path(package, "scripts")
    dir.create(scripts)
    dir.create(file.path(package, "R"))
    writeLines("Package: helperfixture", file.path(package, "DESCRIPTION"))
    dir.create(file.path(package, "tests", "testthat"), recursive = TRUE)
    test_file <- file.path(package, "tests", "testthat", "test-example.R")
    first <- file.path(package, "tests", "testthat", "helper-a.R")
    second <- file.path(package, "tests", "testthat", "helper-b.R")
    sourced <- file.path(package, "explicit.R")
    writeLines(c("source('explicit.R')", "fixture_helper()", "from_source()", "print(1)",
            "base::print(1)", "shadowed <- function() 0", "shadowed()", 'glue::glue("{print(1)}")'), test_file)
    writeLines(c("fixture_helper <- function() 1", "from_source <- function() 1",
            "shadowed <- function() 1", "print <- function(x) x"), first)
    writeLines(c("", "fixture_helper <- function() 2"), second)
    writeLines("from_source <- function() 3", sourced)
    script <- file.path(scripts, "main.R")
    other <- file.path(scripts, "other.R")
    sibling <- file.path(scripts, "helper.R")
    writeLines(c("fixture_helper()", 'glue::glue("{fixture_helper()}")'), script)
    writeLines("fixture_helper()", other)
    writeLines("fixture_helper <- function() 4", sibling)

    client <- language_client(working_dir = package, workspace_folders = c(package, scripts))
    did_open(client, test_file)
    did_open(client, script)
    lookup <- function(path, line = 0L, col = 1L) {
        respond_definition(client, path, c(line, col), retry = FALSE)
    }
    expect_length(lookup(test_file, 1L), 0L)
    rules <- list("tests/testthat/test-*.R" = c("scripts/helper.R", "tests/testthat/helper-b.R", "tests/testthat/helper-a.R"),
        "tests/**/*.R" = "tests/testthat/helper-b.R", "main.R" = "helper.R")
    notify(client, "workspace/didChangeConfiguration", list(settings = list(project_helpers = rules)))

    hits <- lookup(test_file, 1L)
    expect_equal(vapply(hits, `[[`, "", "uri"), c(path_to_uri(second), path_to_uri(first)))
    expect_equal(hits[[1L]]$range$start, list(line = 1, character = 0))
    expect_equal(lookup(test_file, 2L)$uri, path_to_uri(sourced))
    expect_equal(lookup(test_file, 3L)$uri, path_to_uri(first))
    expect_true(endsWith(lookup(test_file, 4L, 7L)$uri, "print.R"))
    expect_equal(lookup(test_file, 6L)$uri, path_to_uri(test_file))
    expect_equal(lookup(test_file, 7L, 16L)$uri, path_to_uri(first))
    # First navigation in the lazy workspace happens inside a glue template.
    expect_equal(lookup(script, 1L, 16L)$uri, path_to_uri(sibling))
    expect_equal(lookup(script)$uri, path_to_uri(sibling))
    did_open(client, other)
    expect_length(lookup(other), 0L)

    notify(client, "workspace/didChangeConfiguration", list(settings = list(project_helpers = list())))
    expect_length(lookup(script), 0L)
})

test_that("a helper's latest buffer wins over its disk definitions until close", {
    skip_on_cran()
    root <- withr::local_tempdir()
    caller <- file.path(root, "query.R")
    helper <- file.path(root, "helper.R")
    writeLines(c("on_disk()", "in_buffer()"), caller)
    writeLines("on_disk <- function() 1", helper)
    client <- language_client(working_dir = root)
    notify(client, "workspace/didChangeConfiguration", list(settings = list(
        project_helpers = list("query.R" = "helper.R"), parse_delay = 0.5)))
    did_open(client, caller)
    lookup <- function(line) respond_definition(client, caller, c(line, 2L), retry = FALSE)
    expect_equal(lookup(0L)$uri, path_to_uri(helper))
    did_open(client, helper)

    notify(client, "textDocument/didChange", list(textDocument = list(uri = path_to_uri(helper), version = 2L),
            contentChanges = list(list(text = "\n'\U0001F986'; in_buffer <- function() 2"))))
    expect_length(lookup(0L), 0L)
    live <- lookup(1L)
    expect_equal(live$uri, path_to_uri(helper))
    expect_equal(live$range$start, list(line = 1, character = 6))

    notify(client, "textDocument/didClose", list(textDocument = list(uri = path_to_uri(helper))))
    expect_length(lookup(1L), 0L)
    expect_equal(lookup(0L)$range$start, list(line = 0, character = 0))

    writeLines(c("# external edit without a file notification", "", "on_disk <- function() 2"), helper)
    expect_equal(lookup(0L)$range$start, list(line = 2, character = 0))
    watched <- function(type) {
        notify(client, "workspace/didChangeWatchedFiles", list(changes = list(list(uri = path_to_uri(helper), type = type))))
    }
    writeLines(c("# changed on disk", "in_buffer <- function() 3"), helper)
    watched(FileChangeType$Changed)
    expect_length(lookup(0L), 0L)
    expect_equal(lookup(1L)$range$start, list(line = 1, character = 0))
    unlink(helper)
    watched(FileChangeType$Deleted)
    expect_length(lookup(1L), 0L)
    writeLines(c("", "", "on_disk <- function() 4"), helper)
    watched(FileChangeType$Created)
    expect_equal(lookup(0L)$range$start, list(line = 2, character = 0))

    notify(client, "workspace/didChangeConfiguration", list(settings = list(index_mode = "off")))
    expect_length(lookup(0L), 0L)
})

test_that("a lazy definition request reads only eligible helpers and reuses their summaries", {
    root <- withr::local_tempdir()
    caller <- path_to_uri(file.path(root, "query.R"))
    helper <- file.path(root, "helper.R")
    dir.create(file.path(root, "ignored"))
    writeLines(c("from_helper <- function() 1", "v <- 10L"), helper)
    writeLines("from_helper <- function() 2", file.path(root, "ignored", "helper.R"))
    writeLines(rep("from_helper <- function() 3", 100L), file.path(root, "large.R"))
    writeLines("from_helper <- function() 4", file.path(root, "a-unrelated.R"))
    settings <- list(project_helpers = list("query.R" = c("helper.R", "ignored/helper.R", "large.R")),
        index_mode = "lazy", index_exclude = "ignored/", index_max_file_size_mb = 0.001,
        index_max_files = 1L, index_persistent_cache = FALSE, s3_dispatch = FALSE, member_completion = FALSE)
    previous <- lapply(names(settings), lsp_settings$get)
    withr::defer(for (i in seq_along(previous)) lsp_settings$set(names(settings)[[i]], previous[[i]]))
    for (name in names(settings)) lsp_settings$set(name, settings[[name]])

    workspace <- Workspace$new(root)
    document <- Document$new(caller, "r", 1L, c("local_fn <- function() 1", "local_fn()", "from_helper()",
            "df <- data.frame(v = 1L)", "dplyr::filter(df, v > .env$v)", "dplyr::filter(unknown, v > 0)",
            "v", "f <- function(v) dplyr::filter(df, .env$v)"))
    document$did_open()
    workspace$documents$set(caller, document)
    parsed <- parse_document(caller, document$content)
    parsed$version <- document$version
    workspace$update_parse_data(caller, parsed)
    server <- list(workspaces = collections::dict(), get_workspace = function(uri) workspace, deliver = identity)
    server$workspaces$set(DEFAULT_WORKSPACE, workspace)
    lookup <- function(line, col = 1L) {
        params <- list(textDocument = list(uri = caller), position = position(line, col))
        text_document_definition(server, 1L, params)$result
    }
    original_read <- read_text_lines
    reads <- character()
    testthat::local_mocked_bindings(read_text_lines = function(path, ...) {
        reads <<- c(reads, path)
        original_read(path, ...)
    }, .package = "languageserver")

    expect_equal(lookup(1L)$uri, caller)
    column <- lookup(4L, 18L)
    expect_equal(column$uri, caller)
    expect_equal(column$range, range(position(3L, 17L), position(3L, 18L)))
    expect_null(lookup(5L, 23L))
    local <- lookup(7L, 40L)
    expect_equal(local$uri, caller)
    expect_equal(local$range, range(position(7L, 14L), position(7L, 15L)))
    expect_length(reads, 0L)
    from_env <- lookup(4L, 27L)
    expect_equal(from_env$uri, path_to_uri(helper))
    expect_equal(from_env$range$start, position(1L, 0L))
    expect_equal(lookup(6L, 0L), from_env)
    expect_equal(lookup(2L)$uri, path_to_uri(helper))
    expect_equal(lookup(2L)$uri, path_to_uri(helper))
    expect_equal(path_to_uri(reads), path_to_uri(helper))
    expect_setequal(workspace$documents$keys(), caller)
    expect_false(workspace$index$full_started)

    index <- workspace$index
    # Full discovery spends its one slot on a-unrelated.R and evicts helper.R.
    index$discover()
    expect_true(index$truncated)
    expect_false(index$summaries$has(path_to_uri(helper)))
    expect_equal(lookup(2L)$uri, path_to_uri(helper))
    set_rules <- function(rules) {
        workspace_did_change_configuration(server, list(settings = list(project_helpers = rules)))
    }
    set_rules(list())
    expect_identical(workspace$index, index)
    expect_null(lookup(2L))
    set_rules(settings$project_helpers)
    expect_identical(workspace$index, index)
    expect_equal(lookup(2L)$uri, path_to_uri(helper))
})
