local_index_settings <- function(...) {
    values <- list(...)
    old <- lapply(names(values), lsp_settings$get)
    names(old) <- names(values)
    withr::defer({
        for (name in names(old)) lsp_settings$set(name, old[[name]])
    }, envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

test_that("index globs include R files and prune excluded directories", {
    expect_true(index_glob_match("script.R", "**/*.R"))
    expect_true(index_glob_match("analysis/script.r", "**/*.R"))
    expect_false(index_glob_match("analysis/script.Rmd", "**/*.R"))
    expect_true(index_glob_match("renv/", "**/renv/**"))
    expect_true(index_glob_match("nested/.git/", "**/.git/**"))
})

test_that("shallow summaries extract definitions and static source calls", {
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("first <- function(x) x", file.path(root, "R", "first.R"))
    writeLines("second <- 2", file.path(root, "R", "second.R"))
    writeLines("third <- 3", file.path(root, "R", "third.R"))
    writeLines("fourth <- 4", file.path(root, "R", "fourth.R"))
    path <- file.path(root, "main.R")
    content <- c(
        "source(file.path(\"R\", \"first.R\"))",
        "base::source(\"R/second.R\")",
        "sys.source(file = here::here(\"R\", \"third.R\"), envir = environment())",
        "source(dynamic_path)",
        "value <- 1",
        "work <- function(x) x"
    )
    writeLines(content, path)

    summary <- index_shallow_summary(path, content, root)
    expect_setequal(names(summary$definitions), c("value", "work"))
    expect_equal(summary$definitions$work$type, "function")
    expect_setequal(
        basename(vapply(summary$sources, path_from_uri, character(1L))),
        c("first.R", "second.R", "third.R")
    )
    expect_false(any(grepl("dynamic", summary$sources, fixed = TRUE)))
})

test_that("source discovery tolerates missing call arguments", {
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    helper <- file.path(root, "helper.R")
    writeLines("helper <- TRUE", helper)
    path <- file.path(root, "main.R")
    content <- c(
        "ordinary_call(, value)",
        "source(file = )",
        "source(\"helper.R\", )"
    )
    writeLines(content, path)

    summary <- index_shallow_summary(path, content, root)

    expect_equal(summary$sources, list(path_to_uri(helper)))
})

test_that("source closure is transitive and cycle safe", {
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    writeLines(c("source(\"a.R\")", "main <- TRUE"),
        file.path(root, "main.R"))
    writeLines(c("source(\"b.R\")", "a <- TRUE"), file.path(root, "a.R"))
    writeLines(c("source(\"a.R\")", "b <- TRUE"), file.path(root, "b.R"))

    index <- WorkspaceIndex$new(root)
    index$discover()
    while (length(index$pending)) index$process_batch()

    closure <- index$source_closure(path_to_uri(file.path(root, "main.R")))
    expect_length(closure, 3L)
    expect_setequal(
        basename(vapply(closure, path_from_uri, character(1L))),
        c("main.R", "a.R", "b.R")
    )
    dependents <- index$dependent_closure(
        path_to_uri(file.path(root, "b.R")))
    expect_setequal(
        basename(vapply(dependents, path_from_uri, character(1L))),
        c("main.R", "a.R", "b.R")
    )
})

test_that("source edges survive incomplete edits and resolve created targets", {
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    main <- file.path(root, "main.R")
    helper <- file.path(root, "helper.R")
    future <- file.path(root, "future.R")
    writeLines(c("source(\"helper.R\")", "source(\"future.R\")"), main)
    writeLines("helper <- TRUE", helper)

    index <- WorkspaceIndex$new(root)
    index$discover()
    while (length(index$pending)) index$process_batch()
    main_uri <- path_to_uri(main)
    helper_uri <- index_canonical_uri(path_to_uri(helper))
    expect_true(helper_uri %in% index$source_closure(main_uri))

    index$update_content(main_uri, c("source(\"helper.R\")", "broken("))
    expect_true(helper_uri %in% index$source_closure(main_uri))

    # Restore the valid buffer, then create a previously unresolved target.
    index$update_content(main_uri,
        c("source(\"helper.R\")", "source(\"future.R\")"))
    writeLines("future <- TRUE", future)
    future_uri <- index_canonical_uri(path_to_uri(future))
    expect_true(index_canonical_uri(main_uri) %in% index$dependents(
        future_uri, include_candidates = TRUE))
    index$update_path(main)
    expect_true(future_uri %in% index$source_closure(main_uri))
})

test_that("discovery enforces exclusions, file sizes, and file limits", {
    local_index_settings(
        index_persistent_cache = FALSE,
        index_max_files = 2L,
        index_max_file_size_mb = 0.0001
    )
    root <- withr::local_tempdir()
    dir.create(file.path(root, "renv"))
    writeLines("ignored <- 1", file.path(root, "renv", "ignored.R"))
    writeLines(strrep("x", 500L), file.path(root, "large.R"))
    writeLines("a <- 1", file.path(root, "a.R"))
    writeLines("b <- 1", file.path(root, "b.R"))
    writeLines("c <- 1", file.path(root, "c.R"))

    index <- WorkspaceIndex$new(root)
    index$discover()
    paths <- vapply(index$files$values(), `[[`, character(1L), "path")
    expect_length(paths, 2L)
    expect_false(any(grepl("renv", paths, fixed = TRUE)))
    expect_false(any(endsWith(paths, "large.R")))
    expect_true(index$truncated)
})

test_that("nested package roots are detected independently", {
    root <- withr::local_tempdir()
    for (package in c("one", "two")) {
        package_root <- file.path(root, package)
        dir.create(file.path(package_root, "R"), recursive = TRUE)
        writeLines(c(
            paste0("Package: ", package),
            "Version: 0.0.1"
        ), file.path(package_root, "DESCRIPTION"))
        writeLines("value <- 1", file.path(package_root, "R", "code.R"))
    }
    one <- index_package_root(file.path(root, "one", "R", "code.R"), root)
    two <- index_package_root(file.path(root, "two", "R", "code.R"), root)
    expect_equal(basename(one), "one")
    expect_equal(basename(two), "two")
    expect_false(identical(one, two))
})

test_that("semantic workspace scope isolates scripts but includes sources", {
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    paths <- file.path(root, c("main.R", "helper.R", "unrelated.R"))
    contents <- list(
        c("source(\"helper.R\")", "main_fun <- function() helper_fun()"),
        "helper_fun <- function() TRUE",
        "unrelated_fun <- function() TRUE"
    )
    Map(writeLines, contents, paths)

    workspace <- Workspace$new(root)
    workspace$index$discover()
    while (length(workspace$index$pending)) workspace$index$process_batch()
    for (i in seq_along(paths)) {
        uri <- path_to_uri(paths[[i]])
        doc <- Document$new(uri, content = contents[[i]])
        parsed <- parse_document(uri, contents[[i]])
        doc$parse_data <- as.list(parsed)
        workspace$documents$set(uri, doc)
    }

    main_uri <- path_to_uri(paths[[1L]])
    unrelated_uri <- path_to_uri(paths[[3L]])
    main_symbols <- workspace$get_namespace(
        WORKSPACE, uri = main_uri)$get_symbols(TRUE)
    unrelated_symbols <- workspace$get_namespace(
        WORKSPACE, uri = unrelated_uri)$get_symbols(TRUE)

    expect_setequal(main_symbols, c("main_fun", "helper_fun"))
    expect_equal(unrelated_symbols, "unrelated_fun")
    expect_false("unrelated_fun" %in% main_symbols)

    workspace_symbols <- workspace$get_definitions_for_query("fun")
    expect_setequal(
        vapply(workspace_symbols, `[[`, character(1L), "name"),
        c("main_fun", "helper_fun", "unrelated_fun")
    )
})

test_that("plain projects load source closures without merging unrelated scripts", {
    skip_on_cran()
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    main <- file.path(root, "main.R")
    writeLines(c(
        "source(\"helper.R\")",
        "helper_fun()",
        "unrelated_fun()"
    ), main)
    writeLines("helper_fun <- function(value = 1) value",
        file.path(root, "helper.R"))
    writeLines("unrelated_fun <- function(hidden = TRUE) hidden",
        file.path(root, "unrelated.R"))

    client <- language_client(root)
    client %>% did_open(main)

    sourced <- client %>% respond_signature(main, c(1, 11),
        retry_when = function(result) length(result$signatures) == 0L)
    expect_length(sourced$signatures, 1L)
    expect_match(sourced$signatures[[1L]]$label, "helper_fun\\(value")

    isolated <- client %>% respond_signature(main, c(2, 14),
        retry_when = function(result) length(result$signatures) > 0L)
    expect_length(isolated$signatures, 0L)

    symbols <- client %>% respond_workspace_symbol("unrelated_fun",
        retry_when = function(result) length(result) == 0L)
    expect_length(symbols, 1L)
})

test_that("references and code lenses include source dependents", {
    skip_on_cran()
    local_index_settings(index_persistent_cache = FALSE)
    root <- withr::local_tempdir()
    definition <- file.path(root, "src_test1.R")
    caller <- file.path(root, "src_test2.R")
    unrelated <- file.path(root, "unrelated.R")
    writeLines(c(
        "test1 <- 1",
        "test2 <- 2",
        "fun1 <- function(x) {",
        "  x + 1",
        "}"
    ), definition)
    writeLines(c(
        "source(\"./src_test1.R\")",
        "",
        "fun1(x)"
    ), caller)
    writeLines("fun1(x)", unrelated)

    client <- language_client(root, capabilities = list(
        textDocument = list(codeLens = list(
            resolveSupport = list(properties = list("command"))
        ))
    ))
    client %>% did_open(caller)
    client %>% did_open(unrelated)

    from_call <- client %>% respond_references(
        caller, c(2, 1), retry_when = function(result) length(result) < 2L)
    expect_setequal(
        vapply(from_call, `[[`, character(1L), "uri"),
        path_to_uri(c(definition, caller))
    )

    client %>% did_open(definition)
    from_definition <- client %>% respond_references(
        definition, c(2, 1),
        retry_when = function(result) length(result) < 2L)
    expect_setequal(
        vapply(from_definition, `[[`, character(1L), "uri"),
        path_to_uri(c(definition, caller))
    )

    lenses <- respond(
        client,
        "textDocument/codeLens",
        list(textDocument = list(uri = path_to_uri(definition)))
    )
    expect_length(lenses, 1L)
    resolved <- respond(client, "codeLens/resolve", lenses[[1L]])
    expect_equal(resolved$command$title, "1 call")
})
