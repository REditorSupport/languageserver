test_that("Workspace validates caches and resolves imported namespaces", {
    old_parse <- lsp_settings$get("parse_cache_max_mb")
    old_diagnostics <- lsp_settings$get("diagnostics_cache_max_mb")
    withr::defer({
        lsp_settings$set("parse_cache_max_mb", old_parse)
        lsp_settings$set("diagnostics_cache_max_mb", old_diagnostics)
    })
    lsp_settings$set("parse_cache_max_mb", NA_real_)
    lsp_settings$set("diagnostics_cache_max_mb", -1)
    workspace <- Workspace$new(NULL)

    workspace$imported_objects$set("coverage_only_object", "base")
    expect_equal(workspace$guess_namespace("coverage_only_object"), "base")
    expect_null(workspace$get_namespace("coveragePackageThatDoesNotExist"))
    expect_length(workspace$get_definitions_for_uri("file:///missing.R"), 0L)
    expect_null(workspace$import_from_namespace_file())
})

test_that("Workspace caches rendered help", {
    workspace <- Workspace$new(NULL)
    old_rich <- lsp_settings$get("rich_documentation")
    withr::defer(lsp_settings$set("rich_documentation", old_rich))
    lsp_settings$set("rich_documentation", FALSE)

    first <- workspace$get_help("mean", "base")
    second <- workspace$get_help("mean", "base")
    expect_false(is.null(first))
    expect_identical(second, first)
    expect_true(workspace$help_cache$size() >= 1L)
})

test_that("Workspace diagnostics globals include package source definitions", {
    root <- withr::local_tempdir()
    writeLines(c("Package: coveragefixture", "Version: 0.0.1"),
        file.path(root, "DESCRIPTION"))
    source_dir <- file.path(root, "R")
    dir.create(source_dir)
    workspace <- Workspace$new(root)

    parsed <- Document$new(
        path_to_uri(file.path(source_dir, "parsed.R")),
        content = "global <- 1"
    )
    parsed$parse_data <- list(
        nonfuncts = "global",
        functions = list(helper = function() TRUE)
    )
    unparsed <- Document$new(
        path_to_uri(file.path(source_dir, "unparsed.R")),
        content = "ignored <- 1"
    )
    outside <- Document$new(
        path_to_uri(file.path(root, "outside.R")),
        content = "outside <- 1"
    )
    outside$parse_data <- list(
        nonfuncts = "outside",
        functions = list()
    )
    workspace$documents$set(parsed$uri, parsed)
    workspace$documents$set(unparsed$uri, unparsed)
    workspace$documents$set(outside$uri, outside)

    globals <- workspace$get_diagnostics_globals()
    expect_true(exists("global", globals, inherits = FALSE))
    expect_true(exists("helper", globals, inherits = FALSE))
    expect_false(exists("outside", globals, inherits = FALSE))
    expect_identical(workspace$get_diagnostics_globals(), globals)
})

test_that("Workspace parses named NAMESPACE imports and polls recent files", {
    root <- withr::local_tempdir()
    writeLines(c("Package: coveragefixture", "Version: 0.0.1"),
        file.path(root, "DESCRIPTION"))
    writeLines(c(
        "import(base, except = c(mean))",
        "importFrom(stats, median)"
    ), file.path(root, "NAMESPACE"))
    workspace <- Workspace$new(root)

    workspace$import_from_namespace_file()
    expect_true("base" %in% workspace$imported_packages)
    expect_equal(workspace$imported_objects$get("median"), "stats")
    expect_null(workspace$poll_namespace_file())
})

test_that("workspace handlers remove folders and ignore unrelated file events", {
    self <- new.env(parent = baseenv())
    self$removed <- character()
    self$remove_workspace <- function(uri) {
        self$removed <- c(self$removed, uri)
    }
    workspace_did_change_workspace_folders(self, list(event = list(
        added = list(),
        removed = list(list(uri = "file:///removed", name = "removed"))
    )))
    expect_equal(self$removed, "file:///removed")

    plain_root <- withr::local_tempdir()
    package_root <- withr::local_tempdir()
    writeLines(c("Package: handlerfixture", "Version: 0.0.1"),
        file.path(package_root, "DESCRIPTION"))
    dir.create(file.path(package_root, "R"))
    plain <- Workspace$new(plain_root)
    package <- Workspace$new(package_root)
    open_path <- file.path(package_root, "R", "open.R")
    writeLines("value <- 1", open_path)
    open_document <- Document$new(path_to_uri(open_path), content = "value <- 1")
    open_document$did_open()
    package$documents$set(open_document$uri, open_document)
    self$get_workspace <- function(uri) {
        if (path_has_parent(path_from_uri(uri), package_root)) package else plain
    }
    self$text_sync <- function(...) stop("ignored events must not be synchronized")

    workspace_did_change_watched_files(self, list(changes = list(
        list(
            uri = path_to_uri(file.path(plain_root, "plain.R")),
            type = FileChangeType$Changed
        ),
        list(
            uri = path_to_uri(file.path(package_root, "outside.R")),
            type = FileChangeType$Changed
        ),
        list(uri = open_document$uri, type = FileChangeType$Changed)
    )))
    expect_true(package$documents$has(open_document$uri))
})

test_that("get_diagnostics_globals includes NAMESPACE and Depends imports via parseNamespaceFile", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"), recursive = TRUE)
    writeLines(c(
        "Package: importglobalsfixture",
        "Version: 0.1.0",
        "Depends: R (>= 4.0.0), methods"
    ), file.path(root, "DESCRIPTION"))
    writeLines(c(
        "import(stats, except = c(mad))",
        "import(grDevices)",
        "importFrom(utils, head, tail)",
        "importFrom(tools, file_ext)"
    ), file.path(root, "NAMESPACE"))

    helper_path <- file.path(root, "R", "helper.R")
    helper_lines <- c(
        "pkg_helper <- function(x, y = 1) x + y",
        "pkg_const <- 42",
        ""
    )
    writeLines(helper_lines, helper_path)

    main_path <- file.path(root, "R", "main.R")
    main_lines <- c(
        "main_fn <- function(z) pkg_helper(z) + pkg_const",
        ""
    )
    writeLines(main_lines, main_path)

    workspace <- Workspace$new(root)
    globals_indexed <- workspace$get_diagnostics_globals(path_to_uri(main_path))

    expect_true(exists("pkg_helper", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_equal(names(formals(globals_indexed$pkg_helper)), c("x", "y"))
    expect_true(exists("pkg_const", envir = globals_indexed, inherits = FALSE))
    expect_true(exists("sd", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_false(exists("mad", envir = globals_indexed, inherits = FALSE))
    expect_true(exists("head", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_true(exists("tail", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_true(exists("file_ext", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_true(exists("rgb", envir = globals_indexed, mode = "function", inherits = FALSE))
    expect_true(exists("is", envir = globals_indexed, mode = "function", inherits = FALSE))

    helper_doc <- Document$new(path_to_uri(helper_path), content = helper_lines)
    helper_doc$update_parse_data(parse_document(helper_doc$uri, helper_lines))
    workspace$documents$set(helper_doc$uri, helper_doc)

    globals_unindexed <- workspace$get_diagnostics_globals()
    expect_true(exists("pkg_helper", envir = globals_unindexed, mode = "function", inherits = FALSE))
    expect_true(exists("sd", envir = globals_unindexed, mode = "function", inherits = FALSE))
    expect_false(exists("mad", envir = globals_unindexed, inherits = FALSE))
    expect_true(exists("head", envir = globals_unindexed, mode = "function", inherits = FALSE))
    expect_true(exists("file_ext", envir = globals_unindexed, mode = "function", inherits = FALSE))
})
