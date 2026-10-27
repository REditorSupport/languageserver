discovery_limits_settings <- function(...) {
    values <- utils::modifyList(list(index_mode = "auto", index_persistent_cache = FALSE,
            index_include = "**/*.R", index_exclude = character(), index_max_files = 10000L,
            index_batch_size = 20L, index_time_budget_ms = Inf), list(...))
    old <- stats::setNames(lapply(names(values), lsp_settings$get), names(values))
    withr::defer(for (name in names(old)) lsp_settings$set(name, old[[name]]), envir = parent.frame())
    for (name in names(values)) lsp_settings$set(name, values[[name]])
}

discovery_limits_manager <- function() {
    manager <- new.env(parent = emptyenv())
    manager$tasks <- collections::ordered_dict()
    manager$add_task <- function(id, task) {
        if (is.null(task)) manager$tasks$pop(id, NULL) else manager$tasks$set(id, task)
    }
    manager$cancel <- function(id) manager$tasks$pop(id, NULL)
    manager$promote <- function(...) NULL
    manager$has <- function(id) manager$tasks$has(id)
    manager$finish <- function() {
        if (!manager$tasks$size()) return(invisible(NULL))
        task <- manager$tasks$pop(manager$tasks$keys()[[1L]])$.__enclos_env__$private
        result <- do.call(task$target, task$args)
        if (is.function(task$callback)) task$callback(result)
        invisible(NULL)
    }
    manager
}

DiscoveryLimitsServer <- R6::R6Class("DiscoveryLimitsServer", inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        replies = NULL,
        initialize = function(workspace) {
            self$workspaces <- collections::dict()
            self$workspaces$set(DEFAULT_WORKSPACE, workspace)
            self$workspace_cache <- collections::dict()
            self$index_task_manager <- discovery_limits_manager()
            self$parse_task_manager <- discovery_limits_manager()
            self$resolve_task_manager <- discovery_limits_manager()
            self$pending_replies <- collections::dict()
            self$replies <- list()
            self$request_handlers <- list(`textDocument/completion` = text_document_completion)
        },
        deliver = function(response, ...) {
            if (!is.null(response$id)) self$replies[[length(self$replies) + 1L]] <- response
        }
    ))

discovery_limits_server <- function(root) {
    workspace <- testthat::with_mocked_bindings(Workspace$new(root),
        workspace_startup_packages = function() startup_packages,
        workspace_startup_s3 = function() list(), .package = "languageserver")
    server <- DiscoveryLimitsServer$new(workspace)
    server$load_workspace(workspace)
    server
}

discovery_limits_open <- function(server, path) {
    workspace <- server$get_workspace(path_to_uri(path))
    document <- Document$new(path_to_uri(path), "r", 1L, stringi::stri_read_lines(path))
    document$did_open()
    workspace$documents$set(document$uri, document)
    server$text_sync(document$uri, document, parse = TRUE)
    document
}

discovery_limits_drain <- function(server) {
    for (i in seq_len(500L)) {
        for (workspace in server$workspaces$values()) workspace$index$process_batch()
        server$process_workspace_documents(time_budget_ms = Inf)
        server$index_task_manager$finish()
        server$parse_task_manager$finish()
        server$process_context_replies()
        server$process_pending_replies(time_budget_ms = Inf)
        if (!server$index_task_manager$tasks$size() && !server$parse_task_manager$tasks$size() &&
                all(vapply(server$workspaces$values(), function(ws) ws$index$complete(), logical(1L)))) {
            return(invisible(NULL))
        }
    }
    stop("package discovery did not settle")
}

test_that("prioritized and activated package roots respect directory exclusions", {
    discovery_limits_settings(index_mode = "full", index_exclude = "R/")
    root <- withr::local_tempdir()
    dir.create(file.path(root, "R"))
    writeLines("Package: excludedfixture", file.path(root, "DESCRIPTION"))
    writeLines("peer <- function(x) x", file.path(root, "R", "peer.R"))
    writeLines("main <- 1", file.path(root, "main.R"))
    index <- WorkspaceIndex$new(root)
    index$discover()
    expect_identical(unlist(index$files$keys(), use.names = FALSE), path_to_uri(file.path(root, "main.R")))
    server <- discovery_limits_server(root)
    discovery_limits_drain(server)
    workspace <- server$get_workspace(path_to_uri(file.path(root, "main.R")))
    expect_identical(workspace$index$files$keys(), index$files$keys())
    expect_false(workspace$index$has_definition("peer"))

    lsp_settings$set("index_mode", "auto")
    lsp_settings$set("index_exclude", "excluded/")
    outer <- withr::local_tempdir()
    nested <- file.path(outer, "excluded", "package")
    dir.create(file.path(nested, "R"), recursive = TRUE)
    writeLines("Package: nestedfixture", file.path(nested, "DESCRIPTION"))
    open_path <- file.path(nested, "R", "open.R")
    writeLines("opened <- function(x) x", open_path)
    writeLines("excluded_peer <- function(x) x", file.path(nested, "R", "peer.R"))
    server <- discovery_limits_server(outer)
    document <- discovery_limits_open(server, open_path)
    discovery_limits_drain(server)
    workspace <- server$get_workspace(document$uri)
    expect_true(workspace$index$is_lazy())
    expect_false(workspace$index$watches_package(nested))
    expect_identical(unlist(workspace$index$files$keys(), use.names = FALSE), document$uri)
    expect_true(workspace$index$has_definition("opened"))
    expect_false(workspace$index$has_definition("excluded_peer"))

    lsp_settings$set("index_exclude", "**/R/")
    server <- discovery_limits_server(outer)
    document <- discovery_limits_open(server, open_path)
    discovery_limits_drain(server)
    workspace <- server$get_workspace(document$uri)
    expect_false(workspace$index$watches_package(nested))
    expect_identical(unlist(workspace$index$files$keys(), use.names = FALSE), document$uri)
    expect_false(workspace$index$has_definition("excluded_peer"))
})

test_that("lazy package activations share a quota and preserve required files", {
    discovery_limits_settings(index_max_files = 2L)
    root <- withr::local_tempdir()
    for (package in c("one", "two")) {
        directory <- file.path(root, package, "R")
        dir.create(directory, recursive = TRUE)
        writeLines(paste0("Package: ", package), file.path(dirname(directory), "DESCRIPTION"))
        writeLines("opened <- function(x) x", file.path(directory, "open.R"))
        for (i in seq_len(4L)) writeLines(paste0("peer", i, " <- function(x) x"),
            file.path(directory, paste0("peer", i, ".R")))
    }
    server <- discovery_limits_server(root)
    first <- discovery_limits_open(server, file.path(root, "one", "R", "open.R"))
    discovery_limits_drain(server)
    workspace <- server$get_workspace(first$uri)
    expect_equal(workspace$index$files$size(), 3L)
    expect_true(workspace$index$truncated)
    second <- discovery_limits_open(server, file.path(root, "two", "R", "open.R"))
    discovery_limits_drain(server)
    expect_equal(workspace$index$files$size(), 4L)
    expect_setequal(setdiff(workspace$index$files$keys(), c(first$uri, second$uri)),
        vapply(file.path(root, "one", "R", c("peer1.R", "peer2.R")), path_to_uri, ""))
    expect_true(workspace$index$truncated)
    expect_true(workspace$index$complete())
    expect_true(workspace$index$has_definition("opened", second$uri))

    # Refreshing an already discovered root does not charge its entries again.
    workspace$index$discover(full = FALSE, directory = file.path(root, "one", "R"))
    discovery_limits_drain(server)
    expect_equal(workspace$index$files$size(), 4L)
    expect_true(workspace$index$truncated)

    dependency <- file.path(root, "dependency.R")
    writeLines("required_dependency <- function(value) value", dependency)
    second$set_content(2L, "source('../../dependency.R')")
    server$text_sync(second$uri, second, parse = TRUE)
    discovery_limits_drain(server)
    expect_true(workspace$index$has_definition("required_dependency"))
    expect_equal(workspace$index$files$size(), 5L)
    expect_true(workspace$index$truncated)

    removed <- file.path(root, "one", "R", "peer1.R")
    unlink(removed)
    workspace_did_change_watched_files(server, list(changes = list(
        list(uri = path_to_uri(removed), type = FileChangeType$Deleted))))
    workspace$index$discover(full = FALSE, directory = file.path(root, "two", "R"))
    discovery_limits_drain(server)
    expect_false(workspace$index$files$has(path_to_uri(removed)))
    expect_true(workspace$index$files$has(path_to_uri(file.path(root, "two", "R", "peer1.R"))))
    expect_equal(workspace$index$files$size(), 5L)
    expect_identical(workspace$index$summaries$get(second$uri)$content_hash, get_content_hash(second$content))
    expect_true(fs::path_norm(dependency) %in% fs::path_norm(
        vapply(workspace$index$source_closure(second$uri), path_from_uri, "")))

    # A fresh complete scan can repair the incomplete catalog and clear the flag.
    lsp_settings$set("index_max_files", 100L)
    workspace$index$discover()
    discovery_limits_drain(server)
    expect_false(workspace$index$truncated)
    expect_true(workspace$index$complete())
})

test_that("opening and closing a discovered peer does not recycle its quota slot", {
    discovery_limits_settings(index_max_files = 2L)
    root <- withr::local_tempdir()
    for (package in c("one", "two")) {
        directory <- file.path(root, package, "R")
        dir.create(directory, recursive = TRUE)
        writeLines(paste0("Package: ", package), file.path(dirname(directory), "DESCRIPTION"))
        for (file in c("open", "peer1", "peer2")) writeLines(paste0(file, " <- function(x) x"),
            file.path(directory, paste0(file, ".R")))
    }
    server <- discovery_limits_server(root)
    first <- discovery_limits_open(server, file.path(root, "one", "R", "open.R"))
    discovery_limits_drain(server)
    peer <- discovery_limits_open(server, file.path(root, "one", "R", "peer1.R"))
    discovery_limits_drain(server)
    second <- discovery_limits_open(server, file.path(root, "two", "R", "open.R"))
    discovery_limits_drain(server)
    text_document_did_close(server, list(textDocument = list(uri = peer$uri)))
    discovery_limits_drain(server)
    workspace <- server$get_workspace(first$uri)
    workspace$index$discover(full = FALSE, directory = file.path(root, "two", "R"))
    discovery_limits_drain(server)
    expect_equal(workspace$index$files$size(), 4L)
    expect_setequal(setdiff(workspace$index$files$keys(), c(first$uri, second$uri)),
        vapply(file.path(root, "one", "R", c("peer1.R", "peer2.R")), path_to_uri, ""))
    expect_false(workspace$index$files$has(path_to_uri(file.path(root, "two", "R", "peer1.R"))))
    expect_true(workspace$index$truncated)
})

test_that("required cyclic source chains and stars remain complete beyond the discovery quota", {
    discovery_limits_settings()
    for (cap in c(1L, 2L)) for (shape in c("chain", "star")) {
        lsp_settings$set("index_max_files", cap)
        root <- withr::local_tempdir()
        filenames <- c("entry.R", paste0("node", seq_len(6L), ".R"))
        paths <- file.path(root, filenames)
        sources <- if (shape == "chain") {
            "source('node1.R')"
        } else {
            paste0("source('", filenames[-1L], "')")
        }
        writeLines(c(sources, "deep_quota_"), paths[[1L]])
        for (i in seq_len(6L)) {
            dependency <- if (i == 6L) {
                "entry.R"
            } else if (shape == "chain") {
                filenames[[i + 2L]]
            } else {
                "node6.R"
            }
            writeLines(c(paste0("source('", dependency, "')"),
                    paste0("deep_quota_", i, " <- function(value) value")), paths[[i + 1L]])
        }
        server <- discovery_limits_server(root)
        document <- discovery_limits_open(server, paths[[1L]])
        text_document_completion(server, 1L, list(textDocument = list(uri = document$uri),
                position = position(length(sources), nchar("deep_quota_"))))
        discovery_limits_drain(server)
        workspace <- server$get_workspace(document$uri)
        server$prune_index_documents(workspace)
        expect_equal(workspace$index$files$size(), length(paths))
        expect_equal(workspace$documents$size(), length(paths))
        expect_length(server$replies, 1L)
        labels <- vapply(server$replies[[1L]]$result$items, `[[`, "", "label")
        expect_true(all(paste0("deep_quota_", seq_len(6L)) %in% labels))

        closure <- workspace$index$source_closure(document$uri)
        expect_identical(as.character(fs::path_norm(vapply(closure, path_from_uri, ""))),
            as.character(fs::path_norm(paths)))
        expect_identical(workspace$index$source_closure(document$uri), closure)
        dependents <- workspace$index$dependent_closure(path_to_uri(paths[[7L]]))
        expect_setequal(fs::path_norm(vapply(dependents, path_from_uri, "")), fs::path_norm(paths))
        expect_identical(workspace$index$dependent_closure(path_to_uri(paths[[7L]])), dependents)

        document$set_content(2L, c("local_only <- 1", "deep_quota_"))
        server$text_sync(document$uri, document, parse = TRUE)
        discovery_limits_drain(server)
        server$prune_index_documents(workspace)
        expect_equal(workspace$index$files$size(), 1L)
        expect_equal(workspace$documents$size(), 1L)
        expect_length(workspace$index$source_closure(document$uri), 1L)
        expect_length(workspace$index$dependent_closure(document$uri), 1L)
        text_document_completion(server, 2L, list(textDocument = list(uri = document$uri),
                position = position(1L, nchar("deep_quota_"))))
        labels <- vapply(tail(server$replies, 1L)[[1L]]$result$items, `[[`, "", "label")
        expect_false(any(paste0("deep_quota_", seq_len(6L)) %in% labels))
    }
})
