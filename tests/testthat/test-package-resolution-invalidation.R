invalidation_workspace <- function(root) {
    workspace <- new.env(parent = emptyenv())
    workspace$root <- file.path(root, "workspace")
    dir.create(workspace$root, recursive = TRUE)
    workspace$documents <- collections::dict()
    workspace$index <- list(enabled = FALSE, finish_document = function(...) NULL)
    workspace$member_metadata <- MemberMetadataCache$new(16 * 1024^2)
    workspace$s3_metadata <- S3MetadataCache$new()
    workspace$startup_member_packages <- character()
    workspace$type_hierarchy_cache <- collections::dict()
    workspace$applied <- list()
    workspace$update_parse_data <- function(uri, parsed) {
        workspace$documents$get(uri)$update_parse_data(parsed)
    }
    workspace$load_packages <- function(packages) {
        workspace$applied[[length(workspace$applied) + 1L]] <- packages
    }
    workspace$update_loaded_packages <- function() NULL
    workspace
}

invalidation_fixture <- function(members = FALSE, s3 = FALSE,
    .local_envir = parent.frame()) {
    previous <- stats::setNames(lapply(c("member_completion", "s3_dispatch"), lsp_settings$get),
        c("member_completion", "s3_dispatch"))
    withr::defer(for (key in names(previous)) lsp_settings$set(key, previous[[key]]),
        envir = .local_envir)
    withr::local_envvar(R_COVR = "false", .local_envir = .local_envir)
    lsp_settings$set("member_completion", members)
    lsp_settings$set("s3_dispatch", s3)

    state <- new.env(parent = emptyenv())
    state$processes <- list()
    state$installation <- "original-installation"
    testthat::local_mocked_bindings(detectCores = function(...) 32L,
        .package = "parallel", .env = .local_envir)
    testthat::local_mocked_bindings(
        package_resolution_installations = function(packages, ...) {
            list(key = state$installation, complete = TRUE,
                packages = unique(c(packages, "R6")))
        },
        diagnostics_callback = function(...) NULL,
        .package = "languageserver", .env = .local_envir)
    testthat::local_mocked_bindings(r_bg = function(func, args, ...) {
        process <- new.env(parent = emptyenv())
        process$args <- args
        process$installation <- state$installation
        process$ready <- FALSE
        process$killed <- 0L
        process$result <- NULL
        process$error <- NULL
        process$is_alive <- function() !process$ready && process$killed == 0L
        process$get_result <- function() {
            if (!is.null(process$error)) stop(process$error)
            process$result
        }
        process$kill <- function(...) process$killed <- process$killed + 1L
        state$processes[[length(state$processes) + 1L]] <- process
        process
    }, .package = "callr", .env = .local_envir)

    root <- withr::local_tempdir(.local_envir = .local_envir)
    workspace <- invalidation_workspace(root)

    server <- new.env(parent = emptyenv())
    server$get_workspace <- function(...) workspace
    server$workspaces <- collections::dict()
    server$workspaces$set(path_to_uri(workspace$root), workspace)
    server$pending_replies <- collections::dict()
    server$queue_pending_replies <- function(...) NULL
    server$deliver <- function(...) NULL
    server$parse_task_manager <- list(cancel = function(...) NULL)
    server$resolve_task_manager <- SharedTaskManager$new("reuse-resolve",
        max_running_tasks = 32L, cpu_load = 1)
    if (members) server$member_task_manager <- SharedTaskManager$new("reuse-members",
        max_running_tasks = 32L, cpu_load = 1)
    withr::defer({
        server$resolve_task_manager$stop()
        if (!is.null(server$member_task_manager)) server$member_task_manager$stop()
    }, envir = .local_envir)

    list(server = server, workspace = workspace, state = state, root = root)
}

invalidation_document <- function(fixture, name, packages = "callr",
    namespace_packages = character(), version = 1L) {
    uri <- path_to_uri(file.path(fixture$root, paste0(".vdoc.", name, ".r")))
    content <- c(paste0("library(", packages, ")"),
        if (length(namespace_packages)) paste0(namespace_packages, "::value"), "value <- 1L")
    document <- Document$new(uri, "r", version, content)
    document$did_open()
    fixture$workspace$documents$set(uri, document)
    fixture$server$pending_replies$set(uri, list())
    invalidation_parse(fixture, document)
    document
}

invalidation_parse <- function(fixture, document) {
    parsed <- parse_document(document$uri, document$content, include_xml = FALSE)
    parse_callback(fixture$server, document$uri, document$version, parsed, cache = FALSE)
}

invalidation_start <- function(fixture) {
    fixture$server$resolve_task_manager$run_tasks()
    if (!is.null(fixture$server$member_task_manager)) fixture$server$member_task_manager$run_tasks()
}

invalidation_result <- function(process) {
    args <- process$args
    packages <- unique(c(startup_packages, "R6", args$pkgs))
    members <- list()
    if (isTRUE(args$prepare)) {
        for (package in unique(c(setdiff(packages, startup_packages), args$namespace_packages))) {
            index <- member_generic_index("")
            index$package <- package
            index$identity <- list(package = package, installation = process$installation)
            members[package] <- list(member_index_freeze(index))
        }
    }
    catalogs <- if (isTRUE(args$prepare_s3)) {
        names <- unique(c(packages, args$namespace_packages))
        stats::setNames(lapply(names, function(package) {
            list(schema = 1L, package = package, generation = process$installation,
                functions = list(), objects = list(), methods = list(), dependencies = character())
        }), names)
    } else {
        NULL
    }
    list(packages = packages, members = members, s3 = catalogs,
        requested = list(packages = normalize_package_request(args$pkgs),
            namespace_packages = normalize_package_request(unique(args$namespace_packages))))
}

invalidation_finish <- function(fixture, processes = fixture$state$processes, error = NULL,
    transform = function(result, process) result) {
    for (process in processes) {
        process$result <- transform(invalidation_result(process), process)
        process$error <- error
        process$ready <- TRUE
    }
    fixture$server$resolve_task_manager$check_tasks()
    if (!is.null(fixture$server$member_task_manager)) fixture$server$member_task_manager$check_tasks()
}

invalidation_close <- function(fixture, document) {
    text_document_did_close(fixture$server, list(textDocument = list(uri = document$uri)))
}


invalidation_pending <- function(fixture) {
    Filter(function(process) !process$ready && process$killed == 0L, fixture$state$processes)
}

test_that("Queued and running installation changes recover attachment resolution", {
    for (stage in c("queued", "running")) {
        fixture <- invalidation_fixture()
        document <- invalidation_document(fixture, stage)
        if (stage == "running") invalidation_start(fixture)
        fixture$state$installation <- "reinstalled-package"
        if (stage == "queued") invalidation_start(fixture)
        original <- fixture$state$processes
        readiness <- logical()
        fixture$server$queue_pending_replies <- function(uri) {
            readiness <<- c(readiness, fixture$server$resolve_task_manager$has(uri))
        }
        invalidation_finish(fixture, original)
        expect_true(length(readiness) > 0L && all(readiness), info = stage)
        expect_equal(length(fixture$workspace$applied), 0L, info = stage)
        expect_true(fixture$server$resolve_task_manager$has(document$uri), info = stage)
        replacement <- fixture$server$resolve_task_manager$get_task(document$uri)
        # Unchanged imports must retain the newly queued replacement.
        invalidation_parse(fixture, document)
        expect_identical(fixture$server$resolve_task_manager$get_task(document$uri), replacement, info = stage)
        invalidation_start(fixture)
        expect_equal(length(fixture$state$processes), 2L, info = stage)
        invalidation_finish(fixture, invalidation_pending(fixture))
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"), info = stage)
        expect_false(fixture$server$resolve_task_manager$has(document$uri), info = stage)
    }
})

test_that("Installation invalidation preserves a pending member refresh successor", {
    fixture <- invalidation_fixture(members = TRUE)
    document <- invalidation_document(fixture, "member-refresh")
    invalidation_start(fixture)
    original <- fixture$state$processes
    old_member <- fixture$server$member_task_manager$get_task(document$uri)
    document$member_refresh <- TRUE
    invalidation_parse(fixture, document)
    expect_true(old_member$context$refresh_pending)
    fixture$state$installation <- "reinstalled-package"
    invalidation_finish(fixture, original)
    expect_length(fixture$workspace$applied, 0L)
    expect_true(fixture$server$resolve_task_manager$has(document$uri))
    expect_true(fixture$server$member_task_manager$has(document$uri))
    replacement <- fixture$server$member_task_manager$get_task(document$uri)
    expect_false(identical(replacement, old_member))
    expect_false(replacement$context$refresh_pending)
    invalidation_start(fixture)
    expect_length(fixture$state$processes, 4L)
    invalidation_finish(fixture, invalidation_pending(fixture))
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
    expect_identical(fixture$workspace$member_metadata$catalog("callr")$identity$installation,
        "reinstalled-package")
    expect_false(fixture$server$resolve_task_manager$has(document$uri))
    expect_false(fixture$server$member_task_manager$has(document$uri))
})

test_that("Invalidated shared roles coalesce replacements across subscribing documents", {
    fixture <- invalidation_fixture(members = TRUE, s3 = TRUE)
    documents <- lapply(seq_len(5L), function(i) invalidation_document(fixture, i))
    invalidation_start(fixture)
    original <- fixture$state$processes
    expect_length(original, 2L)
    fixture$state$installation <- "reinstalled-package"
    invalidation_finish(fixture, original)
    expect_length(fixture$workspace$applied, 0L)
    for (document in documents) {
        expect_true(fixture$server$resolve_task_manager$has(document$uri))
        expect_true(fixture$server$member_task_manager$has(document$uri))
    }
    invalidation_start(fixture)
    expect_length(fixture$state$processes, 4L)
    invalidation_finish(fixture, invalidation_pending(fixture))
    for (document in documents) {
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
        expect_false(fixture$server$resolve_task_manager$has(document$uri))
        expect_false(fixture$server$member_task_manager$has(document$uri))
    }
    expect_identical(fixture$workspace$member_metadata$catalog("callr")$identity$installation,
        "reinstalled-package")
    expect_identical(fixture$workspace$s3_metadata$catalog("callr")$generation, "reinstalled-package")
})

test_that("Invalidation does not restart closed, replaced, transferred, or changed requests", {
    for (change in c("closed", "reopened", "transferred", "request")) {
        fixture <- invalidation_fixture()
        document <- invalidation_document(fixture, change)
        invalidation_start(fixture)
        original <- fixture$state$processes
        old_task <- fixture$server$resolve_task_manager$get_task(document$uri)
        if (change == "closed") invalidation_close(fixture, document)
        if (change == "reopened") {
            replacement <- Document$new(document$uri, "r", 1L, document$content)
            replacement$did_open()
            replacement$requested_packages <- document$requested_packages
            fixture$workspace$documents$set(document$uri, replacement)
        }
        if (change == "transferred") {
            destination <- invalidation_workspace(file.path(fixture$root, "destination"))
            fixture$workspace$documents$remove(document$uri)
            destination$documents$set(document$uri, document)
            fixture$server$get_workspace <- function(...) destination
        }
        if (change == "request") document$requested_packages <- list(packages = "ps", namespace_packages = character())
        fixture$state$installation <- "reinstalled-package"
        # Include a late callback from a worker whose cancellation raced its reply.
        if (change == "closed") {
            old_task$.__enclos_env__$private$callback(invalidation_result(original[[1L]]))
        }
        invalidation_finish(fixture, original)
        invalidation_start(fixture)
        expect_equal(length(fixture$state$processes), 1L, info = change)
        expect_false(fixture$server$resolve_task_manager$has(document$uri), info = change)
        expect_equal(length(fixture$workspace$applied), 0L, info = change)
    }
})

test_that("Late invalidation replies cannot replace an already queued current request", {
    fixture <- invalidation_fixture(members = TRUE, s3 = TRUE)
    document <- invalidation_document(fixture, "changed-before-reply")
    invalidation_start(fixture)
    original <- fixture$state$processes
    old_tasks <- list(fixture$server$resolve_task_manager$get_task(document$uri),
        fixture$server$member_task_manager$get_task(document$uri))
    fixture$state$installation <- "reinstalled-package"
    document$set_content(2L, c("library(ps)", "value <- 2L"))
    invalidation_parse(fixture, document)
    current_resolve <- fixture$server$resolve_task_manager$get_task(document$uri)
    current_member <- fixture$server$member_task_manager$get_task(document$uri)
    for (i in seq_along(old_tasks)) {
        old_tasks[[i]]$.__enclos_env__$private$callback(invalidation_result(original[[i]]))
    }
    expect_identical(fixture$server$resolve_task_manager$get_task(document$uri), current_resolve)
    expect_identical(fixture$server$member_task_manager$get_task(document$uri), current_member)
    expect_length(fixture$workspace$applied, 0L)
    invalidation_start(fixture)
    expect_length(fixture$state$processes, 4L)
    invalidation_finish(fixture, invalidation_pending(fixture))
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "ps"))
    expect_identical(fixture$workspace$member_metadata$catalog("ps")$identity$installation,
        "reinstalled-package")
    expect_false(fixture$server$resolve_task_manager$has(document$uri))
    expect_false(fixture$server$member_task_manager$has(document$uri))
})


test_that("Invalidation defers replacement until pending or failed parses recover", {
    for (parse_state in c("pending", "error", "missing")) {
        fixture <- invalidation_fixture(members = TRUE, s3 = TRUE)
        document <- invalidation_document(fixture, parse_state)
        invalidation_start(fixture)
        original <- fixture$state$processes
        if (parse_state == "pending") document$set_content(2L, c("library(callr)", "value <- 2L"))
        if (parse_state == "error") {
            document$set_content(2L, c("library(callr)", "value <-"))
            invalidation_parse(fixture, document)
            expect_true(document$parse_data$parse_error)
        }
        if (parse_state == "missing") document$parse_data <- NULL
        fixture$state$installation <- "reinstalled-package"
        invalidation_finish(fixture, original)
        expect_true(document$member_refresh, info = parse_state)
        expect_false(fixture$server$resolve_task_manager$has(document$uri), info = parse_state)
        expect_false(fixture$server$member_task_manager$has(document$uri), info = parse_state)
        document$set_content(3L, c("library(callr)", "value <- 3L"))
        invalidation_parse(fixture, document)
        expect_true(fixture$server$resolve_task_manager$has(document$uri), info = parse_state)
        expect_true(fixture$server$member_task_manager$has(document$uri), info = parse_state)
        invalidation_start(fixture)
        expect_equal(length(fixture$state$processes), 4L, info = parse_state)
        invalidation_finish(fixture, invalidation_pending(fixture))
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"), info = parse_state)
        expect_false(document$member_refresh, info = parse_state)
    }
})

test_that("Library and cache context invalidations capture current worker arguments", {
    for (context in c("libraries", "cache")) {
        fixture <- invalidation_fixture(members = TRUE, s3 = TRUE)
        document <- invalidation_document(fixture, context)
        invalidation_start(fixture)
        original <- fixture$state$processes
        path <- withr::local_tempdir()
        if (context == "libraries") {
            previous <- .libPaths()
            withr::defer(.libPaths(previous))
            .libPaths(c(path, previous))
        } else {
            withr::local_options(languageserver.package_cache_dir = path)
        }
        invalidation_finish(fixture, original)
        expect_true(fixture$server$resolve_task_manager$has(document$uri), info = context)
        expect_true(fixture$server$member_task_manager$has(document$uri), info = context)
        invalidation_start(fixture)
        expect_equal(length(fixture$state$processes), 4L, info = context)
        pending <- invalidation_pending(fixture)
        for (process in pending) {
            if (context == "libraries") expect_identical(process$args$lib_paths, .libPaths())
            else expect_identical(process$args$cache_dir, path)
        }
        invalidation_finish(fixture, pending)
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"), info = context)
    }
})

test_that("Invalidation replaces obsolete metadata preparation using current settings", {
    fixture <- invalidation_fixture(members = TRUE, s3 = TRUE)
    document <- invalidation_document(fixture, "settings")
    invalidation_start(fixture)
    original <- fixture$state$processes
    lsp_settings$set("member_completion", FALSE)
    lsp_settings$set("s3_dispatch", FALSE)
    invalidation_finish(fixture, original)
    expect_true(fixture$server$resolve_task_manager$has(document$uri))
    expect_false(fixture$server$member_task_manager$has(document$uri))
    invalidation_start(fixture)
    expect_length(fixture$state$processes, 3L)
    pending <- invalidation_pending(fixture)
    expect_length(pending, 1L)
    expect_false(pending[[1L]]$args$prepare)
    expect_false(pending[[1L]]$args$prepare_s3)
    invalidation_finish(fixture, pending)
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
})
