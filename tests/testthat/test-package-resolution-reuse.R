package_reuse_workspace <- function(root) {
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

package_reuse_fixture <- function(members = FALSE, s3 = FALSE,
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
    workspace <- package_reuse_workspace(root)

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

package_reuse_document <- function(fixture, name, packages = "callr",
    namespace_packages = character(), version = 1L) {
    uri <- path_to_uri(file.path(fixture$root, paste0(".vdoc.", name, ".r")))
    content <- c(paste0("library(", packages, ")"),
        if (length(namespace_packages)) paste0(namespace_packages, "::value"), "value <- 1L")
    document <- Document$new(uri, "r", version, content)
    document$did_open()
    fixture$workspace$documents$set(uri, document)
    fixture$server$pending_replies$set(uri, list())
    package_reuse_parse(fixture, document)
    document
}

package_reuse_parse <- function(fixture, document) {
    parsed <- parse_document(document$uri, document$content, include_xml = FALSE)
    parse_callback(fixture$server, document$uri, document$version, parsed, cache = FALSE)
}

package_reuse_start <- function(fixture) {
    fixture$server$resolve_task_manager$run_tasks()
    if (!is.null(fixture$server$member_task_manager)) fixture$server$member_task_manager$run_tasks()
}

package_reuse_result <- function(process) {
    args <- process$args
    packages <- unique(c(startup_packages, "R6", args$pkgs))
    members <- list()
    if (isTRUE(args$prepare)) {
        for (package in unique(c(setdiff(packages, startup_packages), args$namespace_packages))) {
            index <- member_generic_index("")
            index$package <- package
            index$identity <- list(package = package, revision = 1L)
            members[package] <- list(member_index_freeze(index))
        }
    }
    catalogs <- if (isTRUE(args$prepare_s3)) {
        names <- unique(c(packages, args$namespace_packages))
        stats::setNames(lapply(names, function(package) {
            list(schema = 1L, package = package, generation = "fixture",
                functions = list(), objects = list(), methods = list(), dependencies = character())
        }), names)
    } else {
        NULL
    }
    list(packages = packages, members = members, s3 = catalogs,
        requested = list(packages = normalize_package_request(args$pkgs),
            namespace_packages = normalize_package_request(unique(args$namespace_packages))))
}

package_reuse_finish <- function(fixture, processes = fixture$state$processes, error = NULL,
    transform = function(result, process) result) {
    for (process in processes) {
        process$result <- transform(package_reuse_result(process), process)
        process$error <- error
        process$ready <- TRUE
    }
    fixture$server$resolve_task_manager$check_tasks()
    if (!is.null(fixture$server$member_task_manager)) fixture$server$member_task_manager$check_tasks()
}

package_reuse_close <- function(fixture, document) {
    text_document_did_close(fixture$server, list(textDocument = list(uri = document$uri)))
}

test_that("Equivalent fresh documents share resolution even with metadata features disabled", {
    fixture <- package_reuse_fixture()
    documents <- lapply(seq_len(12L), function(i) package_reuse_document(fixture, i))
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 1L)
    expect_false(fixture$state$processes[[1L]]$args$prepare)
    expect_false(fixture$state$processes[[1L]]$args$prepare_s3)
    package_reuse_finish(fixture)
    expected <- c(startup_packages, "R6", "callr")
    for (document in documents) {
        expect_identical(document$loaded_packages, expected)
        expect_false(fixture$server$resolve_task_manager$has(document$uri))
    }
    cached <- package_reuse_document(fixture, "cached")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 1L)
    expect_identical(cached$loaded_packages, expected)
})

test_that("Split member and S3 workers each share across fresh documents", {
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    documents <- lapply(seq_len(8L), function(i) {
        package_reuse_document(fixture, i, namespace_packages = "processx")
    })
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    expect_identical(vapply(fixture$state$processes, function(process) process$args$prepare, logical(1L)),
        c(FALSE, TRUE))
    expect_identical(vapply(fixture$state$processes, function(process) process$args$prepare_s3, logical(1L)),
        c(TRUE, FALSE))
    package_reuse_finish(fixture)
    for (document in documents) {
        expect_true("R6" %in% document$loaded_packages)
        expect_false(fixture$server$resolve_task_manager$has(document$uri))
        expect_false(fixture$server$member_task_manager$has(document$uri))
    }
    expect_false(is.null(fixture$workspace$member_metadata$catalog("processx")))
    expect_false(is.null(fixture$workspace$s3_metadata$catalog("processx")))
    cached <- package_reuse_document(fixture, "cached", namespace_packages = "processx")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    expect_true("R6" %in% cached$loaded_packages)
})

test_that("Attachment order and namespace requests keep distinct resolution jobs", {
    fixture <- package_reuse_fixture()
    first <- package_reuse_document(fixture, "first", c("callr", "ps"))
    second <- package_reuse_document(fixture, "second", c("ps", "callr"))
    third <- package_reuse_document(fixture, "third", c("callr", "ps"), "processx")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    package_reuse_finish(fixture)
    expect_identical(tail(first$loaded_packages, 2L), c("callr", "ps"))
    expect_identical(tail(second$loaded_packages, 2L), c("ps", "callr"))
    expect_identical(third$requested_packages$namespace_packages, "processx")
})

test_that("Closing one subscriber preserves shared work and closing the last cancels it", {
    for (running in c(FALSE, TRUE)) {
        fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
        first <- package_reuse_document(fixture, "first")
        second <- package_reuse_document(fixture, "second")
        if (running) package_reuse_start(fixture)
        package_reuse_close(fixture, first)
        expect_false(fixture$server$resolve_task_manager$has(first$uri))
        expect_false(fixture$server$member_task_manager$has(first$uri))
        expect_true(fixture$server$resolve_task_manager$has(second$uri))
        expect_true(fixture$server$member_task_manager$has(second$uri))
        expect_true(all(vapply(fixture$state$processes, function(process) process$killed == 0L, logical(1L))))
        package_reuse_close(fixture, second)
        expect_false(fixture$server$resolve_task_manager$has(second$uri))
        expect_false(fixture$server$member_task_manager$has(second$uri))
        if (running) {
            expect_true(all(vapply(fixture$state$processes, function(process) process$killed == 1L, logical(1L))))
        } else {
            package_reuse_start(fixture)
            expect_length(fixture$state$processes, 0L)
        }
    }
})

test_that("Old shared replies cannot update changed or reopened documents", {
    fixture <- package_reuse_fixture()
    changed <- package_reuse_document(fixture, "changed")
    reopened <- package_reuse_document(fixture, "reopened")
    peer <- package_reuse_document(fixture, "peer")
    package_reuse_start(fixture)
    old_processes <- fixture$state$processes
    changed$set_content(2L, c("library(ps)", "value <- 2L"))
    package_reuse_parse(fixture, changed)
    package_reuse_close(fixture, reopened)
    replacement <- package_reuse_document(fixture, "reopened", "ps")
    package_reuse_finish(fixture, old_processes)
    expect_identical(changed$loaded_packages, "ps")
    expect_identical(replacement$loaded_packages, "ps")
    expect_identical(peer$loaded_packages, c(startup_packages, "R6", "callr"))
    expect_identical(reopened$loaded_packages, "callr")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    package_reuse_finish(fixture, fixture$state$processes[2L])
    expect_identical(changed$loaded_packages, c(startup_packages, "R6", "ps"))
    expect_identical(replacement$loaded_packages, changed$loaded_packages)
})

test_that("Completed results invalidate on package installations and library paths", {
    fixture <- package_reuse_fixture()
    package_reuse_document(fixture, "original")
    package_reuse_start(fixture)
    package_reuse_finish(fixture)
    fixture$state$installation <- "reinstalled-package"
    package_reuse_document(fixture, "reinstalled")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    package_reuse_finish(fixture, fixture$state$processes[2L])
    library <- withr::local_tempdir()
    withr::local_libpaths(c(library, .libPaths()))
    package_reuse_document(fixture, "library")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    expect_identical(fixture$state$processes[[3L]]$args$lib_paths, .libPaths())
})

test_that("Failed shared resolutions are retried by the next fresh document", {
    fixture <- package_reuse_fixture()
    first <- package_reuse_document(fixture, "first")
    second <- package_reuse_document(fixture, "second")
    package_reuse_start(fixture)
    package_reuse_finish(fixture, error = simpleError("fixture resolution failed"))
    expect_false(fixture$server$resolve_task_manager$has(first$uri))
    expect_false(fixture$server$resolve_task_manager$has(second$uri))
    retry <- package_reuse_document(fixture, "retry")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    package_reuse_finish(fixture, fixture$state$processes[2L])
    expect_true("R6" %in% retry$loaded_packages)
})

test_that("Saving a document refreshes completed member and S3 resolution", {
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    document <- package_reuse_document(fixture, "saved")
    package_reuse_start(fixture)
    package_reuse_finish(fixture)
    expect_length(fixture$state$processes, 2L)
    document$member_refresh <- TRUE
    package_reuse_parse(fixture, document)
    expect_false(document$member_refresh)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 4L)
    package_reuse_finish(fixture, fixture$state$processes[3:4])
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
})

test_that("Repeated running member refreshes schedule one fresh successor", {
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    document <- package_reuse_document(fixture, "refresh")
    package_reuse_start(fixture)
    original <- fixture$server$member_task_manager$get_task(document$uri)
    original_processes <- fixture$state$processes
    for (i in seq_len(5L)) {
        document$set_content(i + 1L, c("library(callr)", paste0("value <- ", i)))
        document$member_refresh <- TRUE
        package_reuse_parse(fixture, document)
        expect_identical(fixture$server$member_task_manager$get_task(document$uri), original)
    }
    expect_true(original$context$refresh_pending)
    expect_true(all(vapply(original_processes, function(process) process$killed == 0L, logical(1L))))
    package_reuse_finish(fixture, original_processes)
    successor <- fixture$server$member_task_manager$get_task(document$uri)
    expect_false(is.null(successor))
    expect_false(identical(successor, original))
    expect_null(successor$execution_started_at)
    package_reuse_start(fixture)
    expect_identical(sum(vapply(fixture$state$processes, function(process) process$args$prepare, logical(1L))), 2L)
    pending <- Filter(function(process) !process$ready, fixture$state$processes)
    package_reuse_finish(fixture, pending)
    expect_false(fixture$server$member_task_manager$has(document$uri))
})

test_that("Completed shared member catalogs populate another workspace", {
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    package_reuse_document(fixture, "first", namespace_packages = "processx")
    package_reuse_start(fixture)
    package_reuse_finish(fixture)
    other <- fixture
    other$root <- file.path(fixture$root, "other")
    other$workspace <- package_reuse_workspace(other$root)
    other_uri <- path_to_uri(other$root)
    fixture$server$get_workspace <- function(uri) {
        if (startsWith(uri, other_uri)) other$workspace else fixture$workspace
    }
    document <- package_reuse_document(other, "second", namespace_packages = "processx")
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    for (package in c("R6", "callr", "processx")) {
        expect_false(is.null(other$workspace$member_metadata$catalog(package)))
        expect_false(is.null(other$workspace$s3_metadata$catalog(package)))
    }
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
})

test_that("Failed and incomplete metadata results do not satisfy later fresh documents", {
    for (failure in c("missing_member", "failed_member", "partial_member", "failed_s3", "missing_s3")) {
        fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
        package_reuse_document(fixture, "first")
        package_reuse_start(fixture)
        package_reuse_finish(fixture, transform = function(result, process) {
            if (isTRUE(process$args$prepare)) {
                if (failure == "missing_member") result$members$callr <- NULL
                if (failure == "failed_member") result$members$callr$error <- "fixture metadata failure"
                if (failure == "partial_member") result$members$callr$dataset_only <- TRUE
            }
            if (isTRUE(process$args$prepare_s3)) {
                if (failure == "failed_s3") result$s3$callr$error <- "fixture S3 failure"
                if (failure == "missing_s3") result$s3$callr <- NULL
            }
            result
        })
        package_reuse_document(fixture, "retry")
        package_reuse_start(fixture)
        expect_equal(length(fixture$state$processes), 3L, info = failure)
        expect_identical(fixture$state$processes[[3L]]$args$prepare,
            failure %in% c("missing_member", "failed_member", "partial_member"), info = failure)
        package_reuse_finish(fixture, fixture$state$processes[3L])
    }
})

test_that("Shared jobs retain provider readiness for every subscribing document", {
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    documents <- lapply(c("first", "second"), function(name) {
        document <- package_reuse_document(fixture, name, namespace_packages = "processx")
        document$set_content(2L, c("library(callr)", "processx::value$field", "value <- 1L"))
        package_reuse_parse(fixture, document)
        document
    })
    point <- list(row = 1L, col = nchar("processx::value$"))
    for (document in documents) {
        expect_true(s3_metadata_waiting(fixture$server, document$uri, document))
        expect_true(member_metadata_waiting(fixture$server, document$uri, document, point))
    }
    package_reuse_start(fixture)
    package_reuse_finish(fixture)
    for (document in documents) {
        expect_false(s3_metadata_waiting(fixture$server, document$uri, document))
        expect_false(member_metadata_waiting(fixture$server, document$uri, document, point))
    }
})

test_that("Disabling metadata during resolution restarts shared attachment resolution", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    fixture <- package_reuse_fixture(members = TRUE, s3 = TRUE)
    documents <- lapply(c("first", "second"), function(name) package_reuse_document(fixture, name))
    package_reuse_start(fixture)
    original <- fixture$state$processes
    expect_length(original, 2L)
    workspace_did_change_configuration(fixture$server,
        list(settings = list(r = list(lsp = list(member_completion = FALSE, s3_dispatch = FALSE)))))
    expect_false(lsp_settings$get("member_completion"))
    expect_false(lsp_settings$get("s3_dispatch"))
    expect_true(all(vapply(original, function(process) process$killed == 1L, logical(1L))))
    for (document in documents) {
        expect_false(fixture$server$member_task_manager$has(document$uri))
        expect_true(fixture$server$resolve_task_manager$has(document$uri))
        expect_false(document$member_refresh)
    }
    package_reuse_finish(fixture, original)
    expect_length(fixture$workspace$applied, 0L)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    process <- fixture$state$processes[[3L]]
    expect_false(process$args$prepare)
    expect_false(process$args$prepare_s3)
    package_reuse_finish(fixture, list(process))
    for (document in documents) {
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
        package_reuse_parse(fixture, document)
        expect_false(fixture$server$resolve_task_manager$has(document$uri))
    }
})

test_that("Enabling member completion prepares existing documents with S3 disabled", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    fixture <- package_reuse_fixture()
    fixture$server$member_task_manager <- SharedTaskManager$new("reuse-enabled-members",
        max_running_tasks = 32L, cpu_load = 1)
    document <- package_reuse_document(fixture, "existing", namespace_packages = "processx")
    package_reuse_start(fixture)
    package_reuse_finish(fixture)
    expect_length(fixture$state$processes, 1L)
    expect_null(fixture$workspace$member_metadata$catalog("processx"))
    workspace_did_change_configuration(fixture$server,
        list(settings = list(member_completion = TRUE)))
    expect_true(lsp_settings$get("member_completion"))
    expect_false(lsp_settings$get("s3_dispatch"))
    expect_true(fixture$server$member_task_manager$has(document$uri))
    expect_false(document$member_refresh)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    expect_true(fixture$state$processes[[3L]]$args$prepare)
    expect_false(fixture$state$processes[[3L]]$args$prepare_s3)
    package_reuse_finish(fixture, fixture$state$processes[2:3])
    expect_false(is.null(fixture$workspace$member_metadata$catalog("processx")))
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
})

package_reuse_pending_configuration <- function(initial_enabled, parse_state) {
    fixture <- package_reuse_fixture(members = initial_enabled, s3 = initial_enabled)
    if (is.null(fixture$server$member_task_manager)) {
        fixture$server$member_task_manager <- SharedTaskManager$new("reuse-pending-members",
            max_running_tasks = 32L, cpu_load = 1)
    }
    document <- package_reuse_document(fixture, "pending")
    original_content <- document$content
    original_parsed <- document$parse_data
    package_reuse_start(fixture)
    original_processes <- fixture$state$processes
    expect_equal(length(original_processes), if (initial_enabled) 2L else 1L)

    if (parse_state == "pending") {
        document$set_content(2L, c("library(callr)", "value <- 2L"))
        expect_false(identical(document$version, document$parse_data$version))
    } else if (parse_state == "error") {
        document$set_content(2L, c("library(callr)", "value <- )"))
        package_reuse_parse(fixture, document)
        expect_true(document$parse_data$parse_error)
        expect_identical(document$requested_packages$packages, "callr")
    } else {
        if (parse_state == "missing_null_version") document$version <- NULL
        document$parse_data <- NULL
    }

    enabled <- !initial_enabled
    workspace_did_change_configuration(fixture$server,
        list(settings = list(member_completion = enabled, s3_dispatch = enabled)))
    expect_identical(lsp_settings$get("member_completion"), enabled)
    expect_identical(lsp_settings$get("s3_dispatch"), enabled)
    expect_identical(document$requested_packages$packages, "callr")
    expect_true(document$member_refresh)
    expect_equal(length(fixture$state$processes), length(original_processes))

    if (parse_state == "pending") {
        parse_callback(fixture$server, document$uri, 1L, original_parsed,
            source_content = original_content, cache = FALSE)
        expect_false(identical(document$version, document$parse_data$version))
    } else if (parse_state == "error") {
        package_reuse_parse(fixture, document)
        expect_true(document$parse_data$parse_error)
    } else {
        parse_callback(fixture$server, document$uri, document$version, NULL, cache = FALSE)
        expect_null(document$parse_data)
    }
    expect_true(document$member_refresh)
    package_reuse_finish(fixture, original_processes)
    expect_length(fixture$workspace$applied, 0L)
    expect_identical(document$loaded_packages, "callr")
    expect_true(document$member_refresh)

    if (parse_state == "error") {
        document$set_content(3L, c("library(callr)", "value <- 3L"))
    }
    package_reuse_parse(fixture, document)
    expect_identical(document$requested_packages$packages, "callr")
    expect_false(document$member_refresh)
    expect_true(fixture$server$resolve_task_manager$has(document$uri))
    expect_identical(fixture$server$member_task_manager$has(document$uri), enabled)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    replacement <- Filter(function(process) !process$ready, fixture$state$processes)
    expect_identical(vapply(replacement, function(process) process$args$prepare, logical(1L)),
        if (enabled) c(FALSE, TRUE) else FALSE)
    expect_identical(vapply(replacement, function(process) process$args$prepare_s3, logical(1L)),
        if (enabled) c(TRUE, FALSE) else FALSE)

    task <- fixture$server$resolve_task_manager$get_task(document$uri)
    package_reuse_parse(fixture, document)
    expect_identical(fixture$server$resolve_task_manager$get_task(document$uri), task)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
    package_reuse_finish(fixture, replacement)
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
    expect_identical(is.null(fixture$workspace$member_metadata$catalog("callr")), !enabled)
    expect_identical(is.null(fixture$workspace$s3_metadata$catalog("callr")), !enabled)
    package_reuse_parse(fixture, document)
    expect_false(fixture$server$resolve_task_manager$has(document$uri))
    expect_false(fixture$server$member_task_manager$has(document$uri))
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 3L)
}

test_that("Metadata configuration refresh survives a pending newer document version", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    for (enabled in c(TRUE, FALSE)) package_reuse_pending_configuration(enabled, "pending")
})

test_that("Metadata configuration refresh survives a current parse error and recovery", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    for (enabled in c(TRUE, FALSE)) package_reuse_pending_configuration(enabled, "error")
})

test_that("Metadata configuration refresh waits for a missing document parse", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    for (enabled in c(TRUE, FALSE)) {
        package_reuse_pending_configuration(enabled, "missing")
        package_reuse_pending_configuration(enabled, "missing_null_version")
    }
})

test_that("Namespace recovery hints consume deferred refresh with the current preparation modes", {
    withr::local_options(languageserver.member_completion = NULL, languageserver.s3_dispatch = NULL)
    testthat::local_mocked_bindings(
        backend_settings_changed = function(...) NULL,
        sync_formatting_capabilities = function(...) NULL,
        .package = "languageserver")
    for (initial_enabled in c(TRUE, FALSE)) {
        fixture <- package_reuse_fixture(members = initial_enabled, s3 = initial_enabled)
        if (is.null(fixture$server$member_task_manager)) {
            fixture$server$member_task_manager <- SharedTaskManager$new("reuse-hint-members",
                max_running_tasks = 32L, cpu_load = 1)
        }
        document <- package_reuse_document(fixture, "hint")
        package_reuse_start(fixture)
        original_processes <- fixture$state$processes
        document$set_content(2L, c("library(callr)", "processx::value$"))
        enabled <- !initial_enabled
        workspace_did_change_configuration(fixture$server,
            list(settings = list(member_completion = enabled, s3_dispatch = enabled)))
        expect_true(document$member_refresh)
        package_reuse_parse(fixture, document)
        expect_true(document$parse_data$parse_error)
        expect_identical(document$requested_packages$packages, "callr")
        expect_identical(document$requested_packages$namespace_packages, "processx")
        expect_false(document$member_refresh)
        expect_true(fixture$server$resolve_task_manager$has(document$uri))
        expect_identical(fixture$server$member_task_manager$has(document$uri), enabled)
        package_reuse_finish(fixture, original_processes)
        expect_length(fixture$workspace$applied, 0L)
        package_reuse_start(fixture)
        expect_length(fixture$state$processes, 3L)
        replacement <- Filter(function(process) !process$ready, fixture$state$processes)
        expect_identical(vapply(replacement, function(process) process$args$prepare, logical(1L)),
            if (enabled) c(FALSE, TRUE) else FALSE)
        expect_identical(vapply(replacement, function(process) process$args$prepare_s3, logical(1L)),
            if (enabled) c(TRUE, FALSE) else FALSE)
        point <- list(row = 1L, col = nchar("processx::value$"))
        expect_identical(member_metadata_waiting(fixture$server, document$uri, document, point), enabled)
        expect_identical(s3_metadata_waiting(fixture$server, document$uri, document), enabled)

        task <- fixture$server$resolve_task_manager$get_task(document$uri)
        package_reuse_parse(fixture, document)
        expect_identical(fixture$server$resolve_task_manager$get_task(document$uri), task)
        document$set_content(3L, c("library(callr)", "processx::value$field"))
        package_reuse_parse(fixture, document)
        expect_false(document$parse_data$parse_error)
        expect_false(document$member_refresh)
        expect_identical(fixture$server$resolve_task_manager$get_task(document$uri), task)
        package_reuse_start(fixture)
        expect_length(fixture$state$processes, 3L)
        package_reuse_finish(fixture, replacement)
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
        expect_identical(is.null(fixture$workspace$member_metadata$catalog("processx")), !enabled)
        expect_identical(is.null(fixture$workspace$s3_metadata$catalog("processx")), !enabled)
        expect_false(member_metadata_waiting(fixture$server, document$uri, document, point))
        expect_false(s3_metadata_waiting(fixture$server, document$uri, document))
        package_reuse_parse(fixture, document)
        expect_false(fixture$server$resolve_task_manager$has(document$uri))
        expect_false(fixture$server$member_task_manager$has(document$uri))
        package_reuse_start(fixture)
        expect_length(fixture$state$processes, 3L)
    }
})

PackageResolutionTransferServer <- R6::R6Class("PackageResolutionTransferServer",
    inherit = LanguageServer,
    private = list(finalize = function() NULL),
    public = list(
        initialize = function(fixture) {
            self$workspaces <- collections::dict()
            self$workspace_cache <- collections::dict()
            self$pending_replies <- fixture$server$pending_replies
            self$request_callbacks <- collections::dict()
            self$resolve_task_manager <- fixture$server$resolve_task_manager
            self$member_task_manager <- fixture$server$member_task_manager
            self$parse_task_manager <- TaskManager$new("reuse-transfer-parse", cpu_load = 1,
                max_running_tasks = 1L)
            self$rootUri <- character()
            self$ClientCapabilities <- list()
            self$add_workspace(character())
        },
        deliver = function(...) invisible(NULL)
    )
)

package_reuse_transfer <- function(move, enabled, parse_state) {
    previous_diagnostics <- lsp_settings$get("diagnostics")
    withr::defer(lsp_settings$set("diagnostics", previous_diagnostics))
    lsp_settings$set("diagnostics", FALSE)
    fixture <- package_reuse_fixture(members = enabled, s3 = enabled)
    temp_root <- fixture$root
    # Keep workspace storage and parser scheduling deterministic while using
    # the server's actual ownership, transfer and metadata-resolution paths.
    testthat::local_mocked_bindings(Workspace = list(new = function(path) {
        root <- if (is.null(path)) file.path(temp_root, "default") else path
        workspace <- package_reuse_workspace(root)
        workspace$root <- path
        workspace$index <- NULL
        workspace$parse_cache <- ByteLruCache$new(16 * 1024^2)
        workspace
    }), .package = "languageserver")
    server <- PackageResolutionTransferServer$new(fixture)
    withr::defer(server$parse_task_manager$stop())
    fixture$server <- server
    parent <- file.path(temp_root, "parent")
    nested <- file.path(parent, "nested")
    dir.create(nested, recursive = TRUE)
    nested_uri <- path_to_uri(nested)
    server$add_workspace(path_to_uri(parent))
    if (move == "remove") server$add_workspace(nested_uri)
    fixture$root <- nested
    uri <- path_to_uri(file.path(nested, ".vdoc.transfer.r"))
    fixture$workspace <- server$get_workspace(uri)
    document <- package_reuse_document(fixture, "transfer")
    source <- fixture$workspace
    original_parsed <- document$parse_data
    original_content <- document$content
    package_reuse_start(fixture)
    original_processes <- fixture$state$processes
    expect_equal(length(original_processes), if (enabled) 2L else 1L)
    if (parse_state %in% c("warm", "force")) {
        package_reuse_finish(fixture, original_processes)
        expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
        if (parse_state == "force") document$member_refresh <- TRUE
    }

    if (parse_state == "pending") {
        document$set_content(2L, c("library(callr)", "value <- 2L"))
    } else if (parse_state == "error") {
        document$set_content(2L, c("library(callr)", "value <- )"))
        package_reuse_parse(fixture, document)
        expect_true(document$parse_data$parse_error)
    } else if (parse_state == "missing") {
        document$parse_data <- NULL
    } else if (parse_state == "namespace_error") {
        document$set_content(2L, c("library(callr)", "processx::value$"))
        package_reuse_parse(fixture, document)
        expect_true(document$parse_data$parse_error)
        expect_identical(document$requested_packages$namespace_packages, "processx")
    }
    source_applied <- source$applied

    if (move == "add") server$add_workspace(nested_uri)
    else server$remove_workspace(nested_uri)
    destination <- server$get_workspace(uri)
    fixture$workspace <- destination
    expect_false(identical(source, destination))
    expect_false(source$documents$has(uri))
    expect_identical(destination$documents$get(uri), document)
    expect_identical(document$requested_packages$packages, "callr")

    deferred <- parse_state %in% c("pending", "error", "missing")
    if (deferred) {
        expect_true(document$member_refresh)
        if (parse_state == "pending") {
            expect_true(server$parse_task_manager$has(uri))
            parse_callback(server, uri, 1L, original_parsed, original_content, cache = FALSE)
        } else if (parse_state == "error") {
            package_reuse_parse(fixture, document)
            expect_true(document$parse_data$parse_error)
        } else {
            expect_true(server$parse_task_manager$has(uri))
            parse_callback(server, uri, document$version, NULL, cache = FALSE)
        }
        expect_true(document$member_refresh)
        package_reuse_finish(fixture, original_processes)
        expect_length(source$applied, 0L)
        expect_length(destination$applied, 0L)
        expect_identical(document$loaded_packages, "callr")
        if (parse_state == "error") {
            document$set_content(3L, c("library(callr)", "value <- 3L"))
        }
        package_reuse_parse(fixture, document)
        expect_false(document$member_refresh)
    } else {
        # Current successful parses and accepted namespace recovery hints
        # resolve in the destination without waiting for another edit.
        expect_false(document$member_refresh)
        if (parse_state == "namespace_error") {
            expect_true(document$parse_data$parse_error)
            expect_false(server$parse_task_manager$has(uri))
            package_reuse_finish(fixture, original_processes)
            expect_length(destination$applied, 0L)
        }
    }
    expect_identical(server$resolve_task_manager$has(uri), parse_state != "warm")
    expect_identical(!is.null(server$member_task_manager) && server$member_task_manager$has(uri),
        enabled && parse_state != "warm")
    package_reuse_start(fixture)
    count <- length(fixture$state$processes)
    if (deferred || parse_state %in% c("namespace_error", "force")) {
        expect_equal(count, if (enabled) 4L else 2L)
    }
    if (parse_state == "warm") expect_equal(count, if (enabled) 2L else 1L)
    task <- server$resolve_task_manager$get_task(uri)
    package_reuse_parse(fixture, document)
    expect_identical(server$resolve_task_manager$get_task(uri), task)
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, count)
    replacement <- Filter(function(process) !process$ready, fixture$state$processes)
    package_reuse_finish(fixture, replacement)
    expect_identical(source$applied, source_applied)
    expect_true(length(destination$applied) > 0L)
    expect_identical(document$loaded_packages, c(startup_packages, "R6", "callr"))
    expect_identical(is.null(destination$member_metadata$catalog("callr")), !enabled)
    expect_identical(is.null(destination$s3_metadata$catalog("callr")), !enabled)
    if (parse_state == "namespace_error") {
        expect_false(is.null(destination$member_metadata$catalog("processx")))
        expect_false(is.null(destination$s3_metadata$catalog("processx")))
        point <- list(row = 1L, col = nchar("processx::value$"))
        expect_false(member_metadata_waiting(server, uri, document, point))
        expect_false(s3_metadata_waiting(server, uri, document))
    }
    package_reuse_parse(fixture, document)
    expect_false(server$resolve_task_manager$has(uri))
    expect_false(!is.null(server$member_task_manager) && server$member_task_manager$has(uri))
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, count)
}

test_that("Workspace transfers retain package refresh for pending document versions", {
    for (move in c("remove", "add")) {
        for (enabled in c(FALSE, TRUE)) package_reuse_transfer(move, enabled, "pending")
    }
})

test_that("Workspace transfers retain package refresh through parse error recovery", {
    for (move in c("remove", "add")) {
        for (enabled in c(FALSE, TRUE)) package_reuse_transfer(move, enabled, "error")
    }
})

test_that("Workspace transfers retain package refresh until a missing parse arrives", {
    for (move in c("remove", "add")) {
        for (enabled in c(FALSE, TRUE)) package_reuse_transfer(move, enabled, "missing")
    }
})

test_that("Workspace transfers resolve successful parses immediately without another refresh", {
    for (move in c("remove", "add")) {
        for (enabled in c(FALSE, TRUE)) package_reuse_transfer(move, enabled, "valid")
    }
})

test_that("Workspace transfers prepare accepted namespace recovery hints without another edit", {
    for (move in c("remove", "add")) package_reuse_transfer(move, TRUE, "namespace_error")
})

test_that("Workspace transfers reuse complete cached results and honor an earlier forced refresh", {
    for (move in c("remove", "add")) {
        for (enabled in c(FALSE, TRUE)) package_reuse_transfer(move, enabled, "warm")
        package_reuse_transfer(move, TRUE, "force")
    }
})

test_that("S3-only saves coalesce a fresh result after running resolution", {
    fixture <- package_reuse_fixture(s3 = TRUE)
    document <- package_reuse_document(fixture, "s3-refresh")
    package_reuse_start(fixture)
    first <- fixture$state$processes[[1L]]
    for (i in 1:3) {
        document$member_refresh <- TRUE
        package_reuse_parse(fixture, document)
    }
    expect_length(fixture$state$processes, 1L)
    expect_identical(first$killed, 0L)
    first$result <- package_reuse_result(first)
    first$result$s3$callr$generation <- "before-save"
    first$ready <- TRUE
    fixture$server$resolve_task_manager$check_tasks()
    expect_null(fixture$workspace$s3_metadata$catalog("callr"))
    expect_true(fixture$server$resolve_task_manager$has(document$uri))
    package_reuse_start(fixture)
    expect_length(fixture$state$processes, 2L)
    fresh <- fixture$state$processes[[2L]]
    expect_true(fresh$args$prepare_s3)
    expect_false(fresh$args$prepare)
    fresh$result <- package_reuse_result(fresh)
    fresh$result$s3$callr$generation <- "after-save"
    fresh$ready <- TRUE
    fixture$server$resolve_task_manager$check_tasks()
    expect_identical(fixture$workspace$s3_metadata$catalog("callr")$generation, "after-save")
    expect_false(fixture$server$resolve_task_manager$has(document$uri))
    later <- package_reuse_document(fixture, "s3-cached")
    expect_false(fixture$server$resolve_task_manager$has(later$uri))
    expect_length(fixture$state$processes, 2L)
})
