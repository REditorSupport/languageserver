member_fallback_workspace <- function(document) {
    workspace <- new.env(parent = emptyenv())
    workspace$documents <- collections::dict()
    workspace$documents$set(document$uri, document)
    workspace$member_metadata <- collections::dict()
    workspace$s3_metadata <- S3MetadataCache$new()
    workspace$applied <- list()
    workspace$load_packages <- function(packages) {
        workspace$applied[[length(workspace$applied) + 1L]] <- packages
    }
    workspace$update_loaded_packages <- function() NULL
    workspace
}

member_fallback_fixture <- function(.local_envir = parent.frame()) {
    previous <- lapply(c("member_completion", "s3_dispatch"), lsp_settings$get)
    withr::defer({
        lsp_settings$set("member_completion", previous[[1L]])
        lsp_settings$set("s3_dispatch", previous[[2L]])
    }, envir = .local_envir)
    lsp_settings$set("member_completion", TRUE)
    lsp_settings$set("s3_dispatch", FALSE)
    withr::local_envvar(R_COVR = "false", .local_envir = .local_envir)
    state <- new.env(parent = emptyenv())
    state$complete <- FALSE
    state$processes <- list()
    testthat::local_mocked_bindings(detectCores = function(...) 2L,
        .package = "parallel", .env = .local_envir)
    testthat::local_mocked_bindings(package_resolution_installations = function(packages, ...) {
        list(key = "fixture-installation", complete = state$complete, packages = packages)
    }, .package = "languageserver", .env = .local_envir)
    testthat::local_mocked_bindings(r_bg = function(func, args, ...) {
        process <- new.env(parent = emptyenv())
        process$args <- args
        process$ready <- FALSE
        process$killed <- 0L
        process$error <- NULL
        process$is_alive <- function() !process$ready && process$killed == 0L
        process$kill <- function(...) process$killed <- process$killed + 1L
        process$get_result <- function() {
            if (!is.null(process$error)) stop(process$error)
            list(packages = args$pkgs, members = list(), s3 = NULL,
                requested = list(packages = normalize_package_request(args$pkgs),
                    namespace_packages = normalize_package_request(unique(args$namespace_packages))))
        }
        state$processes[[length(state$processes) + 1L]] <- process
        process
    }, .package = "callr", .env = .local_envir)
    document <- Document$new("file:///member-fallback.R", "r", 1L, "value <- 1L")
    document$requested_packages <- list(packages = c("firstfixture", "secondfixture"),
        namespace_packages = "memberfixture")
    server <- new.env(parent = emptyenv())
    server$workspace <- member_fallback_workspace(document)
    server$get_workspace <- function(...) server$workspace
    server$pending_replies <- collections::dict()
    server$queue_pending_replies <- function(...) NULL
    server$member_task_manager <- SharedTaskManager$new("fallback-members",
        max_running_tasks = 2L, cpu_load = 1)
    withr::defer(server$member_task_manager$stop(), envir = .local_envir)
    refresh <- function(doc = document) {
        resolve_task(server, doc$uri, doc, doc$requested_packages$packages,
            namespace_packages = doc$requested_packages$namespace_packages)
    }
    finish <- function(process) {
        process$ready <- TRUE
        server$member_task_manager$check_tasks()
    }
    list(server = server, document = document, state = state,
        manager = server$member_task_manager, refresh = refresh, finish = finish)
}

test_that("Incomplete installation identities retain and promote queued member preparation", {
    fixture <- member_fallback_fixture()
    fixture$refresh()
    first <- fixture$manager$get_task(fixture$document$uri)
    expect_null(first$sharing)
    first$delay <- 60
    for (i in seq_len(5L)) fixture$refresh()
    expect_identical(fixture$manager$get_task(fixture$document$uri), first)
    expect_identical(first$delay, 0)
    fixture$manager$run_tasks()
    expect_length(fixture$state$processes, 1L)
    fixture$finish(fixture$state$processes[[1L]])
    expect_length(fixture$server$workspace$applied, 1L)
    expect_false(fixture$manager$has_work())
})

test_that("Running fallback preparation finishes once before one member refresh", {
    fixture <- member_fallback_fixture()
    fixture$refresh()
    first <- fixture$manager$get_task(fixture$document$uri)
    fixture$manager$run_tasks()
    process <- fixture$state$processes[[1L]]
    fixture$document$set_content(2L, "value <- 2L")
    for (i in seq_len(5L)) fixture$refresh()
    expect_identical(fixture$manager$get_task(fixture$document$uri), first)
    expect_true(first$context$refresh_pending)
    expect_identical(process$killed, 0L)
    fixture$finish(process)
    followup <- fixture$manager$get_task(fixture$document$uri)
    expect_false(is.null(followup))
    expect_false(identical(first, followup))
    expect_null(followup$sharing)
    expect_length(fixture$server$workspace$applied, 1L)
    fixture$manager$run_tasks()
    expect_length(fixture$state$processes, 2L)
    fixture$finish(fixture$state$processes[[2L]])
    expect_length(fixture$server$workspace$applied, 2L)
    expect_false(fixture$manager$has_work())
    expect_identical(process$killed, 0L)
})

test_that("Fallback member preparation keeps different documents isolated", {
    fixture <- member_fallback_fixture()
    other <- Document$new("file:///member-fallback-other.R", "r", 1L, "value <- 1L")
    other$requested_packages <- fixture$document$requested_packages
    fixture$server$workspace$documents$set(other$uri, other)
    fixture$refresh()
    fixture$refresh(other)
    expect_false(identical(fixture$manager$get_task(other$uri),
            fixture$manager$get_task(fixture$document$uri)))
    fixture$manager$run_tasks()
    expect_length(fixture$state$processes, 2L)
    for (process in fixture$state$processes) fixture$finish(process)
    expect_length(fixture$server$workspace$applied, 2L)
    expect_false(fixture$manager$has_work())
})

test_that("Failed fallback preparation retains one refresh retry", {
    fixture <- member_fallback_fixture()
    fixture$refresh()
    fixture$manager$run_tasks()
    process <- fixture$state$processes[[1L]]
    for (i in seq_len(5L)) fixture$refresh()
    process$error <- "fixture preparation failed"
    fixture$finish(process)
    expect_identical(process$killed, 0L)
    expect_length(fixture$server$workspace$applied, 0L)
    fixture$manager$run_tasks()
    expect_length(fixture$state$processes, 2L)
    fixture$finish(fixture$state$processes[[2L]])
    expect_length(fixture$server$workspace$applied, 1L)
    expect_false(fixture$manager$has_work())
})

test_that("Fallback coalescing still replaces changed document and package contexts", {
    for (change in c("document", "workspace", "packages", "namespace", "library", "cache")) {
        fixture <- member_fallback_fixture()
        fixture$refresh()
        first <- fixture$manager$get_task(fixture$document$uri)
        fixture$manager$run_tasks()
        doc <- fixture$document
        if (change == "document") {
            doc <- Document$new(doc$uri, "r", 2L, "value <- 2L")
            doc$requested_packages <- fixture$document$requested_packages
            fixture$server$workspace$documents$set(doc$uri, doc)
        }
        if (change == "workspace") fixture$server$workspace <- member_fallback_workspace(doc)
        if (change == "packages") doc$requested_packages$packages <- rev(doc$requested_packages$packages)
        if (change == "namespace") doc$requested_packages$namespace_packages <- "otherfixture"
        if (change == "library") withr::local_libpaths(withr::local_tempdir())
        if (change == "cache") withr::local_options(languageserver.package_cache_dir = FALSE)
        fixture$refresh(doc)
        expect_false(identical(fixture$manager$get_task(doc$uri), first), info = change)
        expect_identical(fixture$state$processes[[1L]]$killed, 1L, info = change)
        fixture$finish(fixture$state$processes[[1L]])
        expect_equal(length(fixture$server$workspace$applied), 0L, info = change)
        fixture$manager$stop()
    }
})

test_that("Complete and incomplete installation transitions replace member preparation", {
    for (initial in c(FALSE, TRUE)) {
        fixture <- member_fallback_fixture()
        fixture$state$complete <- initial
        fixture$refresh()
        first <- fixture$manager$get_task(fixture$document$uri)
        expect_identical(is.environment(first$sharing), initial)
        fixture$manager$run_tasks()
        fixture$state$complete <- !initial
        fixture$refresh()
        replacement <- fixture$manager$get_task(fixture$document$uri)
        expect_false(identical(replacement, first))
        expect_identical(is.environment(replacement$sharing), !initial)
        expect_identical(fixture$state$processes[[1L]]$killed, 1L)
        fixture$finish(fixture$state$processes[[1L]])
        expect_length(fixture$server$workspace$applied, 0L)
        fixture$manager$run_tasks()
        fixture$finish(fixture$state$processes[[2L]])
        expect_length(fixture$server$workspace$applied, 1L)
        expect_false(fixture$manager$has_work())
        fixture$manager$stop()
    }
})
