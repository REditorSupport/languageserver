member_refresh_workspace <- function(document) {
    workspace <- new.env(parent = emptyenv())
    workspace$documents <- collections::dict()
    workspace$documents$set(document$uri, document)
    workspace$member_metadata <- collections::dict()
    workspace$applied <- list()
    workspace$load_packages <- function(packages) {
        workspace$applied[[length(workspace$applied) + 1L]] <- packages
    }
    workspace$update_loaded_packages <- function() NULL
    workspace
}

member_refresh_fixture <- function(.local_envir = parent.frame()) {
    withr::local_envvar(R_COVR = "false", .local_envir = .local_envir)
    previous <- lsp_settings$get("member_completion")
    withr::defer(lsp_settings$set("member_completion", previous), envir = .local_envir)
    lsp_settings$set("member_completion", TRUE)
    document <- Document$new("file:///member-refresh.R", "r", 1L, "value <- 1L")
    document$requested_packages <- list(packages = c("firstfixture", "secondfixture"),
        namespace_packages = c("memberfixture", "otherfixture"))
    workspace <- member_refresh_workspace(document)
    server <- new.env(parent = emptyenv())
    server$workspace <- workspace
    server$get_workspace <- function(...) server$workspace
    server$pending_replies <- collections::dict()
    server$queue_pending_replies <- function(...) NULL
    server$member_task_manager <- TaskManager$new("refresh", use_session = TRUE,
        min_idle_sessions = 0L, max_running_tasks = 1L, cpu_load = 1)
    withr::defer(server$member_task_manager$stop(), envir = .local_envir)
    session_state <- new.env(parent = emptyenv())
    session_state$status <- "idle"
    session_state$ready <- FALSE
    session_state$killed <- 0L
    session <- list(
        call = function(...) session_state$status <- "busy",
        get_state = function() session_state$status,
        read = function() {
            if (!session_state$ready || session_state$status != "busy") return(NULL)
            session_state$status <- "idle"
            session_state$ready <- FALSE
            if (!is.null(session_state$error)) {
                list(code = 500L, error = session_state$error)
            } else {
                list(code = 200L, error = NULL, result = session_state$result)
            }
        },
        kill = function(...) {
            session_state$killed <- session_state$killed + 1L
            session_state$status <- "finished"
        },
        close = function(...) NULL
    )
    server$member_task_manager$.__enclos_env__$private$sessions <- list(session)
    refresh <- function() {
        resolve_task(server, document$uri, document, document$requested_packages$packages,
            namespace_packages = document$requested_packages$namespace_packages)
    }
    list(server = server, workspace = workspace, document = document,
        manager = server$member_task_manager, session = session_state, refresh = refresh)
}

member_refresh_result <- function(fixture) {
    index <- member_generic_index("")
    index$package <- "memberfixture"
    index$identity <- list(package = "memberfixture", revision = 2L)
    list(packages = fixture$document$requested_packages$packages,
        members = list(memberfixture = member_index_freeze(index)), s3 = NULL,
        requested = fixture$document$requested_packages)
}

test_that("Repeated queued member refreshes retain and promote one worker", {
    fixture <- member_refresh_fixture()
    first_resolve <- fixture$refresh()
    task <- fixture$manager$get_task(fixture$document$uri)
    task$delay <- 60
    for (i in 1:3) {
        resolve <- fixture$refresh()
        expect_false(identical(resolve, first_resolve))
        expect_identical(fixture$manager$get_task(fixture$document$uri), task)
    }
    expect_null(task$execution_started_at)
    expect_identical(task$delay, 0)
    expect_false(task$context$refresh_pending)
    expect_identical(fixture$manager$.__enclos_env__$private$pending_tasks$size(), 1L)
})

test_that("Running member refreshes finish once then capture fresh metadata in one followup", {
    fixture <- member_refresh_fixture()
    fixture$refresh()
    task <- fixture$manager$get_task(fixture$document$uri)
    fixture$manager$run_tasks()
    expect_false(is.null(task$execution_started_at))
    fixture$document$set_content(2L, "value <- 2L")
    for (i in 1:3) fixture$refresh()
    expect_identical(fixture$manager$get_task(fixture$document$uri), task)
    expect_true(task$context$refresh_pending)
    expect_identical(fixture$session$killed, 0L)
    fixture$session$result <- member_refresh_result(fixture)
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    followup <- fixture$manager$get_task(fixture$document$uri)
    expect_false(identical(followup, task))
    expect_false(task$context$refresh_pending)
    expect_false(followup$context$refresh_pending)
    expect_null(followup$execution_started_at)
    expect_length(fixture$workspace$applied, 1L)
    expect_identical(followup$.__enclos_env__$private$args$known_members$memberfixture,
        fixture$session$result$members$memberfixture$identity)
    expect_identical(fixture$manager$.__enclos_env__$private$pending_tasks$size(), 1L)
    fixture$manager$run_tasks()
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    expect_null(fixture$manager$get_task(fixture$document$uri))
    expect_length(fixture$workspace$applied, 2L)
    expect_identical(fixture$session$killed, 0L)
})

test_that("Failed member preparation also coalesces running refreshes into one retry", {
    fixture <- member_refresh_fixture()
    fixture$refresh()
    task <- fixture$manager$get_task(fixture$document$uri)
    fixture$manager$run_tasks()
    for (i in 1:3) fixture$refresh()
    fixture$session$error <- simpleError("fixture failure")
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    followup <- fixture$manager$get_task(fixture$document$uri)
    expect_false(identical(followup, task))
    expect_false(followup$context$refresh_pending)
    expect_length(fixture$workspace$applied, 0L)
    expect_identical(fixture$manager$.__enclos_env__$private$pending_tasks$size(), 1L)
    fixture$session$error <- NULL
    fixture$session$result <- member_refresh_result(fixture)
    fixture$manager$run_tasks()
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    expect_null(fixture$manager$get_task(fixture$document$uri))
    expect_length(fixture$workspace$applied, 1L)
})

test_that("Member application failures retain a requested refresh and newly applied identities", {
    fixture <- member_refresh_fixture()
    fixture$refresh()
    task <- fixture$manager$get_task(fixture$document$uri)
    fixture$manager$run_tasks()
    fixture$refresh()
    fixture$workspace$load_packages <- function(...) stop("fixture application failure")
    fixture$session$result <- member_refresh_result(fixture)
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    followup <- fixture$manager$get_task(fixture$document$uri)
    expect_false(identical(followup, task))
    expect_identical(followup$.__enclos_env__$private$args$known_members$memberfixture,
        fixture$session$result$members$memberfixture$identity)
    expect_false(followup$context$refresh_pending)
    expect_identical(fixture$session$killed, 0L)
})

test_that("Disabled members wake queued replies without scheduling another refresh", {
    fixture <- member_refresh_fixture()
    ready <- 0L
    fixture$server$queue_pending_replies <- function(...) ready <<- ready + 1L
    fixture$refresh()
    fixture$manager$run_tasks()
    fixture$refresh()
    lsp_settings$set("member_completion", FALSE)
    fixture$session$result <- member_refresh_result(fixture)
    fixture$session$ready <- TRUE
    fixture$manager$check_tasks()
    expect_null(fixture$manager$get_task(fixture$document$uri))
    expect_length(fixture$workspace$applied, 0L)
    expect_identical(ready, 1L)
})

test_that("Changed member context replaces work and stale completions cannot queue refreshes", {
    check <- function(change) {
        fixture <- member_refresh_fixture()
        fixture$refresh()
        task <- fixture$manager$get_task(fixture$document$uri)
        task$context$refresh_pending <- TRUE
        result <- member_refresh_result(fixture)
        if (change == "packages") {
            fixture$document$requested_packages$packages <- rev(fixture$document$requested_packages$packages)
        } else if (change == "namespaces") {
            fixture$document$requested_packages$namespace_packages <- rev(fixture$document$requested_packages$namespace_packages)
        } else if (change == "workspace") {
            fixture$server$workspace <- member_refresh_workspace(fixture$document)
        } else if (change == "document") {
            replacement <- Document$new(fixture$document$uri, "r", 1L, "value <- 3L")
            replacement$requested_packages <- fixture$document$requested_packages
            fixture$workspace$documents$set(replacement$uri, replacement)
        } else if (change == "library") {
            withr::local_libpaths(withr::local_tempdir(), action = "prefix")
        } else if (change == "disabled") {
            lsp_settings$set("member_completion", FALSE)
        } else if (change == "empty") {
            fixture$document$requested_packages <- list(packages = character(), namespace_packages = character())
        } else if (change == "cancelled" || change == "replaced") {
            fixture$manager$cancel(fixture$document$uri)
        }
        if (change == "document") {
            resolve_task(fixture$server, replacement$uri, replacement,
                replacement$requested_packages$packages,
                namespace_packages = replacement$requested_packages$namespace_packages)
        } else if (change != "cancelled") {
            fixture$refresh()
        }
        replacement <- fixture$manager$get_task(fixture$document$uri)
        expect_false(identical(replacement, task), info = change)
        if (change %in% c("disabled", "empty", "cancelled")) expect_null(replacement)
        else expect_s3_class(replacement, "Task")
        task$.__enclos_env__$private$callback(result)
        task$.__enclos_env__$private$error(simpleError("stale fixture failure"))
        expect_identical(fixture$manager$get_task(fixture$document$uri), replacement, info = change)
        expect_length(fixture$workspace$applied, 0L)
        expect_length(fixture$server$workspace$applied, 0L)
    }
    for (change in c("packages", "namespaces", "workspace", "document", "library", "disabled", "empty", "cancelled", "replaced")) {
        check(change)
    }
})
