callback_session <- function() {
    session <- new.env(parent = emptyenv())
    session$state <- "idle"
    session$killed <- FALSE
    session$call <- function(...) session$state <- "busy"
    session$get_state <- function() session$state
    session$read <- function() {
        if (session$state != "busy") return(NULL)
        session$state <- "idle"
        list(code = 200L, result = TRUE)
    }
    session$kill <- function(...) {
        session$killed <- TRUE
        session$state <- "finished"
    }
    session$close <- function(...) NULL
    session
}

callback_manager <- function(sessions) {
    manager <- TaskManager$new("callbacks", use_session = TRUE,
        min_idle_sessions = 0L, max_running_tasks = length(sessions), cpu_load = 1)
    private <- manager$.__enclos_env__$private
    private$max_running_tasks <- length(sessions)
    private$sessions <- sessions
    mockery::stub(private$ensure_demand_sessions, "private$create_session", function() {
        stop("callback must reuse the existing worker")
    })
    manager
}

test_that("completion callbacks can cancel another running task", {
    withr::local_envvar(R_COVR = "false")
    manager <- callback_manager(list(callback_session(), callback_session()))
    withr::defer(manager$stop())
    delivered <- character()
    manager$add_task("first", create_task(identity, list(TRUE), callback = function(value) {
        delivered <<- c(delivered, "first")
        manager$cancel("second")
    }))
    manager$add_task("second", create_task(identity, list(TRUE), callback = function(value) {
        delivered <<- c(delivered, "second")
    }))
    manager$run_tasks()
    expect_error(manager$check_tasks(), NA)
    expect_identical(delivered, "first")
    expect_false(manager$has_work())
})

test_that("completion callbacks can dispatch a successor on the same worker", {
    withr::local_envvar(R_COVR = "false")
    session <- callback_session()
    manager <- callback_manager(list(session))
    withr::defer(manager$stop())
    delivered <- character()
    successor <- create_task(identity, list(TRUE), callback = function(value) {
        delivered <<- c(delivered, "successor")
    })
    original <- create_task(identity, list(TRUE), callback = function(value) {
        delivered <<- c(delivered, "original")
        manager$add_task("document", successor)
        manager$run_tasks()
    })
    manager$add_task("document", original)
    manager$run_tasks()
    manager$check_tasks()
    expect_false(session$killed)
    expect_identical(delivered, "original")
    expect_identical(manager$.__enclos_env__$private$running_tasks$get("document", NULL), successor)
    # A repeated check on the finished task must not consume its successor's result.
    expect_true(original$check())
    manager$check_tasks()
    expect_identical(delivered, c("original", "successor"))
    expect_false(manager$has_work())
})

test_that("dispatch callbacks cannot start a delayed replacement from a stale batch", {
    withr::local_envvar(R_COVR = "false")
    broken <- callback_session()
    broken$call <- function(...) stop("dispatch failure")
    idle <- callback_session()
    manager <- callback_manager(list(broken, idle))
    withr::defer(manager$stop())
    replacement <- create_task(identity, list(TRUE), delay = 60)
    manager$add_task("first", create_task(identity, list(TRUE), error = function(e) {
        manager$add_task("second", replacement)
    }))
    manager$add_task("second", create_task(identity, list(TRUE)))
    manager$run_tasks()
    private <- manager$.__enclos_env__$private
    expect_false(private$running_tasks$has("second"))
    expect_identical(private$pending_tasks$get("second", NULL), replacement)
    expect_identical(idle$state, "idle")
})

test_that("dispatch callbacks can stop the manager during a selected batch", {
    withr::local_envvar(R_COVR = "false")
    broken <- callback_session()
    broken$call <- function(...) stop("dispatch failure")
    manager <- callback_manager(list(broken, callback_session()))
    withr::defer(manager$stop())
    manager$add_task("first", create_task(identity, list(TRUE), error = function(e) manager$stop()))
    manager$add_task("second", create_task(identity, list(TRUE)))
    expect_error(manager$run_tasks(), NA)
    expect_false(manager$has_work())
})
test_that("native worker exit cleans descendants despite a buffered session state", {
    state <- new.env(parent = emptyenv())
    state$cleaned <- FALSE
    state$failed <- FALSE
    session <- list(call = function(...) NULL, is_alive = function() FALSE,
        get_state = function() "idle", read = function() stop("A dead worker buffer must not be read"))
    testthat::local_mocked_bindings(task_kill_descendants = function(...) {
        state$cleaned <- TRUE
    }, .package = "languageserver")
    task <- create_task(function() NULL, list(), track_descendants = TRUE,
        error = function(error) state$failed <- inherits(error, "error"))
    task$start(session)
    expect_true(task$check())
    expect_true(state$cleaned)
    expect_true(state$failed)
})
test_that("a buffered worker error cleans observed children before completing", {
    state <- new.env(parent = emptyenv())
    state$cleaned <- FALSE
    session <- list(call = function(...) NULL, is_alive = function() TRUE,
        get_state = function() "idle", read = function() list(code = 500L, error = simpleError("Worker exited")))
    testthat::local_mocked_bindings(task_kill_observed_children = function(...) {
        state$cleaned <- TRUE
    }, task_kill_descendants = function(...) stop("The reusable worker must not be killed"), .package = "languageserver")
    task <- create_task(function() NULL, list(), track_descendants = TRUE)
    task$start(session)
    expect_true(task$check())
    expect_true(state$cleaned)
})
