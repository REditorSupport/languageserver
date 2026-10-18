test_that("unchanged delayed queues share one scheduling scan", {
    manager <- TaskManager$new("delayed", max_running_tasks = 1L)
    withr::defer(manager$stop())
    private <- manager$.__enclos_env__$private
    for (i in 1:100) manager$add_task(as.character(i), create_task(identity, list(TRUE), delay = 60))
    scans <- 0L
    keys <- private$pending_tasks$keys
    private$pending_tasks$keys <- function() {
        scans <<- scans + 1L
        keys()
    }
    for (i in 1:10) {
        manager$run_tasks()
        expect_equal(manager$wait_timeout(1), 1)
    }
    expect_equal(scans, 1L)

    manager$promote("1")
    expect_equal(manager$wait_timeout(1), 0)
    expect_equal(scans, 2L)
    manager$cancel("1")
    expect_equal(manager$wait_timeout(1), 1)
    manager$add_task("ready", create_task(identity, list(TRUE)))
    expect_equal(manager$wait_timeout(1), 0)
})

test_that("cached schedules wake at the next delayed task deadline", {
    manager <- TaskManager$new("deadline", max_running_tasks = 1L)
    withr::defer(manager$stop())
    now <- proc.time()[[3L]]
    mockery::stub(manager$.__enclos_env__$private$schedule, "proc.time", function() c(0, 0, now))
    mockery::stub(manager$wait_timeout, "proc.time", function() c(0, 0, now))
    manager$add_task("first", create_task(identity, list(TRUE), delay = 60))
    manager$add_task("second", create_task(identity, list(TRUE), delay = 120))
    expect_equal(manager$wait_timeout(30), 30)
    now <- now + 90
    expect_equal(manager$wait_timeout(30), 0)
    manager$cancel("first")
    expect_equal(manager$wait_timeout(10), 10)
    now <- now + 60
    expect_equal(manager$wait_timeout(10), 0)
})

test_that("provider promotion takes precedence over newer background work", {
    manager <- TaskManager$new("promoted", process_recent_first = TRUE,
        use_session = TRUE, min_idle_sessions = 0L, max_running_tasks = 1L)
    withr::defer(manager$stop())
    private <- manager$.__enclos_env__$private
    state <- "idle"
    session <- list(
        get_state = function() state,
        call = function(...) state <<- "busy",
        read = function() NULL,
        kill = function(...) state <<- "finished",
        close = function(...) NULL
    )
    private$sessions <- list(session)
    manager$add_task("requested", create_task(identity, list(TRUE), delay = 60))
    manager$add_task("background", create_task(identity, list(TRUE)))
    expect_equal(manager$wait_timeout(), 0)
    manager$promote("requested")
    manager$run_tasks()
    expect_equal(private$running_tasks$keys(), list("requested"))
    expect_equal(private$pending_tasks$keys(), list("background"))
})

test_that("an immediately satisfied replacement cancels obsolete work", {
    manager <- TaskManager$new("cache hit", max_running_tasks = 1L)
    withr::defer(manager$stop())
    manager$add_task("document", create_task(identity, list(TRUE), delay = 60))
    manager$add_task("document", NULL)
    expect_false(manager$has_work())
    expect_equal(manager$wait_timeout(1), 1)
})

test_that("session initialization runs asynchronously before the first task", {
    skip_on_cran()
    manager <- TaskManager$new("initialize", use_session = TRUE,
        max_running_tasks = 1L, min_idle_sessions = 0L,
        session_init = quote(options(languageserver.worker_initialized = TRUE)))
    withr::defer(manager$stop())
    result <- NULL
    manager$add_task("first", create_task(
        function() getOption("languageserver.worker_initialized"), list(),
        callback = function(value) result <<- value))
    for (i in 1:200) {
        manager$check_tasks()
        manager$run_tasks()
        if (!is.null(result)) break
        Sys.sleep(0.02)
    }
    expect_true(result)
    expect_false(manager$has_work())
})

test_that("fast tasks reset a reused worker's idle timeout", {
    manager <- TaskManager$new("idle timeout", use_session = TRUE,
        min_idle_sessions = 0L, max_running_tasks = 1L, session_idle_timeout = 1)
    withr::defer(manager$stop())
    session <- new.env(parent = emptyenv())
    state <- "idle"
    closed <- FALSE
    session$get_state <- function() state
    session$call <- function(...) state <<- "busy"
    session$read <- function() {
        state <<- "idle"
        list(code = 200L, result = TRUE)
    }
    session$close <- function(...) closed <<- TRUE
    attr(session, "idle_start") <- -Inf
    manager$.__enclos_env__$private$sessions <- list(session)
    manager$add_task("quick", create_task(identity, list(TRUE)))
    manager$run_tasks()
    manager$check_tasks()
    expect_false(closed)
    expect_false(manager$has_work())
    expect_length(manager$.__enclos_env__$private$sessions, 1L)
    expect_true(is.finite(attr(session, "idle_start")))
})
