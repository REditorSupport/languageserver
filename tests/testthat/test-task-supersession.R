test_that("superseded parses discard results and reuse a finishing worker", {
    withr::local_envvar(R_COVR = "false")
    state <- new.env(parent = emptyenv())
    state$status <- "idle"
    state$ready <- FALSE
    state$killed <- FALSE
    state$delivered <- character()
    session <- list(
        call = function(...) state$status <- "busy",
        get_state = function() state$status,
        read = function() {
            if (!state$ready || state$status != "busy") {
                return(NULL)
            }
            state$status <- "idle"
            list(code = 200L, error = NULL, result = TRUE)
        },
        kill = function(...) {
            state$killed <- TRUE
            state$status <- "finished"
        },
        close = function(...) NULL
    )
    manager <- TaskManager$new("reuse",
        use_session = TRUE,
        min_idle_sessions = 0, max_running_tasks = 1, cpu_load = 1,
        cancellation_grace = 10
    )
    withr::defer(manager$stop())
    private <- manager$.__enclos_env__$private
    private$sessions <- list(session)
    task <- function(label) {
        create_task(function() TRUE, list(),
            callback = function(value) state$delivered <- c(state$delivered, label)
        )
    }
    manager$add_task("doc", task("old"))
    manager$run_tasks()
    manager$add_task("doc", task("intermediate"))
    manager$add_task("doc", task("latest"))
    manager$run_tasks()
    expect_true(private$running_tasks$has("doc"))
    expect_equal(private$pending_tasks$size(), 1L)
    state$ready <- TRUE
    manager$check_tasks()
    expect_length(state$delivered, 0L)
    expect_false(state$killed)
    expect_length(private$sessions, 1L)
    manager$run_tasks()
    manager$check_tasks()
    expect_equal(state$delivered, "latest")
    expect_false(manager$has_work())
})

test_that("obsolete workers are retired after the bounded cancellation grace", {
    withr::local_envvar(R_COVR = "false")
    state <- new.env(parent = emptyenv())
    state$status <- "idle"
    state$killed <- FALSE
    session <- list(
        call = function(...) state$status <- "busy",
        get_state = function() state$status,
        read = function() NULL,
        kill = function(...) {
            state$killed <- TRUE
            state$status <- "finished"
        },
        close = function(...) NULL
    )
    manager <- TaskManager$new("bounded",
        use_session = TRUE,
        min_idle_sessions = 0, max_running_tasks = 1, cpu_load = 1,
        cancellation_grace = 10
    )
    withr::defer(manager$stop())
    private <- manager$.__enclos_env__$private
    private$sessions <- list(session)
    manager$add_task("doc", create_task(function() TRUE, list()))
    manager$run_tasks()
    manager$add_task("doc", create_task(function() TRUE, list()))
    deadline <- private$superseded_tasks$get("doc")
    manager$add_task("doc", create_task(function() TRUE, list()))
    expect_equal(private$superseded_tasks$get("doc"), deadline)
    private$superseded_tasks$set("doc", -Inf)
    manager$check_tasks()
    expect_true(state$killed)
    expect_false(private$running_tasks$has("doc"))
    expect_true(private$pending_tasks$has("doc"))
    expect_length(private$sessions, 0L)
    expect_equal(private$superseded_tasks$size(), 0L)
})

test_that("Member requests promote the existing debounced parse without replacement", {
    manager <- TaskManager$new("promotion", max_running_tasks = 1L)
    withr::defer(manager$stop())
    task <- create_task(function() TRUE, list(), delay = 60)
    manager$add_task("doc", task)
    manager$promote("doc")
    manager$promote("doc")
    manager$promote("missing")
    private <- manager$.__enclos_env__$private
    expect_identical(private$pending_tasks$get("doc"), task)
    expect_equal(task$delay, 0)
    expect_equal(private$pending_tasks$size(), 1L)
})

test_that("task delay readiness uses monotonic deadlines and promotion", {
    manager <- TaskManager$new("deadline", max_running_tasks = 1L)
    withr::defer(manager$stop())
    task <- create_task(function() TRUE, list(), delay = 60)
    # Wall-clock jumps do not affect dispatch eligibility.
    task$time <- Sys.time() - 3600
    manager$add_task("doc", task)
    expect_equal(manager$wait_timeout(1), 1)
    manager$run_tasks()
    expect_true(manager$.__enclos_env__$private$pending_tasks$has("doc"))
    manager$promote("doc")
    expect_equal(manager$wait_timeout(1), 0)
})

test_that("ready queued tasks do not busy-poll when worker slots are full", {
    manager <- TaskManager$new("full", max_running_tasks = 1L, cpu_load = 1)
    withr::defer(manager$stop())
    manager$.__enclos_env__$private$running_tasks$set("running", create_task(function() TRUE, list()))
    manager$add_task("pending", create_task(function() TRUE, list()))
    expect_equal(manager$wait_timeout(0.01), 0.01)
})

test_that("queued tasks await starting persistent workers without busy-polling", {
    manager <- TaskManager$new("starting readiness", use_session = TRUE,
        min_idle_sessions = 0L, max_running_tasks = 1L)
    withr::defer(manager$stop())
    manager$.__enclos_env__$private$sessions <- list(list(
        get_state = function() "starting", close = function(...) NULL))
    manager$add_task("pending", create_task(function() TRUE, list()))
    expect_equal(manager$wait_timeout(0.01), 0.01)
})
