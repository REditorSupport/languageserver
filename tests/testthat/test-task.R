for (covr in c("false", "true")) {
    withr::with_envvar(list(R_COVR = covr), {
        test_that(paste("Task creation works", covr), {
            task <- create_task(function() 1 + 1, list())
            expect_s3_class(task, "Task")
        })

        test_that(paste("TaskManager runs task without session", covr), {
            tm <- TaskManager$new("test1", use_session = FALSE)

            result <- NULL
            cb <- function(res) {
                result <<- res
            }

            task <- create_task(function(x) x + 1, list(x = 1), callback = cb)
            tm$add_task("t1", task)

            tm$run_tasks()

            # Wait for the task to finish
            for (i in 1:10) {
                Sys.sleep(0.5)
                tm$check_tasks()
                if (!is.null(result)) break
            }

            expect_equal(result, 2)
            tm$stop()
        })

        test_that(paste("TaskManager runs task with session", covr), {
            tm <- TaskManager$new("test2", use_session = TRUE)

            result <- NULL
            cb <- function(res) {
                result <<- res
            }

            task <- create_task(function(x) x + 1, list(x = 1), callback = cb)
            tm$add_task("t1", task)

            # Sessions take time to start, so we might need multiple iterations
            for (i in 1:15) {
                tm$run_tasks()
                Sys.sleep(0.5)
                tm$check_tasks()
                if (!is.null(result)) break
            }

            expect_equal(result, 2)
            tm$stop()
        })

        test_that(paste("Task handles errors with session", covr), {
            tm <- TaskManager$new("test3", use_session = TRUE)

            err_res <- NULL
            err_cb <- function(e) {
                err_res <<- e
            }

            task <- create_task(function() stop("test error"), list(), error = err_cb)
            tm$add_task("t2", task)

            for (i in 1:15) {
                tm$run_tasks()
                Sys.sleep(0.5)
                tm$check_tasks()
                if (!is.null(err_res)) break
            }

            expect_true(inherits(err_res, "error"))
            expect_match(err_res$parent$message, "test error")
            tm$stop()
        })

        test_that(paste("Task handles errors without session", covr), {
            tm <- TaskManager$new("test4", use_session = FALSE)

            err_res <- NULL
            err_cb <- function(e) {
                err_res <<- e
            }

            task <- create_task(function() stop("test error"), list(), error = err_cb)
            tm$add_task("t2", task)

            tm$run_tasks()

            for (i in 1:10) {
                Sys.sleep(0.5)
                tm$check_tasks()
                if (!is.null(err_res)) break
            }

            expect_true(inherits(err_res, "error"))
            expect_match(err_res$parent$message, "test error")
            tm$stop()
        })
    })
}

test_that("TaskManager prunes idle sessions", {
    skip_on_cran()

    # Initialize TaskManager with a short timeout
    tm <- TaskManager$new(
        "test", use_session = TRUE, session_idle_timeout = 2,
        min_idle_sessions = 0)

    # Create a dummy task
    task <- create_task(function() 1, list())
    tm$add_task("1", task)

    # Run the task
    tm$run_tasks()

    # Wait for task completion
    start_time <- Sys.time()
    while (length(tm$.__enclos_env__$private$running_tasks$keys()) > 0 ||
        length(tm$.__enclos_env__$private$pending_tasks$keys()) > 0) {
        tm$check_tasks()
        tm$run_tasks()
        if (Sys.time() - start_time > 10) stop("Task timed out")
        Sys.sleep(0.1)
    }

    # Verify session is idle
    sessions <- tm$.__enclos_env__$private$sessions
    expect_length(sessions, 1)
    expect_equal(sessions[[1]]$get_state(), "idle")

    # Wait for timeout
    Sys.sleep(5)

    # Trigger pruning
    tm$check_tasks()

    # Verify session is removed
    sessions <- tm$.__enclos_env__$private$sessions
    expect_length(sessions, 0)

    tm$stop()
})

test_that("TaskManager refreshes pending task recency", {
    tm <- TaskManager$new(
        "recency", process_recent_first = TRUE,
        max_running_tasks = 1L, cpu_load = 1
    )
    tm$add_task("first", create_task(function() 1, list(), delay = 60))
    tm$add_task("second", create_task(function() 2, list(), delay = 60))
    tm$add_task("first", create_task(function() 3, list(), delay = 60))

    keys <- tm$.__enclos_env__$private$pending_tasks$keys()
    expect_equal(unlist(keys), c("second", "first"))
    tm$stop()
})

test_that("Task cancellation retires its persistent session", {
    withr::local_envvar(R_COVR = "false")
    state <- "idle"
    killed <- FALSE
    interrupted <- FALSE
    session <- list(
        call = function(...) state <<- "busy",
        get_state = function() state,
        read = function() NULL,
        kill = function(grace, close_connections) {
            expect_equal(grace, 0)
            expect_false(close_connections)
            killed <<- TRUE
            state <<- "finished"
        },
        interrupt = function() interrupted <<- TRUE
    )
    task <- create_task(function() NULL, list())

    task$start(session)
    task$kill()

    expect_true(killed)
    expect_false(interrupted)
})

test_that("TaskManager removes a cancelled session from the pool", {
    withr::local_envvar(R_COVR = "false")
    state <- "idle"
    session <- list(
        call = function(...) state <<- "busy",
        get_state = function() state,
        read = function() {
            if (state != "busy") return(NULL)
            state <<- "idle"
            list(code = 200L, error = NULL, result = NULL)
        },
        kill = function(...) NULL,
        close = function(...) NULL
    )
    tm <- TaskManager$new(
        "cancelled session", use_session = TRUE, min_idle_sessions = 0,
        max_running_tasks = 1L, cpu_load = 1
    )
    private <- tm$.__enclos_env__$private
    private$sessions <- list(session)
    tm$add_task("doc", create_task(function() NULL, list()))
    tm$run_tasks()

    tm$add_task("doc", create_task(function() NULL, list()))
    tm$check_tasks()

    expect_equal(state, "idle")
    expect_length(private$sessions, 0L)
    expect_true(private$pending_tasks$has("doc"))
    tm$stop()
})

test_that("TaskManager handles errors while dispatching to a session", {
    withr::local_envvar(R_COVR = "false")
    killed <- FALSE
    session <- list(
        call = function(...) stop("broken pipe"),
        get_state = function() "idle",
        read = function() NULL,
        kill = function(...) killed <<- TRUE,
        close = function(...) NULL
    )
    tm <- TaskManager$new(
        "dispatch error", use_session = TRUE, min_idle_sessions = 0,
        max_running_tasks = 1L, cpu_load = 1
    )
    private <- tm$.__enclos_env__$private
    private$sessions <- list(session)
    task_error <- NULL
    tm$add_task("doc", create_task(
        function() NULL, list(),
        error = function(e) task_error <<- e
    ))

    expect_silent(tm$run_tasks())

    expect_match(conditionMessage(task_error), "broken pipe")
    expect_true(killed)
    expect_length(private$sessions, 0L)
    expect_false(private$running_tasks$has("doc"))
    tm$stop()
})

test_that("TaskManager does not overprovision while a session starts", {
    tm <- TaskManager$new(
        "starting", use_session = TRUE, min_idle_sessions = 0,
        max_running_tasks = 4L, cpu_load = 1
    )
    private <- tm$.__enclos_env__$private
    fake_state <- "starting"
    private$sessions <- list(list(
        get_state = function() fake_state,
        read = function() NULL,
        close = function(...) NULL
    ))

    tm$add_task("doc", create_task(function() 1, list()))
    for (i in 1:3) tm$run_tasks()

    expect_length(private$sessions, 1L)
    expect_true(private$pending_tasks$has("doc"))
    fake_state <- "idle"
    tm$stop()
})

test_that("TaskManager supersedes a running task even at capacity", {
    skip_on_cran()
    tm <- TaskManager$new(
        "supersede", use_session = TRUE,
        max_running_tasks = 1L, cpu_load = 1
    )
    old_result <- NULL
    new_result <- NULL
    tm$add_task("doc", create_task(
        function() {
            Sys.sleep(0.5)
            "old"
        }, list(),
        callback = function(value) old_result <<- value
    ))
    for (i in 1:100) {
        tm$run_tasks()
        tm$check_tasks()
        if (tm$.__enclos_env__$private$running_tasks$has("doc")) break
        Sys.sleep(0.02)
    }

    tm$add_task("doc", create_task(
        function() "new", list(),
        callback = function(value) new_result <<- value
    ))
    expect_false(tm$.__enclos_env__$private$running_tasks$has("doc"))

    for (i in 1:200) {
        tm$check_tasks()
        tm$run_tasks()
        if (!is.null(new_result)) break
        Sys.sleep(0.02)
    }
    expect_null(old_result)
    expect_equal(new_result, "new")
    tm$stop()
})
