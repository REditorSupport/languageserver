shared_task_fixture <- function(key = "packages", callback = function(...) NULL,
    error = function(...) NULL, valid = function() TRUE, cacheable = function(...) TRUE,
    refresh = FALSE, delay = 0, current = NULL) {
    state <- new.env(parent = emptyenv())
    state$starts <- 0L
    state$kills <- 0L
    state$ready <- FALSE
    state$failed <- FALSE
    state$value <- list(packages = "fixture")
    sharing <- new.env(parent = emptyenv())
    sharing$key <- key
    sharing$callback <- callback
    sharing$error <- error
    sharing$valid <- valid
    sharing$current <- current
    sharing$cacheable <- cacheable
    sharing$refresh <- refresh
    sharing$complete <- callback
    sharing$fail <- error
    # Exercise the real scheduler without depending on process timing. Worker
    # completion follows the same dynamic handlers as package-resolution tasks.
    TestTask <- R6::R6Class(inherit = Task, public = list(
        start = function(session = NULL) {
            state$starts <- state$starts + 1L
            self$execution_started_at <- proc.time()[[3L]]
        },
        check = function() {
            if (!state$ready) return(FALSE)
            state$ready <- FALSE
            if (state$failed) sharing$fail(simpleError("worker failed"))
            else sharing$complete(state$value)
            TRUE
        },
        kill = function() {
            state$kills <- state$kills + 1L
            NULL
        }
    ))
    task <- TestTask$new(function() NULL, list(), delay = delay)
    task$sharing <- sharing
    list(task = task, state = state)
}

shared_task_manager <- function(..., .local_envir = parent.frame()) {
    manager <- SharedTaskManager$new("shared tests", max_running_tasks = 1L, ...)
    withr::defer(manager$stop(), envir = .local_envir)
    manager
}

test_that("equivalent document subscriptions share one queued and running worker", {
    manager <- shared_task_manager()
    received <- character()
    first <- shared_task_fixture(callback = function(...) received <<- c(received, "first"))
    second <- shared_task_fixture(callback = function(...) received <<- c(received, "second"))
    manager$add_task("first", first$task)
    manager$add_task("second", second$task)
    expect_identical(manager$get_task("first"), first$task)
    expect_identical(manager$get_task("second"), second$task)
    manager$run_tasks()
    expect_equal(first$state$starts, 1L)
    expect_equal(second$state$starts, 0L)
    expect_identical(manager$get_task("second")$execution_started_at, first$task$execution_started_at)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_setequal(received, c("first", "second"))
    expect_false(manager$has("first"))
    expect_false(manager$has("second"))
    expect_false(manager$has_work())
})

test_that("cancelling the worker's original subscriber retains other documents", {
    manager <- shared_task_manager()
    received <- character()
    first <- shared_task_fixture(callback = function(...) received <<- c(received, "first"))
    second <- shared_task_fixture(callback = function(...) received <<- c(received, "second"))
    manager$add_task("first", first$task)
    manager$add_task("second", second$task)
    manager$run_tasks()
    manager$cancel("first")
    expect_equal(first$state$kills, 0L)
    expect_false(manager$has("first"))
    expect_true(manager$has("second"))
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, "second")
    expect_false(manager$has_work())
})

test_that("last cancellation stops work and equivalent replacements retain work", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    replacement <- shared_task_fixture()
    manager$add_task("document", first$task)
    manager$run_tasks()
    manager$add_task("document", replacement$task)
    expect_equal(first$state$kills, 0L)
    expect_identical(manager$get_task("document"), replacement$task)
    manager$cancel("document")
    expect_equal(first$state$kills, 1L)
    expect_false(manager$has_work())
})

test_that("completed results are reused with aliases visible during cache delivery", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    manager$add_task("first", first$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    manager$check_tasks()
    received <- NULL
    later <- shared_task_fixture(callback = function(value) {
        expect_identical(manager$get_task("later"), later$task)
        expect_true(manager$has("later"))
        manager$promote("later")
        received <<- value
    })
    manager$add_task("later", later$task)
    expect_identical(received, first$state$value)
    expect_equal(later$state$starts, 0L)
    expect_false(manager$has_work())
    expect_false(manager$has("later"))
})

test_that("a refreshed callback can replace its own alias with fresh work", {
    manager <- shared_task_manager()
    next_task <- shared_task_fixture(refresh = TRUE)
    first <- shared_task_fixture(callback = function(...) {
        expect_identical(manager$get_task("document"), first$task)
        manager$add_task("document", next_task$task)
    })
    manager$add_task("document", first$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(manager$get_task("document"), next_task$task)
    expect_true(manager$has_work())
    manager$run_tasks()
    expect_equal(next_task$state$starts, 1L)
    next_task$state$ready <- TRUE
    manager$check_tasks()
    expect_false(manager$has_work())
})

test_that("finished work is cached before callbacks create fresh documents", {
    manager <- shared_task_manager()
    received <- FALSE
    later <- shared_task_fixture(callback = function(...) received <<- TRUE)
    first <- shared_task_fixture(callback = function(...) manager$add_task("later", later$task))
    manager$add_task("first", first$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_true(received)
    expect_equal(later$state$starts, 0L)
    expect_false(manager$has_work())
})

test_that("invalid completion and cache-hit contexts never deliver results", {
    manager <- shared_task_manager()
    current <- TRUE
    first <- shared_task_fixture(valid = function() current,
        callback = function(...) stop("invalid completion delivered"))
    manager$add_task("first", first$task)
    manager$run_tasks()
    current <- FALSE
    first$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    fresh <- shared_task_fixture()
    manager$add_task("fresh", fresh$task)
    manager$run_tasks()
    expect_equal(fresh$state$starts, 1L)
    fresh$state$ready <- TRUE
    manager$check_tasks()
    stale <- shared_task_fixture(valid = function() FALSE,
        callback = function(...) stop("invalid cache hit delivered"))
    expect_no_error(manager$add_task("stale", stale$task))
    expect_equal(stale$state$starts, 0L)
    expect_false(manager$has("stale"))
    expect_false(manager$has_work())
})

test_that("settlement releases invalid aliases and preserves reentrant replacements", {
    manager <- shared_task_manager()
    settled <- FALSE
    invalid <- shared_task_fixture(valid = function() FALSE)
    invalid$task$sharing$settled <- function() {
        expect_false(manager$has("invalid"))
        settled <<- TRUE
    }
    manager$add_task("invalid", invalid$task)
    manager$run_tasks()
    invalid$state$ready <- TRUE
    manager$check_tasks()
    expect_true(settled)

    replacement <- shared_task_fixture(refresh = TRUE)
    first <- shared_task_fixture(callback = function(...) {
        manager$add_task("document", replacement$task)
    })
    first$task$sharing$settled <- function() {
        expect_identical(manager$get_task("document"), replacement$task)
        stop("settlement hook failed")
    }
    manager$add_task("document", first$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_identical(manager$get_task("document"), replacement$task)
    expect_true(manager$has_work())
})

test_that("common context validation runs once for a shared result and cache hit", {
    manager <- shared_task_manager()
    validations <- 0L
    received <- 0L
    current <- function() {
        validations <<- validations + 1L
        TRUE
    }
    tasks <- lapply(1:4, function(i) {
        shared_task_fixture(current = current, callback = function(...) received <<- received + 1L)
    })
    for (i in 1:4) manager$add_task(as.character(i), tasks[[i]]$task)
    manager$run_tasks()
    tasks[[1L]]$state$ready <- TRUE
    manager$check_tasks()
    expect_equal(validations, 1L)
    expect_equal(received, 4L)
    cached <- shared_task_fixture(current = current)
    manager$add_task("cached", cached$task)
    expect_equal(validations, 2L)
    expect_equal(cached$state$starts, 0L)
    expect_false(manager$has_work())
})

test_that("changed common context settles all subscriptions without caching", {
    manager <- shared_task_manager()
    validations <- 0L
    settlements <- 0L
    current <- function() {
        validations <<- validations + 1L
        FALSE
    }
    tasks <- lapply(1:3, function(i) {
        task <- shared_task_fixture(current = current,
            callback = function(...) stop("stale context delivered"))
        task$task$sharing$settled <- function() {
            expect_false(manager$has(as.character(i)))
            settlements <<- settlements + 1L
        }
        task
    })
    for (i in 1:3) manager$add_task(as.character(i), tasks[[i]]$task)
    manager$run_tasks()
    tasks[[1L]]$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_equal(validations, 1L)
    expect_equal(settlements, 3L)
    fresh <- shared_task_fixture()
    manager$add_task("fresh", fresh$task)
    manager$run_tasks()
    expect_equal(fresh$state$starts, 1L)
})

test_that("subscriber failures and invalid contexts do not block other callbacks", {
    manager <- shared_task_manager()
    received <- FALSE
    first <- shared_task_fixture(callback = function(...) stop("callback failed"))
    stale <- shared_task_fixture(valid = function() FALSE,
        callback = function(...) stop("stale callback invoked"))
    second <- shared_task_fixture(callback = function(...) received <<- TRUE)
    manager$add_task("first", first$task)
    manager$add_task("stale", stale$task)
    manager$add_task("second", second$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_true(received)
    expect_false(manager$has_work())
    expect_false(manager$has("stale"))
})

test_that("failed and uncacheable results require a fresh worker", {
    for (failed in c(FALSE, TRUE)) {
        manager <- shared_task_manager()
        errors <- 0L
        first <- shared_task_fixture(cacheable = function(...) FALSE,
            error = function(...) errors <<- errors + 1L)
        second <- shared_task_fixture(error = function(...) errors <<- errors + 1L)
        manager$add_task("first", first$task)
        manager$add_task("second", second$task)
        manager$run_tasks()
        first$state$failed <- failed
        first$state$ready <- TRUE
        manager$check_tasks()
        expect_equal(errors, if (failed) 2L else 0L)
        later <- shared_task_fixture()
        manager$add_task("later", later$task)
        manager$run_tasks()
        expect_equal(later$state$starts, 1L)
        manager$stop()
    }
})

test_that("cache age and byte budgets bound reuse", {
    for (expired in c(FALSE, TRUE)) {
        manager <- if (expired) shared_task_manager() else shared_task_manager(max_cache_bytes = 1)
        first <- shared_task_fixture()
        manager$add_task("first", first$task)
        manager$run_tasks()
        first$state$ready <- TRUE
        manager$check_tasks()
        if (expired) {
            cache <- manager$.__enclos_env__$private$cache
            cached <- cache$get("packages")
            cached$time <- cached$time - 61
            cache$set("packages", cached)
        }
        later <- shared_task_fixture()
        manager$add_task("later", later$task)
        manager$run_tasks()
        expect_equal(later$state$starts, 1L)
        manager$stop()
    }
})

test_that("shared promotion and ordinary compatibility use the existing scheduler", {
    manager <- shared_task_manager()
    first <- shared_task_fixture(delay = 60)
    second <- shared_task_fixture(delay = 60)
    manager$add_task("first", first$task)
    manager$add_task("second", second$task)
    expect_equal(manager$wait_timeout(1), 1)
    manager$promote("second")
    expect_equal(manager$wait_timeout(1), 0)
    manager$run_tasks()
    expect_equal(first$state$starts, 1L)
    manager$cancel("first")
    manager$cancel("second")
    ordinary <- shared_task_fixture()
    ordinary$task$sharing <- NULL
    manager$add_task("ordinary", ordinary$task)
    expect_identical(manager$get_task("ordinary"), ordinary$task)
    expect_true(manager$has("ordinary"))
    manager$run_tasks()
    expect_equal(ordinary$state$starts, 1L)
    manager$add_task("ordinary", NULL)
    expect_equal(ordinary$state$kills, 1L)
    expect_false(manager$has_work())
})
