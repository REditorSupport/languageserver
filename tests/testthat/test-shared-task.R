shared_task_fixture <- function(key = "packages", callback = function(...) NULL,
    error = function(...) NULL, valid = function() TRUE, cacheable = function(...) TRUE,
    refresh = FALSE, delay = 0, current = NULL, invalidated = NULL, settled = NULL) {
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
    sharing$invalidated <- invalidated
    sharing$settled <- settled
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

test_that("running forced refresh waits for one fresh successor and keeps peer results", {
    manager <- shared_task_manager()
    received <- list()
    first <- shared_task_fixture(callback = function(value) received$original <<- value)
    first$state$value <- "before refresh"
    peer <- shared_task_fixture(callback = function(value) received$peer <<- value)
    refresh <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received$refresh <<- value)
    refresh$state$value <- "after refresh"
    manager$add_task("document", first$task)
    manager$add_task("peer", peer$task)
    manager$run_tasks()
    manager$add_task("document", refresh$task)
    expect_identical(manager$get_task("document"), refresh$task)
    expect_null(manager$get_task("document")$execution_started_at)
    expect_equal(first$state$kills, 0L)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received$peer, "before refresh")
    expect_null(received$original)
    expect_null(received$refresh)
    expect_true(manager$has("document"))
    later <- shared_task_fixture(callback = function(value) received$later <<- value)
    manager$add_task("later", later$task)
    expect_null(received$later)
    expect_true(manager$has("later"))
    manager$run_tasks()
    expect_equal(refresh$state$starts, 1L)
    expect_equal(later$state$starts, 0L)
    refresh$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received$refresh, "after refresh")
    expect_identical(received$later, "after refresh")
    expect_false(manager$has_work())
})

test_that("running refresh bursts coalesce using the latest live descriptor", {
    manager <- shared_task_manager()
    received <- list()
    first <- shared_task_fixture()
    old_refresh <- shared_task_fixture(refresh = TRUE,
        callback = function(...) stop("superseded refresh callback ran"))
    peer <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received$peer <<- value)
    latest <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received$latest <<- value)
    latest$state$value <- "latest worker inputs"
    manager$add_task("original", first$task)
    manager$run_tasks()
    manager$add_task("refresh", old_refresh$task)
    manager$add_task("peer", peer$task)
    manager$add_task("refresh", latest$task)
    later <- shared_task_fixture(callback = function(value) received$later <<- value)
    manager$add_task("later", later$task)
    expect_null(manager$get_task("refresh")$execution_started_at)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_length(received, 0L)
    manager$run_tasks()
    expect_equal(latest$state$starts, 1L)
    expect_equal(old_refresh$state$starts, 0L)
    expect_equal(peer$state$starts, 0L)
    expect_equal(later$state$starts, 0L)
    latest$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, list(peer = "latest worker inputs", latest = "latest worker inputs",
            later = "latest worker inputs"))
    expect_false(manager$has_work())
    cached <- shared_task_fixture(callback = function(value) received$cached <<- value)
    manager$add_task("cached", cached$task)
    expect_identical(received$cached, "latest worker inputs")
    expect_equal(cached$state$starts, 0L)
})

test_that("a single document's running refresh burst retains one latest successor", {
    manager <- shared_task_manager()
    received <- NULL
    first <- shared_task_fixture(callback = function(...) stop("pre-refresh result delivered"))
    manager$add_task("document", first$task)
    manager$run_tasks()
    refreshes <- lapply(1:3, function(i) {
        task <- shared_task_fixture(refresh = TRUE, callback = function(value) received <<- value)
        task$state$value <- paste0("refresh ", i)
        manager$add_task("document", task$task)
        task
    })
    expect_equal(first$state$kills, 0L)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_null(received)
    manager$run_tasks()
    expect_identical(vapply(refreshes, function(task) task$state$starts, integer(1L)), c(0L, 0L, 1L))
    refreshes[[3L]]$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, "refresh 3")
    expect_false(manager$has_work())
})

test_that("peer callbacks join a queued successor without losing their new aliases", {
    manager <- shared_task_manager()
    received <- list()
    next_peer <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received$peer <<- value)
    peer <- shared_task_fixture(callback = function(...) {
        expect_identical(manager$get_task("peer"), peer$task)
        manager$add_task("peer", next_peer$task)
    }, settled = function() expect_identical(manager$get_task("peer"), next_peer$task))
    refresh <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received$refresh <<- value)
    refresh$state$value <- "fresh shared result"
    manager$add_task("peer", peer$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    peer$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_identical(manager$get_task("peer"), next_peer$task)
    manager$run_tasks()
    expect_equal(refresh$state$starts, 1L)
    expect_equal(next_peer$state$starts, 0L)
    refresh$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, list(refresh = "fresh shared result", peer = "fresh shared result"))
    expect_false(manager$has_work())
})

test_that("queued refreshes retain one worker and a promoted successor starts promptly", {
    manager <- shared_task_manager()
    received <- NULL
    first <- shared_task_fixture(delay = 60)
    refresh <- shared_task_fixture(refresh = TRUE, callback = function(value) received <<- value)
    manager$add_task("document", first$task)
    manager$add_task("document", refresh$task)
    manager$run_tasks()
    expect_equal(first$state$starts, 1L)
    expect_equal(refresh$state$starts, 0L)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, first$state$value)

    running <- shared_task_fixture(refresh = TRUE)
    manager$add_task("document", running$task)
    manager$run_tasks()
    successor <- shared_task_fixture(refresh = TRUE, delay = 60)
    manager$add_task("document", successor$task)
    manager$promote("document")
    running$state$ready <- TRUE
    manager$check_tasks()
    manager$run_tasks()
    expect_equal(successor$state$starts, 1L)
    expect_equal(successor$task$delay, 0)
})

test_that("refresh cancellation retains peers without recaching pre-refresh results", {
    manager <- shared_task_manager()
    received <- NULL
    first <- shared_task_fixture(callback = function(value) received <<- value)
    refresh <- shared_task_fixture(refresh = TRUE)
    manager$add_task("peer", first$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    manager$cancel("refresh")
    expect_equal(first$state$kills, 0L)
    expect_true(manager$has("peer"))
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, first$state$value)
    expect_equal(refresh$state$starts, 0L)
    later <- shared_task_fixture()
    manager$add_task("later", later$task)
    manager$run_tasks()
    expect_equal(later$state$starts, 1L)
    expect_false(manager$has("peer"))
})

test_that("new subscribers after refresh cancellation still wait for a fresh worker", {
    manager <- shared_task_manager()
    received <- NULL
    first <- shared_task_fixture()
    refresh <- shared_task_fixture(refresh = TRUE)
    later <- shared_task_fixture(callback = function(value) received <<- value)
    manager$add_task("peer", first$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    manager$cancel("refresh")
    manager$add_task("later", later$task)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_null(received)
    expect_true(manager$has("later"))
    manager$run_tasks()
    expect_equal(later$state$starts, 1L)
    expect_equal(refresh$state$starts, 0L)
})

test_that("cancelling the final refresh cancels an otherwise unneeded predecessor", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    refresh <- shared_task_fixture(refresh = TRUE)
    manager$add_task("document", first$task)
    manager$run_tasks()
    manager$add_task("document", refresh$task)
    expect_equal(first$state$kills, 0L)
    manager$cancel("document")
    expect_equal(first$state$kills, 1L)
    expect_false(manager$has_work())
    expect_false(manager$has("document"))
    first$task$sharing$complete("late cancelled result")
    later <- shared_task_fixture()
    manager$add_task("later", later$task)
    manager$run_tasks()
    expect_equal(later$state$starts, 1L)
})

test_that("cancelling the latest descriptor chooses the remaining refresh peer", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    peer <- shared_task_fixture(refresh = TRUE)
    latest <- shared_task_fixture(refresh = TRUE)
    manager$add_task("original", first$task)
    manager$run_tasks()
    manager$add_task("peer", peer$task)
    manager$add_task("latest", latest$task)
    manager$cancel("latest")
    first$state$ready <- TRUE
    manager$check_tasks()
    manager$run_tasks()
    expect_equal(peer$state$starts, 1L)
    expect_equal(latest$state$starts, 0L)
    expect_equal(first$state$kills, 0L)
})

test_that("pre-refresh worker errors do not fail fresh successor subscribers", {
    manager <- shared_task_manager()
    old_errors <- 0L
    refresh_errors <- 0L
    received <- NULL
    first <- shared_task_fixture(error = function(...) old_errors <<- old_errors + 1L)
    refresh <- shared_task_fixture(refresh = TRUE,
        callback = function(value) received <<- value,
        error = function(...) refresh_errors <<- refresh_errors + 1L)
    manager$add_task("peer", first$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    first$state$failed <- TRUE
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_equal(old_errors, 1L)
    expect_equal(refresh_errors, 0L)
    expect_true(manager$has("refresh"))
    manager$run_tasks()
    refresh$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, refresh$state$value)
})

test_that("obsolete owned aliases invalidate after removal and always settle", {
    for (common in c(FALSE, TRUE)) {
        manager <- shared_task_manager()
        invalidations <- 0L
        settlements <- 0L
        next_task <- shared_task_fixture(key = "updated context")
        first <- shared_task_fixture(current = function() !common, valid = function() common,
            callback = function(...) stop("invalid result delivered"),
            invalidated = function() {
                expect_false(manager$has("document"))
                invalidations <<- invalidations + 1L
                manager$add_task("document", next_task$task)
            },
            settled = function() {
                settlements <<- settlements + 1L
                expect_identical(manager$get_task("document"), next_task$task)
            })
        manager$add_task("document", first$task)
        manager$run_tasks()
        first$state$ready <- TRUE
        expect_no_error(manager$check_tasks())
        expect_equal(invalidations, 1L)
        expect_equal(settlements, 1L)
        expect_identical(manager$get_task("document"), next_task$task)
        manager$run_tasks()
        expect_equal(next_task$state$starts, 1L)
    }
})

test_that("cache-hit invalidation replaces its alias and hook errors still settle", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    manager$add_task("first", first$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    manager$check_tasks()
    replacement <- shared_task_fixture(key = "updated context")
    invalid <- shared_task_fixture(current = function() FALSE, invalidated = function() {
        expect_false(manager$has("cached"))
        manager$add_task("cached", replacement$task)
    }, settled = function() expect_identical(manager$get_task("cached"), replacement$task))
    manager$add_task("cached", invalid$task)
    expect_identical(manager$get_task("cached"), replacement$task)
    expect_equal(invalid$state$starts, 0L)

    settled <- FALSE
    failed <- shared_task_fixture(key = "invalid hook", valid = function() FALSE,
        invalidated = function() stop("invalidation hook failed"),
        settled = function() settled <<- TRUE)
    manager$add_task("failed", failed$task)
    manager$cancel("cached")
    manager$run_tasks()
    failed$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_true(settled)
    expect_false(manager$has("failed"))
})

test_that("invalidation of one alias cannot overwrite a peer already replaced by its hook", {
    manager <- shared_task_manager()
    next_first <- shared_task_fixture(key = "updated context")
    next_peer <- shared_task_fixture(key = "updated context")
    peer_settled <- FALSE
    first <- shared_task_fixture(current = function() FALSE, invalidated = function() {
        manager$add_task("first", next_first$task)
        manager$add_task("peer", next_peer$task)
    })
    peer <- shared_task_fixture(current = function() FALSE,
        invalidated = function() stop("obsolete peer invalidated its replacement"),
        settled = function() {
            expect_identical(manager$get_task("peer"), next_peer$task)
            peer_settled <<- TRUE
        })
    manager$add_task("first", first$task)
    manager$add_task("peer", peer$task)
    manager$run_tasks()
    first$state$ready <- TRUE
    expect_no_error(manager$check_tasks())
    expect_true(peer_settled)
    expect_identical(manager$get_task("first"), next_first$task)
    expect_identical(manager$get_task("peer"), next_peer$task)
})

test_that("stale successor context regenerates its descriptor before worker launch", {
    manager <- shared_task_manager()
    received <- NULL
    invalidated <- 0L
    first <- shared_task_fixture()
    regenerated <- shared_task_fixture(key = "updated context",
        callback = function(value) received <<- value)
    regenerated$state$value <- "current descriptor"
    refresh <- shared_task_fixture(refresh = TRUE, current = function() FALSE,
        callback = function(...) stop("stale refresh result delivered"),
        invalidated = function() {
            expect_false(manager$has("refresh"))
            invalidated <<- invalidated + 1L
            manager$add_task("refresh", regenerated$task)
        })
    manager$add_task("peer", first$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    first$state$ready <- TRUE
    manager$check_tasks()
    expect_equal(invalidated, 1L)
    expect_identical(manager$get_task("refresh"), regenerated$task)
    manager$run_tasks()
    expect_equal(refresh$state$starts, 0L)
    expect_equal(regenerated$state$starts, 1L)
    regenerated$state$ready <- TRUE
    manager$check_tasks()
    expect_identical(received, "current descriptor")
})

test_that("stop retires running and deferred refresh subscriptions", {
    manager <- shared_task_manager()
    first <- shared_task_fixture()
    refresh <- shared_task_fixture(refresh = TRUE,
        callback = function(...) stop("stopped refresh callback ran"),
        invalidated = function(...) stop("stopped refresh invalidated"))
    manager$add_task("peer", first$task)
    manager$run_tasks()
    manager$add_task("refresh", refresh$task)
    manager$stop()
    expect_equal(first$state$kills, 1L)
    expect_false(manager$has("peer"))
    expect_false(manager$has("refresh"))
    expect_no_error(first$task$sharing$complete("late stopped result"))
    manager$run_tasks()
    expect_equal(refresh$state$starts, 0L)
    expect_false(manager$has_work())
})
