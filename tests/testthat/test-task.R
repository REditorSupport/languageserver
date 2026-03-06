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
