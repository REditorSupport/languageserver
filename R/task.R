#' @import callr
Task <- R6::R6Class("Task",
    private = list(
        process = NULL,
        session = NULL,
        target = NULL,
        args = NULL,
        callback = NULL,
        error = NULL,
        cancelled = FALSE,
        invoke_handler = function(handler, value, type) {
            if (private$cancelled || is.null(handler)) return(invisible(NULL))
            tryCatch(
                handler(value),
                error = function(e) {
                    tryCatch(
                        logger$info("task ", type, " callback error: ", e),
                        error = function(e) NULL
                    )
                }
            )
            invisible(NULL)
        }
    ),
    public = list(
        time = NULL,
        delay = NULL,
        initialize = function(target, args, callback = NULL, error = NULL, delay = 0) {
            private$target <- target
            private$args <- args
            private$callback <- callback
            private$error <- error
            self$time <- Sys.time()
            if (!is.numeric(delay) || length(delay) != 1L ||
                    is.na(delay) || delay < 0) {
                delay <- 0
            }
            self$delay <- as.numeric(delay)
        },
        start = function(session = NULL) {
            if (is.null(session)) {
                private$process <- callr::r_bg(
                    private$target,
                    private$args,
                    system_profile = TRUE, user_profile = TRUE
                )
            } else {
                private$session <- session
                private$session$call(
                    private$target,
                    private$args
                )
            }
        },
        check = function() {
            if (!is.null(private$session)) {
                res <- private$session$read()
                if (!is.null(res)) {
                    if (isTRUE(res$code == 200) && is.null(res$error)) {
                        private$invoke_handler(
                            private$callback, res$result, "completion")
                        return(TRUE)
                    } else if (!is.null(res$code)) {
                        err <- res$error
                        if (is.null(err)) err <- simpleError(paste("Session error with code", res$code))
                        private$invoke_handler(private$error, err, "error")
                        return(TRUE)
                    }
                }
                state <- private$session$get_state()
                if (identical(state, "finished")) {
                    err <- simpleError("Session finished unexpectedly while task was running")
                    private$invoke_handler(private$error, err, "error")
                    return(TRUE)
                }
                return(FALSE)
            }

            if (is.null(private$process)) {
                FALSE
            } else if (private$process$is_alive()) {
                FALSE
            } else {
                # r_bg$get_result() will throw
                result <- tryCatch(private$process$get_result(), error = function(e) e)

                if (inherits(result, "error")) {
                    private$invoke_handler(private$error, result, "error")
                } else {
                    private$invoke_handler(private$callback, result, "completion")
                }
                TRUE
            }
        },
        fail = function(error) {
            private$invoke_handler(private$error, error, "error")
            invisible(NULL)
        },
        kill = function() {
            private$cancelled <- TRUE
            retired_session <- NULL
            if (!is.null(private$session)) {
                if (!identical(Sys.getenv("R_COVR"), "true")) {
                    # An interrupt can arrive after this call has completed and
                    # interrupt the next task on the persistent session. Retire
                    # the worker instead so cancellation cannot leak across tasks.
                    retired_session <- private$session
                    tryCatch(
                        private$session$kill(
                            grace = 0, close_connections = FALSE),
                        error = function(e) NULL
                    )
                }
            } else if (!is.null(private$process) && private$process$is_alive()) {
                if (identical(Sys.getenv("R_COVR"), "true")) {
                    private$process$wait()
                } else {
                    private$process$kill()
                }
            }
            invisible(retired_session)
        }
    )
)

TaskManager <- R6::R6Class("TaskManager",
    private = list(
        pending_tasks = NULL,
        running_tasks = NULL,
        name = NULL,
        use_session = NULL,
        sessions = NULL,
        process_recent_first = NULL,
        max_running_tasks = NULL,
        session_idle_timeout = NULL,
        min_idle_sessions = NULL,
        cancelled_tasks = NULL,
        stopping = FALSE,
        log_error = function(...) {
            tryCatch(logger$error(...), error = function(e) NULL)
            invisible(NULL)
        },
        remove_session = function(session) {
            keep <- !vapply(
                private$sessions, identical, logical(1L), y = session)
            private$sessions <- private$sessions[keep]
        },
        retire_session = function(session) {
            private$remove_session(session)
            if (!identical(Sys.getenv("R_COVR"), "true")) {
                tryCatch(
                    session$kill(grace = 0),
                    error = function(e) private$log_error(
                        private$name, " failed to retire task session: ", e)
                )
            }
        },
        create_session = function() {
            session <- callr::r_session$new(
                options = callr::r_session_options(
                    system_profile = TRUE,
                    user_profile = TRUE
                ),
                # Starting R can take hundreds of milliseconds. Never wait
                # for it on the language-server event loop.
                wait = FALSE
            )
            private$sessions <- append(private$sessions, session)
            session
        },
        ensure_min_sessions = function() {
            if (!isTRUE(private$use_session) || private$stopping) return(NULL)
            while (length(private$sessions) < private$min_idle_sessions &&
                    length(private$sessions) < private$max_running_tasks) {
                private$create_session()
            }
        },
        ensure_demand_sessions = function(demand) {
            if (!isTRUE(private$use_session) || private$stopping) return(NULL)
            target <- min(
                private$max_running_tasks,
                private$running_tasks$size() + max(as.integer(demand), 0L)
            )
            while (length(private$sessions) < target) {
                created <- tryCatch(
                    {
                        private$create_session()
                        TRUE
                    },
                    error = function(e) {
                        private$log_error(
                            private$name, " failed to create task session: ", e)
                        FALSE
                    }
                )
                if (!created) break
            }
            invisible(NULL)
        },
        find_available_session = function() {
            if (!isTRUE(private$use_session)) {
                return(NULL)
            }

            for (s in private$sessions) {
                state <- tryCatch(
                    {
                        state <- s$get_state()
                        if (state == "starting") {
                            res <- s$read()
                            if (!is.null(res) && isTRUE(res$code == 201)) {
                                state <- s$get_state()
                            }
                        }
                        state
                    },
                    error = function(e) e
                )
                if (inherits(state, "error")) {
                    private$log_error(
                        private$name, " failed to poll task session: ", state)
                    private$retire_session(s)
                    next
                }
                if (state == "idle") {
                    return(s)
                }
            }
            NULL
        },
        prune_sessions = function() {
            for (i in rev(seq_along(private$sessions))) {
                session <- private$sessions[[i]]
                state <- tryCatch(session$get_state(), error = function(e) e)
                if (inherits(state, "error")) {
                    private$log_error(
                        private$name, " failed to inspect task session: ", state)
                    private$retire_session(session)
                    next
                }
                if (state == "finished") {
                    private$sessions[[i]] <- NULL
                } else if (state == "idle") {
                    idle_start <- attr(session, "idle_start")
                    if (is.null(idle_start)) {
                        attr(session, "idle_start") <- Sys.time()
                    } else if (length(private$sessions) > private$min_idle_sessions &&
                        as.numeric(difftime(
                            Sys.time(), idle_start,
                            units = "secs")) > private$session_idle_timeout) {
                        close_error <- tryCatch(
                            {
                                if (identical(Sys.getenv("R_COVR"), "true")) {
                                    session$close(grace = 10000)
                                } else {
                                    session$close()
                                }
                                NULL
                            },
                            error = function(e) e
                        )
                        private$sessions[[i]] <- NULL
                        if (!is.null(close_error)) {
                            private$log_error(
                                private$name,
                                " failed to close idle task session: ",
                                close_error
                            )
                            private$retire_session(session)
                        }
                    }
                } else {
                    attr(session, "idle_start") <- NULL
                }
            }
            private$ensure_min_sessions()
        }
    ),
    public = list(
        initialize = function(name,
                              use_session = FALSE,
                              process_recent_first = FALSE,
                              cpu_load = 0.5,
                              max_running_tasks = 8,
                              session_idle_timeout = 300,
                              min_idle_sessions = 1) {
            private$pending_tasks <- collections::ordered_dict()
            private$running_tasks <- collections::ordered_dict()
            private$name <- name
            private$use_session <- use_session
            private$process_recent_first <- process_recent_first
            private$cancelled_tasks <- list()
            private$stopping <- FALSE
            
            private$session_idle_timeout <- session_idle_timeout
            cpus <- suppressWarnings(parallel::detectCores())
            if (length(cpus) != 1L || is.na(cpus) || cpus < 1L) {
                cpus <- 1L
            }
            max_running_tasks <- min(cpus, max_running_tasks)
            private$max_running_tasks <- max(min(max_running_tasks, round(cpus * cpu_load)), 1)
            if (use_session) {
                private$sessions <- list()
                private$min_idle_sessions <- max(
                    min(as.integer(min_idle_sessions), private$max_running_tasks),
                    0L
                )
                private$ensure_min_sessions()
            }
        },
        add_task = function(id, task) {
            if (is.null(task) || private$stopping) {
                return(NULL)
            }
            # Replacing an ordered-dict value does not update insertion order.
            # Remove it first so process_recent_first reflects actual recency.
            self$cancel(id)
            private$pending_tasks$set(id, task)
        },
        cancel = function(id) {
            if (private$pending_tasks$has(id)) {
                private$pending_tasks$remove(id)
            }
            if (private$running_tasks$has(id)) {
                old_task <- private$running_tasks$pop(id)
                retired_session <- old_task$kill()
                if (!is.null(retired_session)) {
                    # A killed session can still report a buffered result and
                    # transition back to "idle". Remove it before that stale
                    # state makes it eligible for another task.
                    private$remove_session(retired_session)
                }
                private$cancelled_tasks <- append(
                    private$cancelled_tasks, old_task)
            }
            invisible(NULL)
        },
        run_tasks = function() {
            if (private$stopping) return(invisible(NULL))
            n <- max(private$max_running_tasks - private$running_tasks$size(), 0)
            if (n == 0L) return(invisible(NULL))

            pending_ids <- private$pending_tasks$keys()
            if (!length(pending_ids)) return(invisible(NULL))

            eligible <- vapply(pending_ids, function(id) {
                task <- private$pending_tasks$get(id)
                as.numeric(difftime(
                    Sys.time(), task$time, units = "secs")) >= task$delay
            }, logical(1L))
            pending_ids <- pending_ids[eligible]
            if (!length(pending_ids)) return(invisible(NULL))

            # Performance: Prioritize newer tasks over older for better responsiveness
            # For parse tasks, process most recent documents first
            if (length(pending_ids) > n && isTRUE(private$process_recent_first)) {
                # Take the most recent n tasks and dispatch newest first.
                pending_ids <- rev(tail(pending_ids, n))
            } else if (length(pending_ids) > n) {
                pending_ids <- pending_ids[seq_len(n)]
            } else if (isTRUE(private$process_recent_first)) {
                pending_ids <- rev(pending_ids)
            }

            if (isTRUE(private$use_session)) {
                # Provision to actual queued demand. This allows a burst of
                # independent tasks to start workers in parallel without
                # growing the pool for a single pending task.
                private$ensure_demand_sessions(length(pending_ids))
            }

            for (id in pending_ids) {
                task <- private$pending_tasks$get(id)
                session <- NULL

                if (isTRUE(private$use_session)) {
                    session <- private$find_available_session()
                    if (is.null(session)) {
                        next
                    }
                }

                task <- private$pending_tasks$pop(id)
                private$running_tasks$set(id, task)
                start_error <- tryCatch(
                    {
                        task$start(session)
                        NULL
                    },
                    error = function(e) e
                )
                if (!is.null(start_error)) {
                    private$running_tasks$remove(id)
                    if (!is.null(session)) {
                        private$retire_session(session)
                    }
                    task$fail(start_error)
                }
            }
        },
        check_tasks = function() {
            running_tasks <- private$running_tasks
            keys <- private$running_tasks$keys()
            for (key in keys) {
                task <- running_tasks$get(key)
                check_result <- tryCatch(task$check(), error = function(e) e)
                if (inherits(check_result, "error")) {
                    private$log_error(
                        private$name, " failed to check task ", key, ": ",
                        check_result
                    )
                    task$fail(check_result)
                    retired_session <- tryCatch(
                        task$kill(),
                        error = function(e) {
                            private$log_error(
                                private$name, " failed to stop task ", key,
                                ": ", e)
                            NULL
                        }
                    )
                    if (!is.null(retired_session)) {
                        private$remove_session(retired_session)
                    }
                    check_result <- TRUE
                }
                if (isTRUE(check_result)) {
                    # FIXME: debug
                    logger$info(private$name, "task timing:", Sys.time() - task$time, " ", key)
                    running_tasks$remove(key)
                }
            }
            if (length(private$cancelled_tasks)) {
                complete <- vapply(
                    private$cancelled_tasks,
                    function(task) {
                        tryCatch(
                            task$check(),
                            error = function(e) {
                                private$log_error(
                                    private$name,
                                    " failed to reap cancelled task: ", e)
                                tryCatch(task$kill(), error = function(e) NULL)
                                TRUE
                            }
                        )
                    },
                    logical(1L)
                )
                private$cancelled_tasks <- private$cancelled_tasks[!complete]
            }
            if (isTRUE(private$use_session)) {
                private$prune_sessions()
            }
        },
        has_work = function() {
            private$pending_tasks$size() > 0L ||
                private$running_tasks$size() > 0L
        },
        stop = function() {
            if (private$stopping) return(invisible(NULL))
            private$stopping <- TRUE
            private$pending_tasks$clear()
            for (id in private$running_tasks$keys()) {
                task <- private$running_tasks$get(id)
                tryCatch(
                    task$kill(),
                    error = function(e) private$log_error(
                        private$name, " failed to stop task ", id, ": ", e)
                )
            }
            private$running_tasks$clear()
            for (task in private$cancelled_tasks) {
                tryCatch(
                    task$kill(),
                    error = function(e) private$log_error(
                        private$name, " failed to stop cancelled task: ", e)
                )
            }
            private$cancelled_tasks <- list()
            if (private$use_session) {
                sessions <- private$sessions
                private$sessions <- list()
                for (session in sessions) {
                    tryCatch(
                        {
                            if (identical(Sys.getenv("R_COVR"), "true")) {
                                while (session$get_state() %in% c("starting", "busy")) {
                                    session$poll_process(1000)
                                    tryCatch(session$read(), error = function(e) NULL)
                                }
                                session$close(grace = 10000)
                            } else {
                                session$close()
                            }
                        },
                        error = function(e) {
                            private$log_error(
                                private$name,
                                " failed to close task session: ", e)
                            private$retire_session(session)
                        }
                    )
                }
            }
            invisible(NULL)
        }
    )
)

package_call <- function(target) {
    target_name <- as.character(substitute(target))
    eval(bquote(
        function(...) get(.(target_name), envir = asNamespace("languageserver"))(...)
    ), envir = baseenv())
}

create_task <- function(target, args, callback = NULL, error = NULL, delay = 0) {
    Task$new(
        target = target,
        args = args,
        callback = callback,
        error = error,
        delay = delay
    )
}
