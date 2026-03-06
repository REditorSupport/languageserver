Task <- R6::R6Class("Task",
    private = list(
        process = NULL,
        session = NULL,
        target = NULL,
        args = NULL,
        callback = NULL,
        error = NULL
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
            self$delay <- delay
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
                    if (res$code == 200 && is.null(res$error)) {
                        if (!is.null(private$callback)) private$callback(res$result)
                        return(TRUE)
                    } else if (!is.null(res$code)) {
                        if (!is.null(private$error)) {
                            err <- res$error
                            if (is.null(err)) err <- simpleError(paste("Session error with code", res$code))
                            private$error(err)
                        }
                        return(TRUE)
                    }
                }
                state <- private$session$get_state()
                if (identical(state, "finished")) {
                    if (!is.null(private$error)) {
                        err <- simpleError("Session finished unexpectedly while task was running")
                        private$error(err)
                    }
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
                    if (!is.null(private$error)) {
                        private$error(result)
                    }
                } else if (!is.null(private$callback)) {
                    private$callback(result)
                }
                TRUE
            }
        },
        kill = function() {
            if (!is.null(private$session)) {
                if (!identical(Sys.getenv("R_COVR"), "true")) {
                    # Do not close the session, it is persistent and managed by TaskManager.
                    # Just try to interrupt the ongoing computation.
                    private$session$interrupt()
                }
            } else if (!is.null(private$process) && private$process$is_alive()) {
                if (identical(Sys.getenv("R_COVR"), "true")) {
                    private$process$wait()
                } else {
                    private$process$wait(1000)
                    private$process$kill()
                }
            }
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
        find_or_create_session = function() {
            if (!isTRUE(private$use_session)) {
                return(NULL)
            }

            for (s in private$sessions) {
                state <- s$get_state()
                if (state == "starting") {
                    res <- s$read()
                    if (!is.null(res) && res$code == 201) state <- s$get_state()
                }
                if (state == "idle") {
                    return(s)
                }
            }

            if (length(private$sessions) < private$max_running_tasks) {
                session <- callr::r_session$new(
                    options = callr::r_session_options(
                        system_profile = TRUE,
                        user_profile = TRUE
                    ),
                    wait = TRUE
                )
                private$sessions <- append(private$sessions, session)
                return(session)
            }

            NULL
        },
        prune_sessions = function() {
            for (i in rev(seq_along(private$sessions))) {
                session <- private$sessions[[i]]
                state <- session$get_state()
                if (state == "finished") {
                    private$sessions[[i]] <- NULL
                } else if (state == "idle") {
                    idle_start <- attr(session, "idle_start")
                    if (is.null(idle_start)) {
                        attr(session, "idle_start") <- Sys.time()
                    } else if (as.numeric(difftime(Sys.time(), idle_start, units = "secs")) > private$session_idle_timeout) {
                        session$close()
                        private$sessions[[i]] <- NULL
                    }
                } else {
                    attr(session, "idle_start") <- NULL
                }
            }
        }
    ),
    public = list(
        initialize = function(name,
                              use_session = FALSE,
                              process_recent_first = FALSE,
                              cpu_load = 0.5,
                              max_running_tasks = 8,
                              session_idle_timeout = 60) {
            private$pending_tasks <- collections::ordered_dict()
            private$running_tasks <- collections::ordered_dict()
            private$name <- name
            private$use_session <- use_session
            private$process_recent_first <- process_recent_first
            
            private$session_idle_timeout <- session_idle_timeout
            cpus <- min(parallel::detectCores())
            max_running_tasks <- min(cpus, max_running_tasks)
            private$max_running_tasks <- max(min(max_running_tasks, round(cpus * cpu_load)), 1)
            if (use_session) {
                private$sessions <- list()
            }
        },
        add_task = function(id, task) {
            if (is.null(task)) {
                return(NULL)
            }
            private$pending_tasks$set(id, task)
        },
        run_tasks = function() {
            n <- max(private$max_running_tasks - private$running_tasks$size(), 0)

            pending_ids <- private$pending_tasks$keys()
            # Performance: Prioritize newer tasks over older for better responsiveness
            # For parse tasks, process most recent documents first
            if (length(pending_ids) > n && isTRUE(private$process_recent_first)) {
                # Take the most recent n tasks
                pending_ids <- tail(pending_ids, n)
            } else if (length(pending_ids) > n) {
                pending_ids <- pending_ids[seq_len(n)]
            }

            for (id in pending_ids) {
                task <- private$pending_tasks$get(id)
                if (Sys.time() - task$time >= task$delay) {
                    session <- NULL

                    if (isTRUE(private$use_session)) {
                        session <- private$find_or_create_session()
                        if (is.null(session) || session$get_state() == "starting") {
                            next
                        }
                    }

                    if (private$running_tasks$has(id)) {
                        old_task <- private$running_tasks$pop(id)
                        old_task$kill()
                    }
                    task <- private$pending_tasks$pop(id)
                    private$running_tasks$set(id, task)
                    task$start(session)
                }
            }
        },
        check_tasks = function() {
            running_tasks <- private$running_tasks
            keys <- private$running_tasks$keys()
            for (key in keys) {
                task <- running_tasks$get(key)
                if (task$check()) {
                    # FIXME: debug
                    logger$info(private$name, "task timing:", Sys.time() - task$time, " ", key)
                    running_tasks$remove(key)
                }
            }
            if (isTRUE(private$use_session)) {
                private$prune_sessions()
            }
        },
        stop = function() {
            for (id in private$running_tasks$keys()) {
                task <- private$running_tasks$get(id)
                task$kill()
            }
            if (private$use_session) {
                for (session in private$sessions) {
                    if (!identical(Sys.getenv("R_COVR"), "true")) {
                        session$close()
                    }
                }
            }
        }
    )
)

package_call <- function(target) {
    func <- call(":::", as.name("languageserver"), substitute(target))
    target <- eval(substitute(function(...) func(...), list(func = func)))
    target
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
