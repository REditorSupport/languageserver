#' @import callr
Task <- R6::R6Class("Task",
    private = list(
        process = NULL,
        session = NULL,
        target = NULL,
        args = NULL,
        callback = NULL,
        error = NULL,
        cancelled = FALSE
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
                    if (res$code == 200 && is.null(res$error)) {
                        if (!private$cancelled && !is.null(private$callback)) {
                            private$callback(res$result)
                        }
                        return(TRUE)
                    } else if (!is.null(res$code)) {
                        if (!private$cancelled && !is.null(private$error)) {
                            err <- res$error
                            if (is.null(err)) err <- simpleError(paste("Session error with code", res$code))
                            private$error(err)
                        }
                        return(TRUE)
                    }
                }
                state <- private$session$get_state()
                if (identical(state, "finished")) {
                    if (!private$cancelled && !is.null(private$error)) {
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
                    if (!private$cancelled && !is.null(private$error)) {
                        private$error(result)
                    }
                } else if (!private$cancelled && !is.null(private$callback)) {
                    private$callback(result)
                }
                TRUE
            }
        },
        kill = function() {
            private$cancelled <- TRUE
            if (!is.null(private$session)) {
                if (!identical(Sys.getenv("R_COVR"), "true")) {
                    # An interrupt can arrive after this call has completed and
                    # interrupt the next task on the persistent session. Retire
                    # the worker instead so cancellation cannot leak across tasks.
                    private$session$kill(
                        grace = 0, close_connections = FALSE)
                }
            } else if (!is.null(private$process) && private$process$is_alive()) {
                if (identical(Sys.getenv("R_COVR"), "true")) {
                    private$process$wait()
                } else {
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
        min_idle_sessions = NULL,
        cancelled_tasks = NULL,
        stopping = FALSE,
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
        find_or_create_session = function() {
            if (!isTRUE(private$use_session)) {
                return(NULL)
            }

            starting <- FALSE
            for (s in private$sessions) {
                state <- s$get_state()
                if (state == "starting") {
                    res <- s$read()
                    if (!is.null(res) && res$code == 201) state <- s$get_state()
                    starting <- starting || state == "starting"
                }
                if (state == "idle") {
                    return(s)
                }
            }

            # A starting session can serve this task as soon as it becomes
            # idle. Creating another one on every event-loop pass would fill
            # the pool for a single pending task before the first R process
            # has even finished starting.
            if (!starting &&
                    length(private$sessions) < private$max_running_tasks) {
                private$create_session()
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
                    } else if (length(private$sessions) > private$min_idle_sessions &&
                        as.numeric(difftime(
                            Sys.time(), idle_start,
                            units = "secs")) > private$session_idle_timeout) {
                        if (identical(Sys.getenv("R_COVR"), "true")) {
                            session$close(grace = 10000)
                        } else {
                            session$close()
                        }
                        private$sessions[[i]] <- NULL
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
            if (is.null(task)) {
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
                old_task$kill()
                private$cancelled_tasks <- append(
                    private$cancelled_tasks, old_task)
            }
            invisible(NULL)
        },
        run_tasks = function() {
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
                # Take the most recent n tasks
                pending_ids <- tail(pending_ids, n)
            } else if (length(pending_ids) > n) {
                pending_ids <- pending_ids[seq_len(n)]
            }

            for (id in pending_ids) {
                task <- private$pending_tasks$get(id)
                session <- NULL

                if (isTRUE(private$use_session)) {
                    session <- private$find_or_create_session()
                    if (is.null(session) || session$get_state() == "starting") {
                        next
                    }
                }

                task <- private$pending_tasks$pop(id)
                private$running_tasks$set(id, task)
                task$start(session)
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
            if (length(private$cancelled_tasks)) {
                complete <- vapply(
                    private$cancelled_tasks,
                    function(task) task$check(),
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
            private$stopping <- TRUE
            for (id in private$running_tasks$keys()) {
                task <- private$running_tasks$get(id)
                task$kill()
            }
            for (task in private$cancelled_tasks) task$kill()
            if (private$use_session) {
                for (session in private$sessions) {
                    if (identical(Sys.getenv("R_COVR"), "true")) {
                        while (session$get_state() %in% c("starting", "busy")) {
                            session$poll_process(1000)
                            tryCatch(session$read(), error = function(e) NULL)
                        }
                        session$close(grace = 10000)
                    } else {
                        session$close()
                    }
                }
            }
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
