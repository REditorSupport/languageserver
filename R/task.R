Task <- R6::R6Class("Task",
    private = list(
        process = NULL,
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
        start = function(session) {
            private$process <- session
            private$process$start(private$target, private$args)
        },
        check = function() {
            if (is.null(private$process)) {
                FALSE
            } else if (private$process$is_alive()) {
                FALSE
            } else {
                result <- private$process$get_result()
                # release session
                private$process$release()
                if (!is.null(private$callback)) {
                    if (inherits(result, "error")) {
                        if (!is.null(private$error)) {
                            private$error(result)
                        }
                    } else {
                        private$callback(result)
                    }
                }
                TRUE
            }
        },
        kill = function() {
            private$process$kill()
        }
    )
)

TaskManager <- R6::R6Class("TaskManager",
    private = list(
        pending_tasks = NULL,
        running_tasks = NULL,
        session_pool = NULL,
        name = NULL
    ),
    public = list(
        initialize = function(name, session_pool) {
            private$pending_tasks <- collections::ordered_dict()
            private$running_tasks <- collections::ordered_dict()
            private$session_pool <- session_pool
            private$name <- name
        },
        add_task = function(id, task) {
            private$pending_tasks$set(id, task)
        },
        run_tasks = function() {
            n <- private$session_pool$get_idle_size()
            ids <- private$pending_tasks$keys()
            if (length(ids) > n) {
                ids <- ids[seq_len(n)]
            }
            for (id in ids) {
                task <- private$pending_tasks$get(id)
                if (Sys.time() - task$time >= task$delay) {
                    session <- private$session_pool$acquire()
                    if (!is.null(session)) {
                        if (private$running_tasks$has(id)) {
                            task <- private$running_tasks$pop(id)
                            task$kill()
                        }
                        task <- private$pending_tasks$pop(id)
                        private$running_tasks$set(id, task)
                        # acquired session, will need to be released on check
                        task$start(session)
                    }
                }
            }
        },
        check_tasks = function() {
            running_tasks <- private$running_tasks
            keys <- private$running_tasks$keys()
            pending_tasks <- private$pending_tasks
            for (key in keys) {
                task <- running_tasks$get(key)
                if (task$check()) {
                    # FIXME: debug
                    logger$info(private$name, "task timing:", Sys.time() - task$time, " ", key)
                    running_tasks$remove(key)
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
