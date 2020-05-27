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
        start = function() {
            private$process <- callr::r_bg(
                private$target,
                private$args,
                system_profile = TRUE, user_profile = TRUE
            )
        },
        check = function() {
            if (is.null(private$process)) {
                FALSE
            } else if (private$process$is_alive()) {
                FALSE
            } else {
                result <- tryCatch(private$process$get_result(), error = function(e) e)
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
        cpus = NULL,
        pending_tasks = NULL,
        running_tasks = NULL
    ),
    public = list(
        initialize = function() {
            private$cpus <- parallel::detectCores()
            private$pending_tasks <- collections::ordered_dict()
            private$running_tasks <- collections::ordered_dict()
        },
        add_task = function(id, task) {
            private$pending_tasks$set(id, task)
        },
        run_tasks = function(cpu_load = 0.5) {
            n <- max(max(private$cpus * cpu_load, 1) - private$running_tasks$size(), 0)
            ids <- private$pending_tasks$keys()
            if (length(ids) > n) {
                ids <- ids[seq_len(n)]
            }
            for (id in ids) {
                task <- private$pending_tasks$get(id)
                if (Sys.time() - task$time >= task$delay) {
                    if (private$running_tasks$has(id)) {
                        task <- private$running_tasks$pop(id)
                        task$kill()
                    }
                    task <- private$pending_tasks$pop(id)
                    private$running_tasks$set(id, task)
                    task$start()
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
                    logger$info("task timing: ", Sys.time() - task$time, " ", key)
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
