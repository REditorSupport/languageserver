Task <- R6::R6Class("Task",
    private = list(
        process = NULL,
        target = NULL,
        args = NULL,
        callback = NULL,
        error = NULL
    ),
    public = list(
        initialize = function(target, args, callback = NULL, error = NULL) {
            private$target <- target
            private$args <- args
            private$callback <- callback
            private$error <- error
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
                    if (!is.null(private$error) && inherits(result, "error")) {
                        private$error(result)
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
        running_tasks = NULL
    ),
    public = list(
        initialize = function() {
            private$pending_tasks <- collections::OrderedDict()
            private$running_tasks <- collections::OrderedDict()
            self$run_tasks <- throttle(self$run_tasks_, 0.1)
            # self$run_tasks <- self$run_tasks_
        },
        add_task = function(id, task) {
            private$pending_tasks$set(id, task)
        },
        run_tasks = NULL,
        run_tasks_ = function(n = 8) {
            ids <- private$pending_tasks$keys()
            if (length(ids) > n) {
                ids <- ids[1:n]
            }
            for (id in ids) {
                if (private$running_tasks$has(id)) {
                    task <- private$running_tasks$pop(id)
                    task$kill()
                }
                task <- private$pending_tasks$pop(id)
                private$running_tasks$set(id, task)
                task$start()
            }
        },
        check_tasks = function() {
            running_tasks <- private$running_tasks
            keys <- private$running_tasks$keys()
            pending_tasks <- private$pending_tasks
            for (key in keys) {
                task <- running_tasks$get(key)
                if (task$check()) {
                    running_tasks$remove(key)
                }
            }
        }
    )
)


create_task <- function(target, args, callback = NULL, error = NULL) {
    func <- call(":::", as.name("languageserver"), substitute(target))
    target <- eval(substitute(function(...) func(...), list(func = func)))
    Task$new(target = target,
             args = args,
             callback = callback,
             error = error)
}
