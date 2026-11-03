# Share isolated workers across equivalent requests while retaining document
# subscriptions. The scheduler still starts a fresh process for each unique job.
SharedTaskManager <- R6::R6Class("SharedTaskManager",
    private = list(
        scheduler = NULL,
        entries = NULL,
        aliases = NULL,
        cache = NULL,
        cache_ttl = NULL,
        serial = 0L,
        stopping = FALSE,
        valid = function(alias) {
            identical(private$aliases$get(alias$id, NULL), alias) &&
                isTRUE(tryCatch(alias$task$sharing$valid(), error = function(e) FALSE))
        },
        current = function(task) {
            validator <- task$sharing$current
            !is.function(validator) || isTRUE(tryCatch(validator(), error = function(e) FALSE))
        },
        log_error = function(e) {
            tryCatch(logger$info("shared task callback error: ", e), error = function(e) NULL)
            invisible(NULL)
        },
        remove_alias = function(alias) {
            if (identical(private$aliases$get(alias$id, NULL), alias)) {
                private$aliases$remove(alias$id)
            }
            if (identical(alias$entry$subscribers$get(alias$id, NULL), alias)) {
                alias$entry$subscribers$remove(alias$id)
            }
            invisible(NULL)
        },
        deliver = function(alias, value, failed = FALSE, context_current = TRUE) {
            # Member callbacks inspect their active task context. Keep the alias
            # visible throughout delivery, including synchronous cache hits.
            on.exit({
                private$remove_alias(alias)
                settled <- alias$task$sharing$settled
                if (is.function(settled)) tryCatch(settled(), error = private$log_error)
            }, add = TRUE)
            if (!context_current || !private$valid(alias)) return(invisible(NULL))
            handler <- if (failed) alias$task$sharing$error else alias$task$sharing$callback
            if (is.function(handler)) tryCatch(handler(value), error = private$log_error)
            invisible(NULL)
        },
        finish = function(entry, value, failed = FALSE) {
            if (isTRUE(entry$finished)) return(invisible(NULL))
            entry$finished <- TRUE
            if (identical(private$entries$get(entry$key, NULL), entry)) {
                private$entries$remove(entry$key)
            }
            aliases <- entry$subscribers$values()
            # Package installation validation is common to every subscriber.
            # Avoid repeating its filesystem work for a virtual-document burst.
            context_current <- private$current(entry$worker)
            valid <- if (context_current) Filter(private$valid, aliases) else list()
            cacheable <- !failed && length(valid) > 0L && all(vapply(valid, function(alias) {
                isTRUE(tryCatch(alias$task$sharing$cacheable(value), error = function(e) FALSE))
            }, logical(1L)))
            if (cacheable && !private$stopping) {
                # Reentrant requests from callbacks can use this result or queue
                # an explicitly refreshed successor without joining finished work.
                private$cache$set(entry$key, list(value = value, time = proc.time()[[3L]]))
            }
            for (alias in aliases) private$deliver(alias, value, failed, context_current)
            invisible(NULL)
        },
        synchronize = function() {
            for (alias in private$aliases$values()) {
                alias$task$execution_started_at <- alias$entry$worker$execution_started_at
            }
            invisible(NULL)
        },
        subscribe = function(id, task, entry) {
            alias <- new.env(parent = emptyenv())
            alias$id <- id
            alias$task <- task
            alias$entry <- entry
            previous <- private$aliases$get(id, NULL)
            if (!is.null(previous) && identical(previous$entry, entry)) {
                # Replacing the only subscriber must not kill equivalent work.
                private$remove_alias(previous)
            } else {
                self$cancel(id)
            }
            private$aliases$set(id, alias)
            entry$subscribers$set(id, alias)
            task$execution_started_at <- entry$worker$execution_started_at
            alias
        }
    ),
    public = list(
        initialize = function(name, ..., max_cache_bytes = 32 * 1024^2,
            max_cache_entries = 32L, cache_ttl = 60) {
            private$scheduler <- TaskManager$new(name, ...)
            private$entries <- collections::dict()
            private$aliases <- collections::dict()
            private$cache <- ByteLruCache$new(max_cache_bytes, max_cache_entries)
            private$cache_ttl <- max(as.numeric(cache_ttl), 0)
        },
        add_task = function(id, task) {
            if (private$stopping) return(invisible(NULL))
            sharing <- if (is.null(task)) NULL else task$sharing
            if (!is.environment(sharing) || !is.character(sharing$key) ||
                    length(sharing$key) != 1L || is.na(sharing$key) || !nzchar(sharing$key)) {
                self$cancel(id)
                private$scheduler$add_task(id, task)
                return(invisible(NULL))
            }
            key <- sharing$key
            if (isTRUE(sharing$refresh)) private$cache$remove(key)
            cached <- private$cache$get(key)
            if (!is.null(cached) && proc.time()[[3L]] - cached$time > private$cache_ttl) {
                private$cache$remove(key)
                cached <- NULL
            }
            if (!is.null(cached)) {
                entry <- new.env(parent = emptyenv())
                entry$key <- key
                entry$worker <- task
                entry$subscribers <- collections::dict()
                entry$finished <- TRUE
                alias <- private$subscribe(id, task, entry)
                private$deliver(alias, cached$value, context_current = private$current(task))
                return(invisible(NULL))
            }
            entry <- private$entries$get(key, NULL)
            if (!is.null(entry)) {
                private$subscribe(id, task, entry)
                if (task$delay < entry$worker$delay) {
                    private$scheduler$promote(entry$id)
                }
                return(invisible(NULL))
            }
            entry <- new.env(parent = emptyenv())
            entry$key <- key
            private$serial <- private$serial + 1L
            entry$id <- paste0("\001shared-", private$serial)
            entry$worker <- task
            entry$subscribers <- collections::dict()
            entry$finished <- FALSE
            private$subscribe(id, task, entry)
            private$entries$set(key, entry)
            sharing$complete <- function(value) private$finish(entry, value)
            sharing$fail <- function(error) private$finish(entry, error, failed = TRUE)
            private$scheduler$add_task(entry$id, task)
            invisible(NULL)
        },
        get_task = function(id) {
            alias <- private$aliases$get(id, NULL)
            if (is.null(alias)) return(private$scheduler$get_task(id))
            alias$task$execution_started_at <- alias$entry$worker$execution_started_at
            alias$task
        },
        has = function(id) {
            private$aliases$has(id) || private$scheduler$has(id)
        },
        promote = function(id) {
            alias <- private$aliases$get(id, NULL)
            if (is.null(alias)) private$scheduler$promote(id)
            else if (!isTRUE(alias$entry$finished)) private$scheduler$promote(alias$entry$id)
            invisible(NULL)
        },
        cancel = function(id) {
            alias <- private$aliases$get(id, NULL)
            if (is.null(alias)) {
                private$scheduler$cancel(id)
                return(invisible(NULL))
            }
            entry <- alias$entry
            private$remove_alias(alias)
            if (!entry$subscribers$size() && !isTRUE(entry$finished)) {
                if (identical(private$entries$get(entry$key, NULL), entry)) {
                    private$entries$remove(entry$key)
                }
                private$scheduler$cancel(entry$id)
            }
            invisible(NULL)
        },
        run_tasks = function() {
            private$scheduler$run_tasks()
            private$synchronize()
            invisible(NULL)
        },
        check_tasks = function() {
            private$scheduler$check_tasks()
            private$synchronize()
            invisible(NULL)
        },
        wait_timeout = function(max_timeout = 0.01) {
            private$scheduler$wait_timeout(max_timeout)
        },
        has_work = function() private$scheduler$has_work(),
        stop = function() {
            if (private$stopping) return(invisible(NULL))
            private$stopping <- TRUE
            private$aliases$clear()
            private$entries$clear()
            private$cache$clear()
            private$scheduler$stop()
            invisible(NULL)
        }
    )
)
