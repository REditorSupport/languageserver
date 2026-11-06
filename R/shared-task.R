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
        new_entry = function(key, task, predecessor = NULL) {
            entry <- new.env(parent = emptyenv())
            entry$key <- key
            private$serial <- private$serial + 1L
            entry$id <- paste0("\001shared-", private$serial)
            entry$worker <- task
            entry$subscribers <- collections::ordered_dict()
            entry$finished <- FALSE
            entry$scheduled <- FALSE
            entry$refresh_requested <- FALSE
            entry$predecessor <- predecessor
            entry$successor <- NULL
            entry$delay <- task$delay
            entry
        },
        enqueue = function(entry) {
            sharing <- entry$worker$sharing
            sharing$complete <- function(value) private$finish(entry, value)
            sharing$fail <- function(error) private$finish(entry, error, failed = TRUE)
            entry$worker$delay <- min(entry$worker$delay, entry$delay)
            entry$scheduled <- TRUE
            private$scheduler$add_task(entry$id, entry$worker)
            invisible(NULL)
        },
        launch_successor = function(entry) {
            successor <- entry$successor
            if (is.null(successor)) return(invisible(NULL))
            entry$successor <- NULL
            successor$predecessor <- NULL
            if (private$stopping || !successor$subscribers$size()) return(invisible(NULL))
            aliases <- successor$subscribers$values()
            live <- Filter(private$valid, aliases)
            refreshed <- Filter(function(alias) isTRUE(alias$task$sharing$refresh), live)
            candidates <- if (length(refreshed)) refreshed else live
            if (length(candidates)) successor$worker <- tail(candidates, 1L)[[1L]]$task
            # The latest live refresh captures the worker inputs. An install or
            # library change while it waited requires regenerated descriptors.
            if (!length(candidates) || !private$current(successor$worker)) {
                successor$finished <- TRUE
                for (alias in aliases) private$deliver(alias, NULL, context_current = FALSE)
                return(invisible(NULL))
            }
            private$entries$set(successor$key, successor)
            private$enqueue(successor)
            invisible(NULL)
        },
        retire = function(entry) {
            entry$finished <- TRUE
            if (identical(private$entries$get(entry$key, NULL), entry)) {
                private$entries$remove(entry$key)
            }
            if (isTRUE(entry$scheduled)) private$scheduler$cancel(entry$id)
            predecessor <- entry$predecessor
            if (!is.null(predecessor) && identical(predecessor$successor, entry)) {
                predecessor$successor <- NULL
                if (!predecessor$subscribers$size() && !isTRUE(predecessor$finished)) {
                    private$retire(predecessor)
                }
            }
            invisible(NULL)
        },
        deliver = function(alias, value, failed = FALSE, context_current = TRUE) {
            # Member callbacks inspect their active task context. Keep the alias
            # visible throughout delivery, including synchronous cache hits.
            obsolete <- FALSE
            on.exit({
                owned <- identical(private$aliases$get(alias$id, NULL), alias)
                private$remove_alias(alias)
                invalidated <- alias$task$sharing$invalidated
                if (obsolete && owned && !private$stopping && is.function(invalidated)) {
                    tryCatch(invalidated(), error = private$log_error)
                }
                settled <- alias$task$sharing$settled
                if (is.function(settled)) tryCatch(settled(), error = private$log_error)
            }, add = TRUE)
            if (!context_current || !private$valid(alias)) {
                obsolete <- TRUE
                return(invisible(NULL))
            }
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
            cacheable <- !failed && !isTRUE(entry$refresh_requested) &&
                length(valid) > 0L && all(vapply(valid, function(alias) {
                isTRUE(tryCatch(alias$task$sharing$cacheable(value), error = function(e) FALSE))
            }, logical(1L)))
            if (cacheable && !private$stopping) {
                # Reentrant requests from callbacks can use this result or queue
                # an explicitly refreshed successor without joining finished work.
                private$cache$set(entry$key, list(value = value, time = proc.time()[[3L]]))
            }
            private$launch_successor(entry)
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
            if (!is.null(previous) && (identical(previous$entry, entry) ||
                        identical(previous$entry, entry$predecessor))) {
                # Replacing the only subscriber must not kill equivalent work.
                # A forced subscriber also keeps its predecessor alive until
                # the coalesced successor can use the latest refresh inputs.
                private$remove_alias(previous)
            } else {
                self$cancel(id)
            }
            private$aliases$set(id, alias)
            entry$subscribers$set(id, alias)
            entry$delay <- min(entry$delay, task$delay)
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
                entry <- private$new_entry(key, task)
                entry$finished <- TRUE
                alias <- private$subscribe(id, task, entry)
                private$deliver(alias, cached$value, context_current = private$current(task))
                return(invisible(NULL))
            }
            entry <- private$entries$get(key, NULL)
            if (!is.null(entry)) {
                if (!is.null(entry$worker$execution_started_at) &&
                        (isTRUE(sharing$refresh) || isTRUE(entry$refresh_requested))) {
                    entry$refresh_requested <- TRUE
                    if (is.null(entry$successor)) {
                        entry$successor <- private$new_entry(key, task, predecessor = entry)
                    }
                    entry <- entry$successor
                }
                private$subscribe(id, task, entry)
                if (task$delay < entry$worker$delay) {
                    entry$delay <- 0
                    entry$worker$delay <- 0
                    if (isTRUE(entry$scheduled)) private$scheduler$promote(entry$id)
                }
                return(invisible(NULL))
            }
            entry <- private$new_entry(key, task)
            private$subscribe(id, task, entry)
            private$entries$set(key, entry)
            private$enqueue(entry)
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
            else if (!isTRUE(alias$entry$finished)) {
                alias$entry$delay <- 0
                alias$entry$worker$delay <- 0
                if (isTRUE(alias$entry$scheduled)) private$scheduler$promote(alias$entry$id)
            }
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
                if (is.null(entry$successor) || !entry$successor$subscribers$size()) {
                    private$retire(entry)
                }
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
