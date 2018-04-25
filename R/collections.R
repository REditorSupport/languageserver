# adopted from https://github.com/randy3k/collections

QueueL <- R6::R6Class("QueueL",
    cloneable = FALSE,
    private = list(
        q = list(),
        n = 0
    ),
    public = list(
        push = function(item) {
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            if (private$n == 0) stop("queue is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        size = function() private$n,
        as_list = function() private$q
    )
)

OrderedDictL <- R6::R6Class("OrderedDictL",
    cloneable = FALSE,
    private = list(
        e = list()
    ),
    public = list(
        set = function(key, value) {
            private$e[[key]] <- value
        },
        get = function(key, default = NULL) {
            private$e[[key]]
        },
        remove = function(key) {
            v <- self$keys() != key
            if (all(v)) stop("value not found")
            private$e <- private$e[v]
            invisible(NULL)
        },
        pop = function(key, default = NULL) {
            v <- self$get(key, default)
            self$remove(key)
            v
        },
        has = function(key) {
            key %in% self$keys()
        },
        keys = function() {
            names(private$e)
        },
        values = function() {
            ret <- list()
            i <- 0
            for (key in self$keys()) {
                i <- i + 1
                ret[[i]] <- self$get(key)
            }
            ret
        },
        update = function(d) {
            for (key in d$keys()) {
                self$set(key, d$get(key))
            }
            self
        },
        size = function() length(private$e),
        as_list = function() private$e
    )
)
