# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

Queue <- R6::R6Class("Queue",
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        put = function(x) {
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(x))
            } else {
                private$last <- .Call(
                    "pairlist_append", PACKAGE = "languageserver", private$last, x)
            }
            invisible(x)
        },
        get = function() {
            v <- .Call("pairlist_car", PACKAGE = "languageserver", private$q)
            private$q <- .Call("pairlist_cdr", PACKAGE = "languageserver", private$q)
            v
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)

MutableQueue <- R6::R6Class("MutableQueue",
    inherit = Queue,
    public = list(
        put = function(id, x) {
            updated <- FALSE
            cur <- private$q
            for (i in seq_len(length(private$q))) {
                v <- .Call("pairlist_car", PACKAGE = "languageserver", cur)
                if (v$id == id) {
                    .Call("pairlist_update", PACKAGE = "languageserver", cur, list(id = id, x = x))
                    updated <- TRUE
                    break
                } else {
                    cur <- .Call("pairlist_cdr", PACKAGE = "languageserver", cur)
                }
            }
            if (!updated) {
                super$put(list(id = id, x = x))
            }
            invisible(x)
        }
    )
)
