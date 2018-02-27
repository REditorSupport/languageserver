# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

Queue <- R6::R6Class("Queue",
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        put = function(item) {
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(item))
            } else {
                private$last <- .Call(
                    "pairlist_append", PACKAGE = "languageserver", private$last, item)
            }
            invisible(item)
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

NamedQueue <- R6::R6Class("NamedQueue",
    inherit = Queue,
    public = list(
        put = function(id, item) {
            updated <- FALSE
            cur <- private$q
            for (i in seq_len(length(private$q))) {
                v <- .Call("pairlist_car", PACKAGE = "languageserver", cur)
                if (v$id == id) {
                    .Call("pairlist_setcar",
                        PACKAGE = "languageserver",
                        cur,
                        list(id = id, item = item))
                    updated <- TRUE
                    break
                } else {
                    cur <- .Call("pairlist_cdr", PACKAGE = "languageserver", cur)
                }
            }
            if (!updated) {
                super$put(list(id = id, item = item))
            }
            invisible(item)
        },
        get = function(id = NULL) {
            if (is.null(id)) {
                super$get()
            } else {
                cur <- private$q
                v <- .Call("pairlist_car", PACKAGE = "languageserver", cur)
                d <- .Call("pairlist_cdr", PACKAGE = "languageserver", cur)
                if (v$id == id) {
                    private$q <- d
                    return(v)
                } else {
                    while (!is.null(d)) {
                        v <- .Call("pairlist_car", PACKAGE = "languageserver", d)
                        dd <- .Call("pairlist_cdr", PACKAGE = "languageserver", d)
                        if (v$id == id) {
                            .Call("pairlist_setcdr", PACKAGE = "languageserver", cur, dd)
                            return(v)
                        }
                        cur <- d
                        d <- dd
                    }
                }
                stop("id not found")
            }
        },
        has = function(id) {
            cur <- private$q
            while (!is.null(cur)) {
                v <- .Call("pairlist_car", PACKAGE = "languageserver", cur)
                if (v$id == id) {
                    return(TRUE)
                }
                cur <- .Call("pairlist_cdr", PACKAGE = "languageserver", cur)
            }
            return(FALSE)
        }
    )
)
