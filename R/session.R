Session <- R6::R6Class("Session",
    private = list(
        # parent session poll - dependency injection
        parent_pool = NULL,
        # session id - be released by parent
        id = NULL,
        session = NULL,
        # prev function call is running
        running = FALSE,
        result = NULL
    ),
    public = list(
        initialize = function(parent_pool, id) {
            private$parent_pool <- parent_pool
            private$id <- id
            private$session <- callr::r_session$new(callr::r_session_options(system_profile = TRUE, user_profile = TRUE))
        },
        start = function(target, args) {
            if (private$running) {
                logger$info("warning: prev function call is running in session", private$id)
            }
            # FIXME: check session is alive or create new session for dead session
            private$running <- TRUE
            private$session$call(target, args)
        },
        # TRUE for running process, FALSE for compeletion
        is_alive = function() {
            if (private$running) {
                data <- private$session$read()
                if (!is.null(data)) {
                    # FIXME: check data$code == 200
                    # https://callr.r-lib.org/reference/r_session.html#method-read
                    private$running <- FALSE
                    if (!is.null(data$error)) {
                        private$result <- data$error
                    } else {
                        private$result <- data$result
                    }
                    # FALSE for compeletion
                    return(FALSE)
                }
            }
            # TRUE for running process
            return(TRUE)
        },
        get_result = function() {
            private$result
        },
        kill = function() {
            logger$info("session kill", private$id)

            private$session$close(grace = 100)
            private$session <- callr::r_session$new(callr::r_session_options(system_profile = TRUE, user_profile = TRUE))
            private$running <- FALSE
            self$release()
        },
        # release current session from session pool
        release = function() {
            private$parent_pool$release(private$id)
        }
    )
)

SessionPool <- R6::R6Class("SessionPool",
    private = list(
        idle_keys = NULL,
        sessions = NULL,
        pending_size = 0,
        idle_size = 0,
        size = 0
    ),
    public = list(
        initialize = function(size) {
            private$sessions <- collections::ordered_dict()
            private$idle_keys <- collections::queue()

            if (size > 0) {
                private$size <- size
                for (i in seq_len(size)) {
                    istr <- as.character(i)
                    # FIXME: handle new session error
                    private$sessions$set(istr, Session$new(self, istr))
                    private$idle_keys$push(istr)
                    private$idle_size <- private$idle_size + 1
                }
            }
        },
        get_idle_size = function() {
            private$idle_size
        },
        acquire = function() {
            if (private$idle_size > 0) {
                private$idle_size <- private$idle_size - 1
                session_id <- private$idle_keys$pop()
                if (!is.null(session_id)) {
                    return(private$sessions$get(session_id))
                }
            }
            return(NULL)
        },
        # called by child session
        release = function(id) {
            # FIXME: remove dead locked session with timeout
            logger$info("session released", id)
            private$idle_keys$push(id)
            private$idle_size <- private$idle_size + 1
        }
    )
)
