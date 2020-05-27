Session <- R6::R6Class("Session",
    private = list(
        # parent session poll - dependency injection
        parent_pool = NULL,
        # session id - be released by parent
        id = NULL,
        session = NULL,
        # prev task is running
        is_running = FALSE,
        result = NULL,
        # r session is ready
        is_ready = FALSE,
        # initial task before r session is ready
        init_task = NULL
    ),
    public = list(
        initialize = function(parent_pool, id) {
            private$parent_pool <- parent_pool
            private$id <- id

            private$session <- callr::r_session$new(
                callr::r_session_options(system_profile = TRUE, user_profile = TRUE),
                # skip waiting
                wait = FALSE
            )
        },
        start = function(target, args) {
            if (private$is_running) {
                logger$info("warning: prev function call is running in session", private$id)
            }
            private$is_running <- TRUE
            private$result <- NULL

            # FIXME: check session is alive or create new session for dead session
            if (private$is_ready) {
                private$session$call(target, args)
            } else {
                private$init_task <- list(target = target, args = args)
            }
        },
        # TRUE for running process, FALSE for compeletion
        is_alive = function() {
            if (!private$is_running) {
                # FALSE for compeletion
                return(FALSE)
            }

            # FIXME: handle data$code != 200
            # https://callr.r-lib.org/reference/r_session.html#method-read
            data <- private$session$read()
            if (!is.null(data)) {
                # session is not ready
                if (!private$is_ready) {
                    if (data$code == 201) {
                        logger$info("session ready", private$id, Sys.time())

                        private$is_ready <- TRUE
                        private$session$call(private$init_task$target, private$init_task$args)
                        private$init_task <- NULL
                    } else {
                        logger$info("session init error", private$id, data)
                        # maybe restart on error?
                        # self$restart()
                    }
                    # continue running
                    return(TRUE)
                }

                # session is ready
                if (data$code == 200) {
                    if (!is.null(data$error)) {
                        private$result <- data$error
                    } else {
                        private$result <- data$result
                    }
                } else {
                    # error data code 301, 500, 501, 502
                    logger$info("session error", private$id, data)
                    # maybe restart on error?
                    # self$restart()
                }
                private$is_running <- FALSE
                # FALSE for compeletion
                return(FALSE)
            }

            # TRUE for running process
            return(private$is_running)
        },
        get_result = function() {
            private$result
        },
        restart = function(should_release = FALSE) {
            logger$info("session restart", private$id)
            private$session$close(grace = 100)
            private$session <- callr::r_session$new(
                callr::r_session_options(system_profile = TRUE, user_profile = TRUE),
                wait = FALSE
            )
            private$is_ready <- FALSE

            if (should_release) {
                private$is_running <- FALSE
                self$release()
            }
        },
        kill = function() {
            logger$info("session kill", private$id)
            self$restart(should_release = TRUE)
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