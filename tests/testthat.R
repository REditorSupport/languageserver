library(testthat)
library(languageserver)

filter <- Sys.getenv("TESTTHAT_FILTER", "")
timing_file <- Sys.getenv("TESTTHAT_TIMINGS", "")
if (nzchar(timing_file)) {
    # CheckReporter normally replays a file's events after it finishes. Stream
    # them instead so the clock measures execution, including server shutdown.
    TimingReporter <- R6::R6Class("TimingReporter", inherit = CheckReporter,
        public = list(
            started = NULL,
            current_file = NULL,
            initialize = function() {
                super$initialize()
                self$capabilities$parallel_updates <- TRUE
                self$started <- new.env(parent = emptyenv())
                write.csv(data.frame(file = character(), seconds = numeric()),
                    timing_file, row.names = FALSE)
            },
            start_file = function(name) {
                self$current_file <- name
                if (!exists(name, self$started, inherits = FALSE)) {
                    self$started[[name]] <- proc.time()[[3]]
                }
            },
            end_file = function() {
                name <- self$current_file
                seconds <- proc.time()[[3]] - self$started[[name]]
                write.table(data.frame(file = name, seconds = seconds),
                    timing_file, append = TRUE, sep = ",", row.names = FALSE,
                    col.names = FALSE)
                rm(list = name, envir = self$started)
            }
        ))
    test_check("languageserver", filter = if (nzchar(filter)) filter else NULL,
        reporter = TimingReporter$new())
} else if (nzchar(filter)) {
    test_check("languageserver", filter = filter)
} else {
    test_check("languageserver")
}
