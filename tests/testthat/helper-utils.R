request_with_timeout <- function(f, client, timeout_seconds = 10,
                                 condition = function(x) FALSE) {
    start_time <- Sys.time()
    f() # make request
    data <- jsonlite::fromJSON(client$fetch(blocking = TRUE), simplifyVector = FALSE)
    while(Sys.time() - start_time < timeout_seconds && (length(data$result) < 1 || condition(data))) {
        f() # make request again
        data <- jsonlite::fromJSON(client$fetch(blocking = TRUE), simplifyVector = FALSE)
    }
    data
}
