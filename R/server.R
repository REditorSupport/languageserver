str_empty <- function(s) {
    stringr::str_length(stringr::str_trim(s)) == 0
}

#' @export
server <- function(debug=FALSE){

    con <- file("stdin")
    open(con, blocking=TRUE)
    while (TRUE) {
        ret <- try({
            header <- readLines(con, n = 1)
            if (str_empty(header)) next
            if (debug) {
                cat("received", header, "\n", file=stderr())
            }

            matches <- stringr::str_match(header[1], "Content-Length: ([0-9]+)")
            if (is.na(matches[2])) stop("Unexpected input: ", header)

            empty_line <- readLines(con, n = 1)
            if (!str_empty(empty_line)) stop("Unexpected non-empty line")

            data <- readChar(con, as.numeric(matches[2]), useBytes = TRUE)
            if (debug) {
                cat("received", data, "\n", file=stderr())
            }
            handle_raw(data)
        })
        if(inherits(ret, "error")) break
    }
    close(con)
}

handle_raw <- function(data){

}
