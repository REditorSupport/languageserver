rd_topic <- function(topic, package) {
    rd_file <- as.character(help((topic), (package)))
    utils:::.getHelpFile(rd_file)
}

rd_section <- function(rd, section) {
    ret <- rd[sapply(rd, function(x) attr(x, "Rd_tag") == paste0("\\", section))]
    if (length(ret) == 0) {
        list()
    } else {
        ret[[1]]
    }
}

rd_parse_arguments <- function(x) {
    out <- list()
    for (d in x) {
        attr(d[[1]][[1]], "Rd_tag") %in% c("TEXT", "\\dots") || next
        out <- c(out, list(rd_parse_argument(d)))
    }
    out
}

rd_parse_argument <- function(x){
    if (attr(x[[1]][[1]], "Rd_tag") == "\\dots") {
        argument_name <- "..."
    } else {
        argument_name <- trimws(as.character(x[[1]][[1]]))
    }
    list(
        arg = argument_name,
        content = rd_parse_text(x[[2]])
    )
}

rd_parse_text <- function(x){
    y <- trimws(paste(unlist(x), collapse = ""))
    y <- gsub("\\\\", "\\\\\\\\", y)
    gsub("\n", "\\\\n", y)
}
