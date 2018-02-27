get_help <- function(topic, package = NULL) {
    if (is.null(package) || is.na(package)) {
        hfile <- utils::help((topic))
    } else {
        hfile <- utils::help((topic), (package))
    }
    if (length(hfile) > 0) {
        repr::repr_text(hfile)
    } else {
        NULL
    }
}
