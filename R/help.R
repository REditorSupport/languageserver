get_help <- function(topic, package = NULL) {
    if (is.null(package) || is.na(package)) {
        hfile <- help((topic))
    } else {
        hfile <- help((topic), (package))
    }
    if (length(hfile) > 0) {
        repr::repr_text(hfile)
    } else {
        NULL
    }
}
