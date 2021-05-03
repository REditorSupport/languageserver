protocol_set_trace <- function(self, id, params) {
    value <- params$value
    if (value == "off") {
        logger$disable_debug()
    } else {
        logger$enable_debug()
    }
}
