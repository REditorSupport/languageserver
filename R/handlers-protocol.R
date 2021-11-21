protocol_set_trace <- function(self, params) {
    value <- params$value
    if (value == "off") {
        lsp_settings$set("trace", FALSE)
    } else {
        lsp_settings$set("trace", TRUE)
    }
}
