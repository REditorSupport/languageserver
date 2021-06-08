protocol_set_trace <- function(self, id, params) {
    value <- params$value
    if (value == "off") {
        lsp_settings$set("debug", FALSE)
    } else {
        lsp_settings$set("debug", TRUE)
    }
}
