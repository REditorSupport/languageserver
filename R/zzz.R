request_handlers <- list(
    initialize = on_initialize
)


notification_handlers <- list(
    initialized = on_initialized,
    exit = on_exit,
    `textDocument/didOpen` = textDocumentdidOpen,
    `textDocument/didSave` = textDocumentdidSave
)
