request_handlers <- list(
    initialize = on_initialize
)


notification_handlers <- list(
    initialized = on_initialized,
    exit = on_exit,
    `textDocument/didOpen` = textDocumentdidOpen,
    `textDocument/didSave` = textDocumentdidSave
)

general <- list(
    initialized = on_initialized,
    initialize = on_initialize,
    exit = on_exit
)

text_document <- list(
    `textDocument/didOpen` = text_document_did_open,
    `textDocument/didSave` = text_document_did_save
)
