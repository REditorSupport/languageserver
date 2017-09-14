general <- list(
    initialized = on_initialized,
    initialize = on_initialize,
    exit = on_exit
)

text_document <- list(
    `textDocument/didOpen` = text_document_did_open,
    `textDocument/didSave` = text_document_did_save
)
