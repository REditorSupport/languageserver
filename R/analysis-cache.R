# Analysis adapters supply the semantic context and key that make a result
# reusable. Stores are allocated on demand, bounded independently by domain,
# and retain serialized values so mutable request state is never shared.
analysis_cache <- function(owner, domain, context = NULL, max_bytes = 2 * 1024^2, max_entries = 128L) {
    if (is.null(owner$analysis_caches)) owner$analysis_caches <- new.env(parent = emptyenv())
    entry <- owner$analysis_caches[[domain]]
    if (is.null(entry)) {
        entry <- list(context = context, values = ByteLruCache$new(max_bytes, max_entries))
    } else if (!identical(entry$context, context)) {
        entry$values$clear()
        entry$context <- context
    }
    owner$analysis_caches[[domain]] <- entry
    entry$values
}

analysis_cache_get <- function(cache, key) {
    if (is.null(cache) || is.null(key) || !cache$has(key)) return(list(hit = FALSE))
    list(hit = TRUE, value = unserialize(cache$get(key)))
}

analysis_cache_set <- function(cache, key, value) {
    if (!is.null(cache) && !is.null(key)) cache$set(key, serialize(value, NULL))
    invisible(value)
}

# Exact syntax is a context-independent input. Only formals are parsed; neither
# function bodies nor defaults are evaluated. Both syntax and XML providers use
# these facts across reparses, independently of declaration position.
analysis_function_header <- function(document, text, symbol) {
    cache <- analysis_cache(document, "function-headers")
    key <- digest::digest(list(symbol, text), algo = "xxhash64")
    found <- analysis_cache_get(cache, key)
    if (found$hit) return(found$value)
    fn <- str2expression(c(text, "NULL"))[[1L]]
    value <- list(signature = get_signature(symbol, fn), formals = fn[[2L]])
    analysis_cache_set(cache, key, value)
}

analysis_documentation_owner <- new.env(parent = emptyenv())

analysis_documentation <- function(document, comments) {
    owner <- if (is.null(document)) analysis_documentation_owner else document
    cache <- analysis_cache(owner, "source-documentation", context = asNamespace("roxygen2"))
    key <- digest::digest(comments, algo = "xxhash64")
    found <- analysis_cache_get(cache, key)
    if (found$hit) return(found$value)
    value <- if (length(comments)) convert_comment_to_documentation(comments)
    analysis_cache_set(cache, key, value)
}
