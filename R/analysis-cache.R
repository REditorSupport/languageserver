# Analysis adapters supply the semantic context and key that make a result
# reusable. Stores are allocated on demand, bounded independently by domain,
# and retain serialized values so mutable request state is never shared.
analysis_cache_format <- 2L

analysis_cache <- function(owner, domain, context = NULL, max_bytes = 2 * 1024^2, max_entries = 128L) {
    if (is.null(owner$analysis_caches)) owner$analysis_caches <- new.env(parent = emptyenv())
    entry <- owner$analysis_caches[[domain]]
    limits <- list(bytes = max(as.numeric(max_bytes), 0), entries = max(as.integer(max_entries), 1L))
    runtime <- list(namespace = asNamespace("languageserver"), format = analysis_cache_format)
    if (is.null(entry) || !identical(entry$context, context) || !identical(entry$limits, limits) ||
            !identical(entry$runtime, runtime)) {
        # Keep old handles isolated: an in-flight computation may still write
        # its result after a newer context has been installed.
        entry <- list(context = context, limits = limits, runtime = runtime,
            values = ByteLruCache$new(limits$bytes, limits$entries))
    }
    owner$analysis_caches[[domain]] <- entry
    entry$values
}

analysis_cache_get <- function(cache, key) {
    if (is.null(cache) || is.null(key) || !cache$has(key)) return(list(hit = FALSE))
    tryCatch({
        record <- cache$get(key)
        if (!is.list(record) || is.object(record) || !identical(record$format, analysis_cache_format) || !is.raw(record$payload) ||
                !identical(record$checksum, digest::digest(record$payload, algo = "xxhash64", serialize = FALSE))) {
            cache$remove(key)
            return(list(hit = FALSE))
        }
        list(hit = TRUE, value = unserialize(record$payload))
    }, error = function(e) {
        cache$remove(key)
        list(hit = FALSE)
    })
}

analysis_cache_set <- function(cache, key, value) {
    if (!is.null(cache) && !is.null(key) && cache$capacity()$bytes > 0 &&
            as.numeric(utils::object.size(value)) <= cache$capacity()$bytes) {
        payload <- serialize(value, NULL)
        if (length(payload) <= cache$capacity()$bytes) {
            cache$set(key, list(format = analysis_cache_format, payload = payload,
                    checksum = digest::digest(payload, algo = "xxhash64", serialize = FALSE)))
        }
    }
    invisible(value)
}

# Exact syntax is a context-independent input. Only formals are parsed; neither
# function bodies nor defaults are evaluated. Both syntax and XML providers use
# these facts across reparses, independently of declaration position.
analysis_function_header <- function(document, text, symbol) {
    context <- list(format = lapply(c("width", "digits", "scipen", "deparse.cutoff"), getOption),
        formatter = get_signature)
    cache <- analysis_cache(document, "function-headers", context)
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
