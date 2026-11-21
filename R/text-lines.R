strip_utf8_bom <- function(text) {
    text <- enc2utf8(text)
    bom <- which(startsWith(text, "\ufeff"))
    if (length(bom)) text[bom] <- substr(text[bom], 2L, nchar(text[bom]))
    text
}

split_text_lines <- function(text, keep_final_empty = TRUE) {
    if (is.na(text)) return(NA_character_)
    text <- strip_utf8_bom(text)
    if (!nzchar(text)) return("")
    # Normalize Unicode line breaks before a fixed split. Splitting at each
    # regex match is much slower for large UTF-8 documents.
    text <- gsub("\r\n|[\r\v\f\u0085\u2028\u2029]", "\n", text, perl = TRUE)
    # The end of a newline-terminated buffer is an editable empty line.
    if (keep_final_empty) text <- paste0(text, "\n")
    strsplit(text, "\n", fixed = TRUE)[[1L]]
}

decode_text_bytes <- function(bytes, encoding) {
    if (identical(encoding, "")) encoding <- NULL
    # Explicit and configured ICU charsets take precedence even for bytes
    # that incidentally form valid UTF-8. Without ICU, prefer UTF-8 on disk
    # so the index agrees with the same source text opened in the editor.
    if (is.null(encoding)) {
        use_icu_default <- isNamespaceLoaded("stringi") ||
            (!l10n_info()[["UTF-8"]] && requireNamespace("stringi", quietly = TRUE))
        if (use_icu_default) encoding <- stringi::stri_enc_get()
    }
    if (is.null(encoding) || toupper(encoding) %in% c("UTF-8", "UTF8")) {
        text <- iconv(list(bytes), from = "UTF-8", to = "UTF-8")
        if (!is.na(text)) return(text)
    }
    # ICU preserves the previous handling of malformed input and legacy
    # converters when available. Otherwise use R's native encoding by default.
    if (requireNamespace("stringi", quietly = TRUE)) {
        if (is.null(encoding)) encoding <- stringi::stri_enc_get()
        return(stringi::stri_encode(bytes, encoding, "UTF-8"))
    }
    if (is.null(encoding)) encoding <- if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) "UTF-8" else ""
    text <- iconv(list(bytes), from = encoding, to = "UTF-8")
    if (!is.na(text)) return(text)
    warning("Invalid input bytes replaced while decoding text", call. = FALSE)
    iconv(list(bytes), from = encoding, to = "UTF-8", sub = "\ufffd")
}

read_text_lines <- function(path, encoding = NULL) {
    con <- file(path, "rb")
    on.exit(close(con))
    chunks <- list()
    repeat {
        bytes <- readBin(con, "raw", n = 65536L)
        chunks[[length(chunks) + 1L]] <- bytes
        if (!length(bytes)) break
    }
    split_text_lines(decode_text_bytes(do.call(c, chunks), encoding), keep_final_empty = FALSE)
}

write_text_lines <- function(text, path) {
    stopifnot(!anyNA(text))
    writeLines(strip_utf8_bom(text), path, useBytes = TRUE)
}

count_newlines <- function(text) {
    without_newlines <- gsub("\n", "", text, fixed = TRUE, useBytes = TRUE)
    nchar(text, type = "bytes") - nchar(without_newlines, type = "bytes")
}

leading_whitespace <- function(text) {
    text <- strip_utf8_bom(text)
    match <- regexpr(paste0("^", unicode_whitespace, "*"), text, perl = TRUE)
    substring(text, 1L, attr(match, "match.length"))
}
