strip_document_bom <- function(text) {
    if (length(text) && isTRUE(startsWith(text[[1L]], "\ufeff"))) {
        text[[1L]] <- substr(text[[1L]], 2L, nchar(text[[1L]]))
    }
    text
}

split_document_lines <- function(text) {
    split_text_lines(strip_document_bom(text))
}

split_text_lines <- function(text, keep_final_empty = TRUE) {
    if (is.na(text)) return(NA_character_)
    text <- enc2utf8(text)
    if (!nzchar(text)) return("")
    # Normalize Unicode line breaks before a fixed split. Splitting at each
    # regex match is much slower for large UTF-8 documents.
    text <- gsub("\r\n|[\r\v\f\u0085\u2028\u2029]", "\n", text, perl = TRUE)
    # The end of a newline-terminated buffer is an editable empty line.
    if (keep_final_empty) text <- paste0(text, "\n")
    strsplit(text, "\n", fixed = TRUE)[[1L]]
}

# UTF-32LE starts with the UTF-16LE signature, so check the longer marks first.
text_byte_order_marks <- list(
    "UTF-32BE" = as.raw(c(0, 0, 0xfe, 0xff)),
    "UTF-32LE" = as.raw(c(0xff, 0xfe, 0, 0)),
    "UTF-8" = as.raw(c(0xef, 0xbb, 0xbf)),
    "UTF-16BE" = as.raw(c(0xfe, 0xff)),
    "UTF-16LE" = as.raw(c(0xff, 0xfe))
)

text_bom_encoding <- function(bytes) {
    for (encoding in names(text_byte_order_marks)) {
        mark <- text_byte_order_marks[[encoding]]
        if (length(bytes) >= length(mark) && identical(bytes[seq_along(mark)], mark)) return(encoding)
    }
    NULL
}

decode_text_bytes <- function(bytes, encoding) {
    if (identical(encoding, "")) encoding <- NULL
    bom <- text_bom_encoding(bytes)
    if (!is.null(bom)) {
        # A matching Unicode declaration can omit the byte order. A conflicting
        # explicit charset owns the original bytes, including the apparent mark.
        declared <- gsub("[-_]", "", toupper(encoding))
        signature <- gsub("-", "", bom)
        if (is.null(encoding) || declared %in% c(signature, sub("(LE|BE)$", "", signature))) {
            encoding <- bom
            bytes <- bytes[-seq_along(text_byte_order_marks[[bom]])]
        }
    }
    # Unmarked files retain any configured ICU charset. Without ICU, prefer
    # UTF-8 so an indexed file agrees with the same source opened in the editor.
    if (is.null(encoding)) {
        use_icu_default <- isNamespaceLoaded("stringi") ||
            (!l10n_info()[["UTF-8"]] && requireNamespace("stringi", quietly = TRUE))
        if (use_icu_default) encoding <- stringi::stri_enc_get()
    }
    from <- if (is.null(encoding) || toupper(encoding) == "UTF8") "UTF-8" else toupper(encoding)
    if (from %in% names(text_byte_order_marks)) {
        text <- iconv(list(bytes), from = from, to = "UTF-8")
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
    writeLines(enc2utf8(strip_document_bom(text)), path, useBytes = TRUE)
}

count_newlines <- function(text) {
    without_newlines <- gsub("\n", "", text, fixed = TRUE, useBytes = TRUE)
    nchar(text, type = "bytes") - nchar(without_newlines, type = "bytes")
}

leading_whitespace <- function(text) {
    text <- enc2utf8(text)
    match <- regexpr(paste0("^", unicode_whitespace, "*"), text, perl = TRUE)
    substring(text, 1L, attr(match, "match.length"))
}
