#include "message_buffer.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <ctype.h>

typedef struct {
    unsigned char *data;
    size_t start, end, capacity, scan, line_start;
    size_t body_length;
    int body, has_length;
} message_buffer;

static SEXP buffer_tag(void) {
    return Rf_install("languageserver_message_buffer");
}

static message_buffer *get_buffer(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != buffer_tag() ||
            R_ExternalPtrAddr(pointer) == NULL) {
        Rf_error("invalid message buffer");
    }
    return (message_buffer *) R_ExternalPtrAddr(pointer);
}

static void free_buffer(SEXP pointer) {
    message_buffer *buffer = (message_buffer *) R_ExternalPtrAddr(pointer);
    if (buffer != NULL) {
        free(buffer->data);
        free(buffer);
        R_ClearExternalPtr(pointer);
    }
}

SEXP new_message_buffer_c(void) {
    /* Allocate the R owner first so an allocation error cannot leak C memory. */
    SEXP pointer = PROTECT(R_MakeExternalPtr(NULL, buffer_tag(), R_NilValue));
    message_buffer *buffer = (message_buffer *) calloc(1, sizeof(message_buffer));
    if (buffer == NULL) Rf_error("cannot allocate message buffer");
    R_SetExternalPtrAddr(pointer, buffer);
    R_RegisterCFinalizerEx(pointer, free_buffer, TRUE);
    UNPROTECT(1);
    return pointer;
}

SEXP message_buffer_append_c(SEXP pointer, SEXP chunk) {
    message_buffer *buffer = get_buffer(pointer);
    if (TYPEOF(chunk) != RAWSXP) Rf_error("message chunk must be raw bytes");
    size_t n = (size_t) XLENGTH(chunk);
    if (!n) return R_NilValue;
    size_t pending = buffer->end - buffer->start;
    if (n > SIZE_MAX - pending) Rf_error("message buffer is too large");
    size_t required = pending + n;
    if (n > buffer->capacity - buffer->end) {
        if (required <= buffer->capacity) {
            /* Reclaim already consumed frames only when the tail is full. */
            memmove(buffer->data, buffer->data + buffer->start, pending);
        } else {
            size_t capacity = buffer->capacity ? buffer->capacity : 4096;
            while (capacity < required) {
                if (capacity > SIZE_MAX / 2) { capacity = required; break; }
                capacity *= 2;
            }
            unsigned char *data = (unsigned char *) malloc(capacity);
            if (data == NULL) Rf_error("cannot grow message buffer");
            if (pending) memcpy(data, buffer->data + buffer->start, pending);
            free(buffer->data);
            buffer->data = data;
            buffer->capacity = capacity;
        }
        buffer->scan -= buffer->start;
        buffer->line_start -= buffer->start;
        buffer->end = pending;
        buffer->start = 0;
    }
    memcpy(buffer->data + buffer->end, RAW(chunk), n);
    buffer->end += n;
    return R_NilValue;
}

static int equal_header(const unsigned char *text, size_t n, const char *name) {
    size_t length = strlen(name);
    if (n != length) return 0;
    for (size_t i = 0; i < n; i++) {
        if (tolower(text[i]) != tolower((unsigned char) name[i])) return 0;
    }
    return 1;
}

static void parse_header(message_buffer *buffer, size_t end) {
    const unsigned char *text = buffer->data + buffer->line_start;
    size_t n = end - buffer->line_start;
    if (n && text[n - 1] == '\r') n--;
    if (!n) {
        if (!buffer->has_length) Rf_error("missing Content-Length header");
        buffer->body = 1;
        buffer->start = end + 1;
        return;
    }
    size_t colon = 0;
    while (colon < n && text[colon] != ':') colon++;
    if (colon == n) Rf_error("Unexpected non-empty line");
    if (equal_header(text, colon, "Content-Length")) {
        size_t i = colon + 1;
        while (i < n && (text[i] == ' ' || text[i] == '\t')) i++;
        size_t length = 0, first = i;
        while (i < n && text[i] >= '0' && text[i] <= '9') {
            unsigned int digit = text[i++] - '0';
            if (length > ((size_t) INT_MAX - digit) / 10)
                Rf_error("Content-Length exceeds the supported string size");
            length = length * 10 + digit;
        }
        if (i == first) Rf_error("invalid Content-Length header");
        while (i < n && (text[i] == ' ' || text[i] == '\t')) i++;
        if (i != n) Rf_error("invalid Content-Length header");
        if (buffer->has_length && buffer->body_length != length)
            Rf_error("conflicting Content-Length headers");
        buffer->body_length = length;
        buffer->has_length = 1;
    } else {
        /* Keep compatibility with optional Content-* headers. */
        const char *prefix = "Content";
        if (colon < strlen(prefix) || !equal_header(text, strlen(prefix), prefix)) {
            Rf_error("Unexpected non-empty line");
        }
    }
}

SEXP message_buffer_fetch_c(SEXP pointer) {
    message_buffer *buffer = get_buffer(pointer);
    if (!buffer->body) {
        /* The scan cursor advances over partial lines once, including UTF-8. */
        while (buffer->scan < buffer->end) {
            size_t current = buffer->scan++;
            if (buffer->data[current] == '\n') {
                parse_header(buffer, current);
                buffer->line_start = buffer->scan;
                if (buffer->body) break;
            }
        }
        if (!buffer->body) return R_NilValue;
    }
    if (buffer->end - buffer->start < buffer->body_length) return R_NilValue;
    SEXP value = PROTECT(Rf_mkCharLenCE((const char *) buffer->data + buffer->start,
        (int) buffer->body_length, CE_UTF8));
    SEXP result = PROTECT(Rf_ScalarString(value));
    buffer->start += buffer->body_length;
    if (buffer->start == buffer->end) buffer->start = buffer->end = 0;
    buffer->scan = buffer->line_start = buffer->start;
    buffer->body = buffer->has_length = 0;
    buffer->body_length = 0;
    UNPROTECT(2);
    return result;
}

SEXP message_buffer_pending_c(SEXP pointer) {
    message_buffer *buffer = get_buffer(pointer);
    return Rf_ScalarLogical(buffer->end != buffer->start || buffer->body ||
        buffer->has_length);
}
