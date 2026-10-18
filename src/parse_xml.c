#include "parse_xml.h"

#include <R_ext/Utils.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int row, closing, terminal;
    int line1, col1, line2, col2;
    int start, end;
} xml_event;

typedef struct {
    char *data;
    size_t size;
} xml_buffer;

static SEXP column(SEXP data, const char *name, SEXPTYPE type, R_xlen_t n) {
    SEXP names = Rf_getAttrib(data, R_NamesSymbol);
    if (TYPEOF(data) == VECSXP && TYPEOF(names) == STRSXP && XLENGTH(names) == XLENGTH(data)) {
        for (R_xlen_t i = 0; i < XLENGTH(names); ++i) {
            if (!strcmp(CHAR(STRING_ELT(names, i)), name)) {
                SEXP value = VECTOR_ELT(data, i);
                if (TYPEOF(value) == type && (n < 0 || XLENGTH(value) == n)) return value;
                break;
            }
        }
    }
    Rf_error("invalid XML parse-data column: %s", name);
    return R_NilValue;
}

static const char *xml_token(const char *token) {
    static const char *operators[] = {
        "'?'", "'~'", "'+'", "'-'", "'*'", "'/'", "':'", "'^'", "'$'", "'@'",
        "'('", "'['", "';'", "'{'", "'}'", "')'", "'!'", "']'", "','", "'\\\\'"
    };
    static const char *names[] = {
        "OP-QUESTION", "OP-TILDE", "OP-PLUS", "OP-MINUS", "OP-STAR", "OP-SLASH",
        "OP-COLON", "OP-CARET", "OP-DOLLAR", "OP-AT", "OP-LEFT-PAREN", "OP-LEFT-BRACKET",
        "OP-SEMICOLON", "OP-LEFT-BRACE", "OP-RIGHT-BRACE", "OP-RIGHT-PAREN",
        "OP-EXCLAMATION", "OP-RIGHT-BRACKET", "OP-COMMA", "OP-LAMBDA"
    };
    if (token[0] == '\'') {
        for (size_t i = 0; i < sizeof(operators) / sizeof(operators[0]); ++i) {
            if (!strcmp(token, operators[i])) return names[i];
        }
        return NULL;
    }
    /* Only parser token names are accepted as markup. Future token spellings
     * can use the reference writer until their mapping is supported here. */
    for (size_t i = 0; token[i]; ++i) {
        unsigned char c = (unsigned char) token[i];
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' ||
                (i && ((c >= '0' && c <= '9') || c == '-' || c == '.'))) continue;
        return NULL;
    }
    return token[0] ? token : NULL;
}

static int event_compare(const void *left, const void *right) {
    const xml_event *a = left, *b = right;
    if (a->line1 != b->line1) return a->line1 < b->line1 ? -1 : 1;
    if (a->col1 != b->col1) return a->col1 < b->col1 ? -1 : 1;
    if (a->line2 != b->line2) return a->line2 > b->line2 ? -1 : 1;
    if (a->col2 != b->col2) return a->col2 > b->col2 ? -1 : 1;
    if (a->terminal != b->terminal) return a->terminal < b->terminal ? -1 : 1;
    if (a->start != b->start) return a->start > b->start ? -1 : 1;
    /* Match R's stable order when source ranges coincide. */
    if (a->closing != b->closing) return a->closing < b->closing ? -1 : 1;
    return a->row < b->row ? -1 : a->row > b->row;
}

static void append(xml_buffer *buffer, const char *text, size_t size) {
    if (size > INT_MAX - buffer->size) Rf_error("XML parse data exceeds string capacity");
    if (buffer->data) memcpy(buffer->data + buffer->size, text, size);
    buffer->size += size;
}

static void text(xml_buffer *buffer, const char *value) {
    append(buffer, value, strlen(value));
}

static void number(xml_buffer *buffer, int value) {
    char digits[10];
    size_t n = 0;
    do {
        digits[n++] = (char) ('0' + value % 10);
        value /= 10;
    } while (value);
    if (buffer->data) {
        for (size_t i = 0; i < n / 2; ++i) {
            char digit = digits[i];
            digits[i] = digits[n - i - 1];
            digits[n - i - 1] = digit;
        }
    }
    append(buffer, digits, n);
}

static void escaped(xml_buffer *buffer, const char *value) {
    const char *start = value;
    for (const char *p = value; ; ++p) {
        unsigned char c = (unsigned char) *p;
        const char *replacement = NULL;
        if (c == '&') replacement = "&amp;";
        else if (c == '<') replacement = "&lt;";
        else if (c == '>') replacement = "&gt;";
        else if (c && c < 32 && c != '\t' && c != '\n') replacement = "";
        if (!c || replacement) {
            append(buffer, start, (size_t) (p - start));
            if (!c) return;
            text(buffer, replacement);
            start = p + 1;
        }
    }
}

/* Emit the same event stream as xmlparsedata, including detached comments,
 * empty/missing arguments, operator names, escaping, and stable tie order.
 * Two passes allocate one output buffer instead of one R string per tag. */
SEXP syntax_xml_c(SEXP data) {
    SEXP tokens = column(data, "token", STRSXP, -1);
    R_xlen_t n = XLENGTH(tokens);
    if (n > INT_MAX / 2) Rf_error("XML parse data exceeds index capacity");
    SEXP terminal = column(data, "terminal", LGLSXP, n);
    SEXP texts = column(data, "text", STRSXP, n);
    SEXP line1 = column(data, "line1", INTSXP, n), col1 = column(data, "col1", INTSXP, n);
    SEXP line2 = column(data, "line2", INTSXP, n), col2 = column(data, "col2", INTSXP, n);
    int maxcol = 0;
    for (R_xlen_t i = 0; i < n; ++i) {
        if (INTEGER(line1)[i] < 0 || INTEGER(line2)[i] < 0 ||
                INTEGER(col1)[i] < 0 || INTEGER(col2)[i] < 0 ||
                LOGICAL(terminal)[i] == NA_LOGICAL) Rf_error("invalid XML parse-data position");
        if (INTEGER(col1)[i] > maxcol) maxcol = INTEGER(col1)[i];
        if (INTEGER(col2)[i] > maxcol) maxcol = INTEGER(col2)[i];
        if (!xml_token(CHAR(STRING_ELT(tokens, i)))) return R_NilValue;
    }
    if (maxcol == INT_MAX) return R_NilValue;
    ++maxcol;
    xml_event *events = (xml_event *) R_alloc(n ? 2 * n : 1, sizeof(xml_event));
    size_t count = 0;
    for (int i = 0; i < n; ++i) {
        double start = (double) INTEGER(line1)[i] * maxcol + INTEGER(col1)[i];
        double end = (double) INTEGER(line2)[i] * maxcol + INTEGER(col2)[i];
        if (start > INT_MAX || end > INT_MAX) return R_NilValue;
        xml_event event = {i, 0, LOGICAL(terminal)[i], INTEGER(line1)[i], INTEGER(col1)[i],
            INTEGER(line2)[i], INTEGER(col2)[i], (int) start, (int) end};
        events[count++] = event;
        if (!event.terminal) {
            event.closing = event.terminal = 1;
            event.line1 = event.line2;
            event.col1 = event.col2;
            --event.line2;
            --event.col2;
            events[count++] = event;
        }
    }
    if (count > 1) qsort(events, count, sizeof(xml_event), event_compare);
    xml_buffer buffer = {NULL, 0};
    for (int pass = 0; pass < 2; ++pass) {
        if (pass) buffer.data = (char *) R_alloc(buffer.size + 1, 1);
        buffer.size = 0;
        text(&buffer, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n<exprlist>\n");
        for (size_t j = 0; j < count; ++j) {
            if ((j & 4095) == 0) R_CheckUserInterrupt();
            xml_event *event = &events[j];
            int i = event->row;
            const char *tag = xml_token(CHAR(STRING_ELT(tokens, i)));
            if (j) text(&buffer, "\n");
            text(&buffer, event->closing ? "</" : "<");
            text(&buffer, tag);
            if (event->closing) {
                text(&buffer, ">");
                continue;
            }
            /* Parser positions use integer arithmetic. Write decimal digits
             * directly, without allocating millions of scalar R strings. */
            text(&buffer, " line1=\"");
            number(&buffer, event->line1);
            text(&buffer, "\" col1=\"");
            number(&buffer, event->col1);
            text(&buffer, "\" line2=\"");
            number(&buffer, event->line2);
            text(&buffer, "\" col2=\"");
            number(&buffer, event->col2);
            text(&buffer, "\" start=\"");
            number(&buffer, event->start);
            text(&buffer, "\" end=\"");
            number(&buffer, event->end);
            text(&buffer, "\">");
            escaped(&buffer, Rf_translateCharUTF8(STRING_ELT(texts, i)));
            if (event->terminal) {
                text(&buffer, "</");
                text(&buffer, tag);
                text(&buffer, ">");
            }
        }
        text(&buffer, "\n</exprlist>\n");
    }
    buffer.data[buffer.size] = '\0';
    SEXP result = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(buffer.data, (int) buffer.size, CE_UTF8)));
    UNPROTECT(1);
    return result;
}
