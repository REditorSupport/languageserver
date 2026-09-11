#include "json.h"

#include <Rversion.h>

#include <limits.h>
#include <math.h>
#include <string.h>

/* LSP responses are shallow. A bound also makes cyclic/deep inputs fall back
 * before recursive C calls can exhaust the native stack. */
#define JSON_MAX_DEPTH 128

typedef struct {
    size_t size;
    size_t limit;
    size_t visited;
} json_measure;

static int deferred_vector(SEXP value) {
#if R_VERSION >= R_Version(3, 5, 0)
    /* An extension's ALTREP accessor can execute code or change between
     * passes. Let jsonlite handle these rather than trusting a measured size. */
    return ALTREP(value);
#else
    return 0;
#endif
}

static int add_size(json_measure *measure, size_t amount) {
    if (amount > measure->limit - measure->size) return 0;
    measure->size += amount;
    return 1;
}

static int integer_text(int value, char *out) {
    char reversed[10];
    int count = 0;
    int size = 0;
    unsigned int magnitude = value < 0 ?
        (unsigned int) (-(value + 1)) + 1U : (unsigned int) value;
    do {
        reversed[count++] = (char) ('0' + magnitude % 10U);
        magnitude /= 10U;
    } while (magnitude);
    if (value < 0) out[size++] = '-';
    while (count) out[size++] = reversed[--count];
    return size;
}

static int protocol_class(SEXP value, SEXP classes, const char *name) {
    /* These exact wrappers are defined in R/interfaces.R. jsonlite's force
     * fallback encodes their underlying lists/strings without modification. */
    if (TYPEOF(value) == STRSXP && XLENGTH(classes) == 1 &&
            strcmp(name, "document_uri") == 0) return 1;
    if (TYPEOF(value) != VECSXP) return 0;
    if (strcmp(name, "range") == 0) {
        return XLENGTH(classes) == 2 && STRING_ELT(classes, 1) != NA_STRING &&
            strcmp(CHAR(STRING_ELT(classes, 1)), "list") == 0;
    }
    if (XLENGTH(classes) != 1) return 0;
    static const char *list_classes[] = {
        "position", "location", "symbol_information", "document_symbol", "text_edit",
        "text_document_position_params", "completion_params", "reference_params",
        "document_symbol_params", "code_action_params", "code_lens_params",
        "document_link_params", "document_formatting_params",
        "document_range_formatting_params", "document_on_type_formatting_params",
        "rename_params", "did_open_text_document_params", "did_change_text_document_params",
        "will_save_text_document_params", "did_save_text_document_params",
        "did_close_text_document_params", "did_change_configuration_params"
    };
    for (size_t i = 0; i < sizeof(list_classes) / sizeof(list_classes[0]); i++) {
        if (strcmp(name, list_classes[i]) == 0) return 1;
    }
    return 0;
}

/* Only these attributes have the same simple interpretation for every
 * supported vector type. All other classes/attributes go through jsonlite. */
static int array_marker(SEXP value, int *force_array) {
    *force_array = 0;
#if R_VERSION >= R_Version(4, 6, 0)
    R_xlen_t attribute_count = R_getAttribCount(value);
#else
    R_xlen_t attribute_count = 0;
    for (SEXP attr = ATTRIB(value); attr != R_NilValue; attr = CDR(attr)) {
        attribute_count++;
    }
#endif
    if (!attribute_count) return 1;
    if (attribute_count > 2) return 0;
    SEXP names = Rf_getAttrib(value, R_NamesSymbol);
    SEXP classes = Rf_getAttrib(value, R_ClassSymbol);
    if (attribute_count != (names != R_NilValue) + (classes != R_NilValue)) return 0;
    if (names != R_NilValue && (TYPEOF(names) != STRSXP || deferred_vector(names) ||
            XLENGTH(names) != XLENGTH(value))) return 0;
    if (classes == R_NilValue) return 1;
    if (TYPEOF(classes) != STRSXP || deferred_vector(classes) || XLENGTH(classes) < 1 ||
            STRING_ELT(classes, 0) == NA_STRING) return 0;
    const char *class_name = CHAR(STRING_ELT(classes, 0));
    if (strcmp(class_name, "AsIs") == 0) {
        if (XLENGTH(classes) != 1) return 0;
        *force_array = 1;
    } else if (strcmp(class_name, "scalar") == 0) {
        if (XLENGTH(value) != 1 || TYPEOF(value) == VECSXP) return 0;
        if (XLENGTH(classes) == 2) {
            const char *base_class = TYPEOF(value) == INTSXP ? "integer" :
                TYPEOF(value) == REALSXP ? "numeric" :
                TYPEOF(value) == LGLSXP ? "logical" : "character";
            if (STRING_ELT(classes, 1) == NA_STRING ||
                    strcmp(CHAR(STRING_ELT(classes, 1)), base_class) != 0) return 0;
        } else if (XLENGTH(classes) != 1) {
            return 0;
        }
    } else if (!protocol_class(value, classes, class_name)) {
        return 0;
    }
    return 1;
}

static int simple_names(SEXP names) {
    R_xlen_t count = XLENGTH(names);
    for (R_xlen_t i = 0; i < count; i++) {
        SEXP name = STRING_ELT(names, i);
        if (name == NA_STRING || LENGTH(name) == 0 || Rf_getCharCE(name) == CE_BYTES) return 0;
    }
    /* jsonlite repairs empty and duplicate object keys using make.unique().
     * Defer these uncommon cases instead of changing their names. */
    if (count > 16) return Rf_any_duplicated(names, FALSE) == 0;
    for (R_xlen_t i = 1; i < count; i++) {
        const char *name = Rf_translateCharUTF8(STRING_ELT(names, i));
        for (R_xlen_t j = 0; j < i; j++) {
            if (strcmp(name, Rf_translateCharUTF8(STRING_ELT(names, j))) == 0) return 0;
        }
    }
    return 1;
}

static int measure_string(SEXP string, json_measure *measure) {
    if (string == NA_STRING) return add_size(measure, 4);
    if (Rf_getCharCE(string) == CE_BYTES) return 0;
    const unsigned char *text = (const unsigned char *) Rf_translateCharUTF8(string);
    size_t size = 2;
    for (const unsigned char *p = text; *p; p++) {
        unsigned char ch = *p;
        if (ch == '"' || ch == '\\' || ch == '\b' || ch == '\t' ||
                ch == '\n' || ch == '\f' || ch == '\r') {
            size += 2;
        } else {
            size += ch < 0x20 ? 6 : 1;
        }
        if (size > measure->limit - measure->size) return 0;
        if (((size_t) (p - text) & 65535U) == 65535U) R_CheckUserInterrupt();
    }
    return add_size(measure, size);
}

static int measure_value(SEXP value, json_measure *measure, int depth) {
    if (depth > JSON_MAX_DEPTH) return 0;
    if ((++measure->visited & 16383U) == 0) R_CheckUserInterrupt();
    if (value == R_NilValue) return add_size(measure, 4);
    if (Rf_isS4(value) || (TYPEOF(value) != VECSXP && TYPEOF(value) != LGLSXP &&
            TYPEOF(value) != INTSXP && TYPEOF(value) != REALSXP &&
            TYPEOF(value) != STRSXP) || deferred_vector(value)) return 0;
    int force_array;
    if (!array_marker(value, &force_array)) return 0;
    R_xlen_t count = XLENGTH(value);
    if (count > INT_MAX) return 0;
    char number[11];

    if (TYPEOF(value) == VECSXP) {
        SEXP names = Rf_getAttrib(value, R_NamesSymbol);
        int named = names != R_NilValue;
        if (named && !simple_names(names)) return 0;
        if (!add_size(measure, 2 + (count ? (size_t) count - 1 : 0))) return 0;
        for (R_xlen_t i = 0; i < count; i++) {
            if (named) {
                SEXP name = STRING_ELT(names, i);
                if (!measure_string(name, measure)) return 0;
                if (!add_size(measure, 1)) return 0;
            }
            if (!measure_value(VECTOR_ELT(value, i), measure, depth + 1)) return 0;
        }
        return 1;
    }

    if (TYPEOF(value) != LGLSXP && TYPEOF(value) != INTSXP &&
            TYPEOF(value) != REALSXP && TYPEOF(value) != STRSXP) return 0;
    int array = force_array || count != 1;
    if (array && !add_size(measure, 2 + (count ? (size_t) count - 1 : 0))) return 0;
    for (R_xlen_t i = 0; i < count; i++) {
        if ((i & 16383) == 16383) R_CheckUserInterrupt();
        switch (TYPEOF(value)) {
        case LGLSXP: {
            int item = LOGICAL(value)[i];
            if (item != 0 && item != 1 && item != NA_LOGICAL) return 0;
            if (!add_size(measure, item == 0 ? 5 : 4)) return 0;
            break;
        }
        case INTSXP: {
            int item = INTEGER(value)[i];
            /* jsonlite's numeric NA policy differs from its other vectors. */
            if (item == NA_INTEGER ||
                    !add_size(measure, (size_t) integer_text(item, number))) return 0;
            break;
        }
        case REALSXP: {
            double item = REAL(value)[i];
            /* Avoid changing jsonlite's decimal/scientific notation policy. */
            if (!R_FINITE(item) || item < INT_MIN || item > INT_MAX ||
                    item != trunc(item)) return 0;
            size_t size = item == 0 && signbit(item) ? 2 :
                (size_t) integer_text((int) item, number);
            if (!add_size(measure, size)) return 0;
            break;
        }
        case STRSXP:
            if (!measure_string(STRING_ELT(value, i), measure)) return 0;
            break;
        default:
            return 0;
        }
    }
    return 1;
}

static char *write_string(SEXP string, char *out) {
    if (string == NA_STRING) {
        memcpy(out, "null", 4);
        return out + 4;
    }
    static const char hex[] = "0123456789abcdef";
    const unsigned char *text = (const unsigned char *) Rf_translateCharUTF8(string);
    *out++ = '"';
    for (const unsigned char *p = text; *p; p++) {
        unsigned char ch = *p;
        if (ch == '"' || ch == '\\') {
            *out++ = '\\';
            *out++ = (char) ch;
        } else if (ch < 0x20) {
            *out++ = '\\';
            switch (ch) {
            case '\b': *out++ = 'b'; break;
            case '\t': *out++ = 't'; break;
            case '\n': *out++ = 'n'; break;
            case '\f': *out++ = 'f'; break;
            case '\r': *out++ = 'r'; break;
            default:
                *out++ = 'u';
                *out++ = '0';
                *out++ = '0';
                *out++ = hex[ch >> 4];
                *out++ = hex[ch & 15];
            }
        } else {
            *out++ = (char) ch;
        }
        if (((size_t) (p - text) & 65535U) == 65535U) R_CheckUserInterrupt();
    }
    *out++ = '"';
    return out;
}

/* The preflight accepted every node and counted the exact allocation. Neither
 * pass calls user code, so values cannot change between measuring and writing. */
static char *write_value(SEXP value, char *out, size_t *visited) {
    if ((++*visited & 16383U) == 0) R_CheckUserInterrupt();
    if (value == R_NilValue) {
        memcpy(out, "null", 4);
        return out + 4;
    }
    R_xlen_t count = XLENGTH(value);
    if (TYPEOF(value) == VECSXP) {
        SEXP names = Rf_getAttrib(value, R_NamesSymbol);
        int named = names != R_NilValue;
        *out++ = named ? '{' : '[';
        for (R_xlen_t i = 0; i < count; i++) {
            if (i) *out++ = ',';
            if (named) {
                SEXP name = STRING_ELT(names, i);
                out = write_string(name, out);
                *out++ = ':';
            }
            out = write_value(VECTOR_ELT(value, i), out, visited);
        }
        *out++ = named ? '}' : ']';
        return out;
    }
    int force_array;
    array_marker(value, &force_array);
    int array = force_array || count != 1;
    if (array) *out++ = '[';
    for (R_xlen_t i = 0; i < count; i++) {
        if ((i & 16383) == 16383) R_CheckUserInterrupt();
        if (i) *out++ = ',';
        switch (TYPEOF(value)) {
        case LGLSXP: {
            int item = LOGICAL(value)[i];
            const char *text = item == NA_LOGICAL ? "null" : item ? "true" : "false";
            int size = item == 0 ? 5 : 4;
            memcpy(out, text, (size_t) size);
            out += size;
            break;
        }
        case INTSXP:
            out += integer_text(INTEGER(value)[i], out);
            break;
        case REALSXP:
            if (REAL(value)[i] == 0 && signbit(REAL(value)[i])) {
                *out++ = '-';
                *out++ = '0';
            } else {
                out += integer_text((int) REAL(value)[i], out);
            }
            break;
        case STRSXP:
            out = write_string(STRING_ELT(value, i), out);
            break;
        }
    }
    if (array) *out++ = ']';
    return out;
}

SEXP response_json_c(SEXP value, SEXP max_bytes) {
    if (TYPEOF(max_bytes) != INTSXP || XLENGTH(max_bytes) != 1 ||
            INTEGER(max_bytes)[0] == NA_INTEGER || INTEGER(max_bytes)[0] < 0) {
        Rf_error("JSON size limit must be a non-negative integer");
    }
    json_measure measure = {0, (size_t) INTEGER(max_bytes)[0], 0};
    if (!measure_value(value, &measure, 0)) return R_NilValue;
    SEXP buffer = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) measure.size));
    char *start = (char *) RAW(buffer);
    size_t visited = 0;
    char *end = write_value(value, start, &visited);
    if ((size_t) (end - start) != measure.size) Rf_error("JSON size mismatch");
    SEXP result = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(
        start, (int) measure.size, CE_UTF8)));
    SEXP json_class = PROTECT(Rf_mkString("json"));
    Rf_setAttrib(result, R_ClassSymbol, json_class);
    UNPROTECT(3);
    return result;
}
