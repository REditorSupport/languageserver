#include <R.h>
#include <Rinternals.h>
#include <string.h>

/*
 * Convert code points to UTF-16 code units.
 *
 * Given a UTF-8 string and code point positions (0-indexed),
 * return the equivalent UTF-16 code unit positions.
 *
 * Args:
 *   line: character string (UTF-8)
 *   points: integer vector of code point positions
 *
 * Returns:
 *   Integer vector of UTF-16 code unit positions
 */
SEXP code_point_to_unit_c(SEXP line, SEXP points) {
    if (!Rf_isString(line) || Rf_length(line) != 1) {
        Rf_error("line must be a single character string");
    }
    
    if (!Rf_isInteger(points)) {
        Rf_error("points must be an integer vector");
    }

    const char* text = Rf_translateCharUTF8(STRING_ELT(line, 0));
    int text_len = strlen(text);
    int n_points = Rf_length(points);
    
    // Allocate result array
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n_points));
    int* result_ptr = INTEGER(result);
    int* points_ptr = INTEGER(points);
    
    // Build a mapping from code points to UTF-16 units
    // We iterate through the UTF-8 string once
    int code_point = 0;
    int utf16_unit = 0;
    int byte_idx = 0;
    
    // Arrays to cache calculations (for multi-point requests)
    int* cp_to_unit = (int*) malloc((text_len + 1) * sizeof(int));
    if (cp_to_unit == NULL) {
        UNPROTECT(1);
        Rf_error("Memory allocation failed");
    }
    
    // Build code point to UTF-16 unit mapping
    cp_to_unit[0] = 0;
    
    while (byte_idx < text_len) {
        unsigned char c = (unsigned char)text[byte_idx];
        
        if (c < 0x80) {
            // ASCII: 1 byte = 1 UTF-16 unit
            utf16_unit += 1;
            byte_idx += 1;
        } else if ((c & 0xE0) == 0xC0) {
            // 2-byte sequence: 1 UTF-16 unit
            utf16_unit += 1;
            byte_idx += 2;
        } else if ((c & 0xF0) == 0xE0) {
            // 3-byte sequence: 1 UTF-16 unit
            utf16_unit += 1;
            byte_idx += 3;
        } else if ((c & 0xF8) == 0xF0) {
            // 4-byte sequence: 2 UTF-16 units (surrogate pair)
            utf16_unit += 2;
            byte_idx += 4;
        } else if ((c & 0xC0) == 0x80) {
            // Continuation byte (shouldn't happen in well-formed UTF-8)
            byte_idx += 1;
        } else {
            // Invalid UTF-8, skip
            byte_idx += 1;
        }
        
        code_point++;
        if (code_point < text_len + 1) {
            cp_to_unit[code_point] = utf16_unit;
        }
    }
    
    int max_cp = code_point;
    int max_unit = utf16_unit;
    
    // Now extract results for requested points
    for (int i = 0; i < n_points; i++) {
        int pt = points_ptr[i];
        
        if (pt < 0) {
            result_ptr[i] = 0;
        } else if (pt >= max_cp) {
            result_ptr[i] = max_unit;
        } else {
            result_ptr[i] = cp_to_unit[pt];
        }
    }
    
    free(cp_to_unit);
    UNPROTECT(1);
    return result;
}

/*
 * Convert UTF-16 code units to code points.
 *
 * Given a UTF-8 string and UTF-16 code unit positions (0-indexed),
 * return the equivalent code point positions.
 *
 * Args:
 *   line: character string (UTF-8)
 *   units: integer vector of UTF-16 code unit positions
 *
 * Returns:
 *   Integer vector of code point positions
 */
SEXP code_point_from_unit_c(SEXP line, SEXP units) {
    if (!Rf_isString(line) || Rf_length(line) != 1) {
        Rf_error("line must be a single character string");
    }
    
    if (!Rf_isInteger(units)) {
        Rf_error("units must be an integer vector");
    }

    const char* text = Rf_translateCharUTF8(STRING_ELT(line, 0));
    int text_len = strlen(text);
    int n_units = Rf_length(units);
    
    // Allocate result array
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n_units));
    int* result_ptr = INTEGER(result);
    int* units_ptr = INTEGER(units);
    
    // First pass: determine maximum UTF-16 unit position
    int code_point = 0;
    int utf16_unit = 0;
    int byte_idx = 0;
    
    while (byte_idx < text_len) {
        unsigned char c = (unsigned char)text[byte_idx];
        int units_for_char = 1;
        
        if (c < 0x80) {
            byte_idx += 1;
        } else if ((c & 0xE0) == 0xC0) {
            byte_idx += 2;
        } else if ((c & 0xF0) == 0xE0) {
            byte_idx += 3;
        } else if ((c & 0xF8) == 0xF0) {
            units_for_char = 2;
            byte_idx += 4;
        } else if ((c & 0xC0) == 0x80) {
            byte_idx += 1;
        } else {
            byte_idx += 1;
        }
        
        utf16_unit += units_for_char;
        code_point++;
    }
    
    int max_unit = utf16_unit;
    int max_cp = code_point;
    
    // Allocate and initialize unit_to_cp array
    int* unit_to_cp = (int*) calloc(max_unit + 1, sizeof(int));
    if (unit_to_cp == NULL) {
        UNPROTECT(1);
        Rf_error("Memory allocation failed");
    }
    
    // Second pass: build the mapping, marking each UTF-16 unit with its code point
    code_point = 0;
    utf16_unit = 0;
    byte_idx = 0;
    
    while (byte_idx < text_len) {
        unsigned char c = (unsigned char)text[byte_idx];
        int units_for_char = 1;
        int start_unit = utf16_unit;
        
        if (c < 0x80) {
            byte_idx += 1;
        } else if ((c & 0xE0) == 0xC0) {
            byte_idx += 2;
        } else if ((c & 0xF0) == 0xE0) {
            byte_idx += 3;
        } else if ((c & 0xF8) == 0xF0) {
            units_for_char = 2;
            byte_idx += 4;
        } else if ((c & 0xC0) == 0x80) {
            byte_idx += 1;
        } else {
            byte_idx += 1;
        }
        
        utf16_unit += units_for_char;
        
        // Mark all UTF-16 units from start_unit to utf16_unit with current code point
        for (int u = start_unit; u < utf16_unit && u <= max_unit; u++) {
            unit_to_cp[u] = code_point;
        }
        
        code_point++;
    }
    
    // Extract results for requested units
    for (int i = 0; i < n_units; i++) {
        int u = units_ptr[i];
        
        if (u < 0) {
            result_ptr[i] = 0;
        } else if (u >= max_unit) {
            result_ptr[i] = max_cp;
        } else {
            result_ptr[i] = unit_to_cp[u];
        }
    }
    
    free(unit_to_cp);
    UNPROTECT(1);
    return result;
}
