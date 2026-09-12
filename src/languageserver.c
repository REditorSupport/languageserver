#include "search.h"
#include "reader.h"
#include "semantic.h"
#include "encoding.h"
#include "token.h"
#include "match.h"
#include "completion.h"
#include "index.h"
#include "navigation.h"
#include "signature.h"
#include "call_hierarchy.h"
#include "json.h"

#ifdef _WIN32

# include <fcntl.h>
# include <io.h>
# include <stdio.h>

#else

#include <unistd.h> /* for getppid */

static int ppid = -1;

SEXP process_is_detached(void) {
    if (ppid == -1) {
        ppid = (int) getppid();
    }
    return Rf_ScalarInteger(ppid != (int) getppid());
}
#endif

static const R_CallMethodDef CallEntries[] = {
    {"find_unbalanced_bracket", (DL_FUNC) &find_unbalanced_bracket, 4},
    {"new_bracket_scan_cache_c", (DL_FUNC) &new_bracket_scan_cache_c, 0},
    {"find_unbalanced_bracket_cached_c", (DL_FUNC) &find_unbalanced_bracket_cached_c, 5},
    {"response_json_c", (DL_FUNC) &response_json_c, 2},
    {"enclosed_by_quotes", (DL_FUNC) &enclosed_by_quotes, 2},
    {"detect_comments", (DL_FUNC) &detect_comments, 2},
    {"stdin_read_char", (DL_FUNC) &stdin_read_char, 1},
    {"stdin_read_line", (DL_FUNC) &stdin_read_line},
    {"encode_semantic_tokens_c", (DL_FUNC) &encode_semantic_tokens_c, 5},
    {"code_point_to_unit_c", (DL_FUNC) &code_point_to_unit_c, 2},
    {"code_point_from_unit_c", (DL_FUNC) &code_point_from_unit_c, 2},
    {"scan_token_c", (DL_FUNC) &scan_token_c, 3},
    {"match_with_c", (DL_FUNC) &match_with_c, 2},
    {"fuzzy_find_c", (DL_FUNC) &fuzzy_find_c, 2},
    {"completion_parse_index_c", (DL_FUNC) &completion_parse_index_c, 7},
    {"completion_select_c", (DL_FUNC) &completion_select_c, 4},
    {"source_calls_c", (DL_FUNC) &source_calls_c, 1},
    {"navigation_find_token_c", (DL_FUNC) &navigation_find_token_c, 6},
    {"reference_resolve_local_c", (DL_FUNC) &reference_resolve_local_c, 8},
    {"signature_info_c", (DL_FUNC) &signature_info_c, 1},
    {"active_parameter_c", (DL_FUNC) &active_parameter_c, 2},
    {"semantic_token_delta_c", (DL_FUNC) &semantic_token_delta_c, 2},
    {"semantic_token_range_c", (DL_FUNC) &semantic_token_range_c, 3},
    {"range_line_bounds_c", (DL_FUNC) &range_line_bounds_c, 3},
    {"function_assignment_ids_c", (DL_FUNC) &function_assignment_ids_c, 1},
    {"range_provider_index_c", (DL_FUNC) &range_provider_index_c, 1},
    {"call_hierarchy_containers_c", (DL_FUNC) &call_hierarchy_containers_c, 2},
#if !defined(_WIN32)
    {"process_is_detached", (DL_FUNC) &process_is_detached},
#endif
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
#ifdef _WIN32
    _setmode(_fileno(stdout), _O_BINARY);
#endif
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
