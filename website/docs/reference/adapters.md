# Custom styles and adapters

Use the built-in backends for ordinary linting and formatting. If your project needs a particular styler transformation or has its own R linter, configure the server’s R profile as shown below. [Choose a built-in backend](/guide/diagnostics-formatting)

## Change how styler formats a selection

The default style is `styler::tidyverse_style(indent_by = options$tabSize)`. To format spacing and indentation while preserving assignment operators, give the server a function that accepts the editor’s formatting options and returns a styler style.

```r
options(languageserver.formatting_style = function(options) {
    styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
})
```

To keep `=` assignments while applying the other default transformations, use this function instead.

```r
options(languageserver.formatting_style = function(options) {
    style <- styler::tidyverse_style(indent_by = options$tabSize)
    style$token$force_assignment_op <- NULL
    style
})
```

Restart the server to pick up the profile, then choose **Format Selection** or **Format Document**. [Styler customization](https://styler.r-lib.org/articles/customizing_styler.html)

## Supply an R linter

Pass a descriptor to the `languageserver.diagnostics_backend` R option. This example reports a warning on the first tab of any line that begins with a tab.

```r
options(languageserver.diagnostics_backend = list(
    id = "leading-tab",
    document_types = "r",
    run = function(request) {
        lines <- which(startsWith(request$content, "\t"))
        lapply(lines, function(line) {
            list(
                range = list(
                    start = list(line = line - 1L, character = 0L),
                    end = list(line = line - 1L, character = 1L)
                ),
                severity = 2L,
                message = "Indent with spaces."
            )
        })
    }
))
```

After a restart, type a literal tab at the beginning of a line in an R file. The diagnostic source will be `leading-tab`. To return to lintr, remove the option from the profile and restart.

## Adapter contract

| Field | Meaning |
| --- | --- |
| `id` | Unique, stable name. The server sets this as the source of each diagnostic. |
| `document_types` | One or both of `"r"` and `"literate"`. |
| `run` | Function taking a request and returning a list of LSP diagnostics or text edits. It runs in a worker and must be serializable. |
| `modes` | Required for a formatting adapter set through `languageserver.formatting_backend`. Select from `"document"`, `"range"`, `"ranges"`, and `"on_type"`. |
| `needs_workspace_globals` | Optional. Set to `TRUE` for a diagnostics adapter that needs `request$globals`, including package imports and static source dependencies. |
| `config_files` | Optional configuration filenames to watch in ancestor directories. Absolute paths are also accepted. |

Read the current buffer from `request$content`, a character vector with one element per line. Requests also contain the document URI, original and resolved paths, language, workspace root, and working directory. A formatting request additionally includes its mode and editor options. Incoming `range`, `ranges`, and on-type `point` use zero-based R code-point coordinates (`row` and `col`). Return LSP ranges using zero-based `line` and UTF-16 `character` offsets, and include `newText` for each formatting edit. The server handles publication and replies.

Use R options for descriptors containing functions. Workspace JSON configuration can select the built-in backends by name. [Adapter implementation](https://github.com/REditorSupport/languageserver/blob/v0.3.22/R/backends.R) · [Adapter examples in the test suite](https://github.com/REditorSupport/languageserver/blob/v0.3.22/tests/testthat/test-backends.R)
