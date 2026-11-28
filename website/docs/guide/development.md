# Try development features

These examples cover changes merged into `master` since CRAN 0.3.22. To try them, install the development build from r-universe into the R library used by your editor, then restart its language server.

```r
install.packages("languageserver", repos = c(
  reditorsupport = "https://reditorsupport.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
```

To return to the CRAN release, run `install.packages("languageserver", repos = "https://cloud.r-project.org")` and restart the server. [Development builds](https://reditorsupport.r-universe.dev/languageserver) · [Release notes](https://github.com/REditorSupport/languageserver/blob/master/NEWS.md)

For a minimal development installation, add `roxygen2` if you want structured help from local `#'` comments, and `stringi` for ICU-based Unicode matching and detection of legacy-encoded files. The server reads UTF-8 and byte-order-marked UTF-16 and UTF-32 source files with base R. CRAN 0.3.22 installs both packages as dependencies.

## Edit R inside a glue string

```r
amount <- 42
glue::glue("Total: {price}", price = amount)
```

Put the cursor on `price` inside the braces and choose **Go to Definition**. The target is the named argument `price = amount`. Rename `price` to `total` there and both occurrences change together. The variable supplying its value keeps the name `amount`.

Inside the braces, you can also request completion, hover help, or Signature Help. Try typing `{` in a new glue string and requesting completion for `amo`. In VS Code, **Trigger Suggest** works on demand. To reopen suggestions automatically as you type inside an existing R string, add this to `settings.json`. Use the `[rmd]` block for R Markdown.

```json
{
  "[r]": {
    "editor.quickSuggestions": { "strings": "on" }
  }
}
```

The server recognizes directly quoted templates with the standard delimiters in `glue()`, `glue_data()`, `str_glue()`, and `str_glue_data()`, including raw strings. Doubled braces represent literal text. If you wrap glue, you can also ask the editor to color R expressions in your wrapper's templates. This `.Rprofile` example identifies the first unnamed argument of `my_glue()` and the `template` argument of `reports::render()`.

```r
options(languageserver.glue_highlight_functions = list(
  my_glue = 1L,
  "reports::render" = "template"
))
```

These registrations apply only to semantic highlighting. [Settings](../reference/settings)

With `glue_data()`, the data argument can supply a template name. In this example, use **Go to Definition** on `value` inside the braces to find the named list entry. **Find References** also finds `record$value`. An explicit named glue argument can take precedence over the field. Custom delimiters, transformers, or an unknown evaluation environment can limit which bindings the server can identify.

```r
record <- list(value = 4)
record$value
glue::glue_data(record, "{value}")
```

## Find a helper loaded by your test runner

A test might call a helper without a visible `source()` line. In a project containing `tests/testthat/`, save these two files.

```r
# tests/testthat/helper-scores.R
sample_scores <- function() c(3, 7, 8)
```

```r
# tests/testthat/test-scores.R
mean(sample_scores())
```

In the project's `.Rprofile`, map the calling files to the helper.

```r
options(languageserver.project_helpers = list(
  "tests/testthat/test-*.R" = "tests/testthat/helper-scores.R"
))
```

Open the project root, restart the server, and leave the helper file closed. Invoke **Go to Definition** on `sample_scores` in the test. The server opens the helper's definition. The caller can be a glob, and helper filenames are literal paths relative to the workspace root. This mapping applies to Go to Definition and works with index modes `auto`, `lazy`, and `full`. [Project navigation](./navigation)

## Rename a variable used beside a data-frame column

```r
score <- 5
scores <- data.frame(score = c(3, 7, 8))
dplyr::filter(scores, .data$score > .env$score)
```

Place the cursor on `score` in the first line and rename it to `threshold`. The last comparison becomes `.data$score > .env$threshold`. The `.env` pronoun explicitly identifies the R variable, and `.data` identifies the column. A bare name in a data-masked call can be ambiguous. When the server cannot determine which binding a rename would affect, it declines the edit.

## Go to Definition for S4 methods

```r
methods::setGeneric("describeSample", function(x) standardGeneric("describeSample"))
methods::setMethod("describeSample", "numeric", function(x) mean(x))
methods::setMethod("describeSample", "character", function(x) unique(x))

summarize <- function(value) describeSample(value)
```

Invoke **Go to Definition** on `describeSample` in the last line. The editor receives three source locations, the generic followed by its two method registrations. Pick a method to inspect its body. This works even when the class of `value` is unknown. You can also navigate from the generic name in `selectMethod("describeSample", "numeric")`. Visible declarations can come from the current package's R files or the script's static source dependencies.

For an operator such as `+`, the results can include methods registered for the operator and its `Arith` and `Ops` groups. Replacement access, such as `value(x)[i] <- y`, can lead from `value` to a registered `value<-` method.

Use **Find References** to see the generic’s source registrations and calls that resolve to it. A local function of the same name can have its own callers. Rename declines edits to a registered S4 generic.

## Open an S3 implementation or a class declaration

In the [S3 signature example](./writing-r#find-the-arguments-for-an-s3-method), put the cursor on `present` in `present(frame, format = "text")`. **Go to Definition** opens `present`. **Go to Implementation** opens `present.data.frame`, the method selected for `frame`. Try the same action on `present(values, scientific = TRUE)` to open `present.numeric`. The server needs enough information about the input class to identify the method.

To find the class an object comes from, try this R6 example.

```r
Counter <- R6::R6Class("Counter")
counter <- Counter$new()
counter
```

Invoke **Go to Type Definition** on the final `counter`. The editor opens the `Counter` class declaration. This action can also locate source definitions for reference classes, S4, and S7 when the receiver has one identifiable declaration. [Client capabilities](../reference/capabilities)

## Keep editing an incomplete script

Try **Go to Definition** on `double_score` in the last line while the middle line still ends in `::`. It can find the declaration on the first line. The outline and semantic highlighting can also use the complete statements around a recoverable syntax error.

```r
double_score <- function(points) points * 2
stats::
report_score <- function(x) double_score(x)
```

Recovery depends on the enclosing R syntax. An unfinished string or unmatched delimiter can obscure a scope. Rename and refactoring are available where the server can identify which code an edit would affect. In an R Markdown or Quarto file, R chunks recover independently. [Mixed-language documents](./literate)

## More changes to released features

| Where you work | What changes in development |
| --- | --- |
| [Writing calls](./writing-r#completion-in-development-builds) | Complete paths in quoted `source()` calls and follow functions assigned to list or environment members. Extracted or quoted member functions can supply argument help. |
| [Reading code](./reading-code) | Find nested functions and class members in the outline. Fold multiline `[[`, follow a compound source path, and see parameter-name hints for resolved member calls. Documented method parameters support linked editing, and a debugger can request the receiver of a field access. |
| [Classes and methods](./classes-and-methods) | Follow R6 clones and reference-class methods. S7 1.0 class and generic declarations using `:=` support property navigation, method-specific arguments, and hints. Installed member functions can open a generated body when their source file is unavailable. |
| [References and navigation](./navigation) | Matching a source member uses its declaration, including for call counts and named-argument references. Package-source search also recognizes chained assignment aliases and quoted operators. Find References can locate a package parameter’s declaration, body uses, and named callers before the whole package finishes indexing. |
| [Data-frame columns](./data-analysis#find-the-source-of-a-column) | Navigate to a named constructor column in supported data.table, dplyr, and base R expressions. References and Rename distinguish it from a same-named enclosing variable. |
| [Refactoring](./editing-actions) | Standalone brace blocks preserve the surrounding bindings for extraction and inlining. Proposed edits account for saved reads and list updates. |

Large-file parsing, bulk lint fixes, and edits in literate documents can finish sooner. S3 method information becomes available sooner after startup on R versions before 4.6. Closing external Quarto documents releases their temporary state, and shutting down the server stops background work while the editor finishes the protocol handshake. On a slow machine, `R_LANGSVR_TIMEOUT_SCALE` can give metadata workers more time. [Server settings](../reference/settings#development-settings)
