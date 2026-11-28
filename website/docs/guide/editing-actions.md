# Quick fixes and refactoring

Open your editor's code actions menu at a diagnostic, a symbol, or a selection. In VS Code this is the lightbulb beside the line. The choices depend on the current code and the action your editor requests.

## Fix or suppress a diagnostic

With lintr enabled, enter `score <- 1+2` in an R file and wait for the spacing diagnostic. Put the cursor on `+` and open **Quick Fix**. Choose **Add spaces around `+`** to get `score <- 1 + 2`.

If the spelling is intentional, choose **Disable infix_spaces_linter for this line**. The edit adds `# nolint: infix_spaces_linter.` and leaves other rules active. **Disable all linters for this line** inserts `# nolint`. A selection containing findings on multiple lines can offer a grouped suppression.

Direct fixes are available for specific lintr findings. Examples include assignment operators, comma and infix spacing, indentation, pipe consistency, `T` and `F`, comparisons with `NA`, and trailing whitespace or missing final newlines. Suggestions depend on the rules enabled in the project and the text covered by the diagnostic. [Diagnose and format code](./diagnostics-formatting)

## Fix all eligible findings

In a fresh R file, enter these lines using the default lintr rules.

```r
first <- 1+2
second <- 3+4
```

Once the editor shows both diagnostics, run its **Fix All** source action. A client requesting source actions can show **Fix all auto-fixable lintr problems**. Applied to both findings, the result is `first <- 1 + 2` and `second <- 3 + 4`. The server works from the lintr diagnostics supplied by the editor and selects a compatible set of direct edits when fixes overlap. A finding that needs a judgment call stays in the diagnostics list.

## Extract a function

Use **Extract expression to variable** to name part of an expression. To reuse several statements together, select the two assignment lines inside this function, from `shifted` through `* 2`. Choose **Extract selection to function**. [Extract a variable step by step](./navigation#extract-an-expression)

```r
total <- function(values) {
    shifted <- values + 1
    scaled <- shifted * 2
    sum(scaled)
}
```

The new nested function takes `values`. Its result is assigned to `scaled` at the call site, so the subsequent `sum(scaled)` still works. The server can return one local binding used after the selection. When the editor supports snippet workspace edits, type `adjust_values` over the suggested `extracted_function` name to change the declaration and call together. Otherwise use **Rename Symbol** after extraction. The same linked-name behavior is available when extracting a variable.

## Inline a local variable

```r
double_total <- function() {
    total <- 1 + 2
    total * 2
}
```

Place the cursor on the last `total` and choose **Inline local variable `total`**. The edit removes the assignment and replaces the last line with `(1 + 2) * 2`. Parentheses keep the original order of operations. A binding with multiple reads or multiple assignments is ineligible.

**Development builds** also check intervening operations and the context in which a saved value would be evaluated. For example, if `saved <- x` is followed by `x <- 0`, inlining `saved` at a later use could change the result. The development server declines that edit. Extract and Inline can also recognize statements grouped in standalone braces and eligible selections in intact scopes elsewhere in a partially typed file. [Development build installation](./development)

## Open refactoring help

In clients that support documentation links in code actions and opening external documents, choose **Learn about R refactorings** from the Extract or Inline menu. The editor runs the server-provided command and opens the refactoring documentation. [More examples from the released package](https://github.com/REditorSupport/languageserver/blob/v0.3.22/inst/examples/lsp-3-18.R)
