# Read and navigate R code

Use this script to explore the outline, occurrence highlighting, selection, and parameter documentation in your editor. Save it as `scores.R` in an open folder. The examples on this page work with CRAN 0.3.22 unless marked **Development**.

```r
# Scoring ----
#' Scale scores by a fixed amount.
#' @param values Numeric scores.
scale_scores <- function(values) {
    total <- sum(values)
    values / total
}

scale_scores(c(2, 4, 6))
```

## Navigate the outline

Open **Outline** or **Go to Symbol in File** and choose `scale_scores` to jump to its declaration. The section comment `# Scoring ----` also gives the file a named landmark. In editors with a hierarchical outline, sections can contain definitions and classes can expose members. Open **Go to Symbol in Workspace** and search for `scale_scores` to find the definition by name from another file. [Workspace search and indexing](./navigation#find-symbols-across-a-workspace)

**Development.** A named helper defined inside `scale_scores` appears under the enclosing function in a hierarchical outline. If the helper is assigned several times in the same scope, the final assignment determines whether it appears as a function. Class outlines and workspace search also recognize additional declarations such as R6 active bindings, S4 slots, and S7 properties.

## Highlight a symbol

Place the cursor on `total` in the assignment. An editor with occurrence highlighting enabled marks that occurrence and the read in `values / total`. Use **Find References** when you need a list of locations across files. [Project navigation](./navigation)

**Development.** Selecting a source-defined method can highlight uses of that particular declaration, even when another object has a method of the same name. Highlights also work on expressions inside supported glue templates. [Edit R inside a glue string](./development#edit-r-inside-a-glue-string)

## Read semantic highlighting

Enable semantic highlighting in an editor and theme that support it. In `scores.R`, the server identifies `scale_scores` as a function declaration, `values` as a parameter, and `@param` as a documentation tag. Add `if (TRUE) return(values)` inside the function to see a control-flow keyword, a built-in constant, and a parameter in the same expression. The editor theme assigns the colors. Clients can request tokens for the whole file or a visible range, and can request incremental updates after an edit.

**Development.** Known class-member declarations and embedded R in supported glue strings receive semantic tokens. For example, in `glue::glue("Total: {sum(values)}")` a theme can color `sum` as a function while displaying `Total: ` as string text. [Glue highlighting settings](../reference/settings#development-settings)

## Fold code and expand a selection

Use the fold control beside the `scale_scores` declaration to collapse its multiline body. Section headings and contiguous comments can also provide folds. In an R Markdown or Quarto document, folds can cover headings and fenced chunks. [R Markdown and Quarto](./literate)

To select part of an expression, place the cursor inside `values` on the division line and use **Expand Selection**. Repeating the command grows the selection from the name to `values / total` and then to enclosing code. Editors can offer the reverse operation as **Shrink Selection**.

**Development.** A multiline `[[…]]` expression can be folded through its closing brackets. If an editor requests selection ranges for several cursors in a literate document, cursors in R receive their enclosing expressions and cursors in prose retain their positions.

## Follow a file link

Save a text file named `rounds.csv` in the workspace root and add `rounds <- read.csv("rounds.csv")` to `scores.R`. Choose **Go to Definition** inside the quoted filename to open the text file. Alternatively, use your editor’s **Open Link** gesture on the path. A document link can target any existing regular file, and the server checks its size against `link_file_size_limit` when the editor opens the link. [Server settings](../reference/settings#completion-help-and-the-outline)

**Development.** For `source(file.path("R", "helpers.R"))`, the whole path expression can link to an existing `R/helpers.R`. This also works for statically known file arguments to `sys.source()`.

## Edit a color

Enter this line in an R file and enable color decorators in your editor.

```r
palette <- c("steelblue", "#2a3b6a", "#2a3b6a80")
```

The editor can show a swatch for each string. Open the color picker on the last swatch and choose a color. The server offers lowercase and uppercase hexadecimal replacements for the contents of the quotes. An opaque color uses six digits, and a translucent color adds two digits for alpha.

**Development.** The picker also recognizes `"transparent"` and names with spaces or mixed case, such as `"Light Goldenrod Yellow"`. Three- and four-digit hex colors are available on versions of R whose `grDevices::col2rgb()` accepts short hex notation.

## Show parameter hints

Turn on parameter inlay hints in your editor, then enter `stats::rnorm(10, 1, 2)`. Look for `mean =` beside `1` and `sd =` beside `2`. These labels are editor annotations. A client that resolves a hint can show the signature and parameter help when you hover over the label.

By default a call needs at least two supplied arguments and the parameter name needs at least two characters, so `n` receives no hint. Explicitly named arguments and arguments already written as the matching parameter name also omit the label. [Inlay hint settings](../reference/settings#completion-help-and-the-outline)

**Development.** When a member's declaration is known, the same hints work on calls such as `canvas$draw(40, 20)` for a method taking `width` and `height`. Hints can follow the selected S3 method for positional arguments and account for the implicit name supplied by an S7 `:=` declaration.

## Link a parameter to its documentation

Enable linked editing in your editor. In `scores.R`, place the cursor on `values` in the `@param` tag or inside `function(values)`. Editing the name there can update the other linked range as you type. The ranges cover the formal parameter and its explicit documentation. To update uses of the parameter in the body as well, use **Rename Symbol** on the formal. [Try a parameter rename](./quick-start#rename-a-parameter)

**Development.** Linked editing also recognizes documented parameters on supported source methods.

## See values while debugging

Set a breakpoint on `values / total` and run the example with an R debugger. At that line, `total` is `12`. If the editor and debug adapter request inline values, the language server identifies names in the visible source and the debugger looks up their values in the selected stack frame. Enable inline values in the debugger's settings if needed.

**Development.** For an expression such as `record$score`, the server requests the variable `record` from the frame. This also works when there is no separate variable named `score`.
