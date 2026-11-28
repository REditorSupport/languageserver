# Write and understand R

## Discover a package function

In an R file, type `stats::med` and request completion. Choose `median` and hover over it. The server looks up the installed package’s function and help. To inspect the parameters while writing a call, put the cursor after the opening parenthesis and request **Signature Help**.

```r
stats::median(c(1, 5, 8), na.rm = TRUE)
```

Package suggestions depend on the packages installed in the R library the server uses. With a long completion list, typing a few more letters narrows the results.

## Inspect a completion

While typing a bare name, the completion list can include local variables, function parameters, names in the project, attached or imported package functions, and constants such as `TRUE`. Use `pkg::` for exports from a particular package. Try this example before running it.

```r
#' Summarize a set of measurements.
#' @param values Numeric observations from one station.
#' @param method Summary statistic to compute.
summarize_values <- function(values, method = c("mean", "median")) {
    switch(match.arg(method), mean = mean(values), median = stats::median(values))
}

readings <- c(2, 4, 9)
summarize_values(readings, method = "median")
```

On the last line, request completion after `summarize_v` and **highlight** `summarize_values` in the list. An editor that requests completion details can now show the documentation from the `#'` comment before you insert the name. A client with snippet support can put the cursor inside the parentheses when you accept a function. For compact signatures or value previews alongside suggestions, enable `completion_label_details`. [Completion settings](../reference/settings#completion-help-and-the-outline)

After completing the line, hover over `summarize_values` to read its signature and description. Hover over the named argument `method` to see its `@param` text, or over `readings` to see the assignment in scope. These are three ways to inspect the same call without opening another file.

## Match arguments as you type

In the call above, replace `method = "median"` with `met` and request completion. Choose the parameter named `method`. After `method = `, type `med` without quotes and request completion again. Choosing the value `median` inserts `"median"`. The server can read choices from literal character defaults such as `c("mean", "median")`. For `options(`, it can also suggest R option names, such as `scipen`.

Request **Signature Help** just after the comma or on the value of a named argument. The editor highlights the corresponding parameter, including named arguments supplied out of declaration order. Try `readings |> summarize_values()` in R 4.1 or newer. With the cursor just inside the parentheses, the pipe already supplies `values`, so the active parameter is `method`. Signature Help also accounts for supported magrittr pipes and explicit placeholders.

## Complete members from your script

```r
reading <- list(station = "North", temperature = 18)
reading$temperature
```

On the second line, remove `temperature` and request completion just after `$`. Both `station` and `temperature` are candidates. The server can infer those names from the list declaration before you run the script. The same idea applies to data-frame columns and to declared fields of classes the server recognizes. A name assembled at run time can provide too little information for the server to suggest it.

Some conversions can return different classes depending on the input. A suggestion marked `[possible receiver]` or `[possible member]` is shared by known return types. Hover or Signature Help can also indicate when the receiver is uncertain. In an R6 method, assigning `alias <- self` and then replacing `self$data` can leave completion on `alias$data$` showing the earlier fields.

For a known S4 object, use `@` to see declared slots. Class-specific completion can also follow members through R6 method chains and supported Polars expressions when package metadata identifies the receiver. [Classes and methods](./classes-and-methods) · [Data analysis](./data-analysis)

## Find the arguments for an S3 method

An S3 generic chooses a method from the class of its input. Try the following example in an R file.

```r
present <- function(x, ...) UseMethod("present")
present.data.frame <- function(x, columns = names(x), format = "text", ...) {
    list(columns = columns, format = format)
}
present.numeric <- function(x, digits = 2, scientific = FALSE, ...) {
    format(x, digits = digits, scientific = scientific)
}

frame <- data.frame(value = 1:3)
values <- c(1.2, 2.3)
present(frame, format = "text")
present(values, scientific = TRUE)
```

Put the cursor after the comma in each call and request **Signature Help**. With `frame`, look for `columns` and `format` from the data-frame method. With `values`, look for `digits` and `scientific` from the numeric method. You can also request argument completion at that position while entering each call.

Add `describe <- function(input) present(input)` and request Signature Help inside `present(input)`. Here the caller supplies the class of `input`, so the editor shows the generic’s `present(x, ...)` signature. The package includes a longer example covering aliases, pipes, and installed generics. [S3 dispatch example](https://github.com/REditorSupport/languageserver/blob/v0.3.22/inst/examples/s3-dispatch.R)

## Completion in development builds

- **Source paths.** Save `helpers.R` beside your current script. Type `source("hel` and request completion inside the quotes. Choose `helpers.R` to insert the file name. The server also offers directories for nested paths and recognizes the file argument of `sys.source()`.
- **Incomplete code.** While you finish a call, the server can keep using complete expressions elsewhere in the buffer. In an R Markdown or Quarto document it recovers each R cell independently.
- **Methods added after construction.** After `tools <- list()` and `tools$score <- function(values) sum(values)`, try `tools$sc` and inspect the signature inside `tools$score(`. The development server follows the new declaration, including a member written with a literal `[[` name. It also recognizes more extracted or quoted method calls and S7 1.0 declarations.
- **Glue strings.** Inside the braces of a supported glue template, request completion, Hover, or Signature Help on the embedded R expression. [Development build and glue example](./development#edit-r-inside-a-glue-string)
