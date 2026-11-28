# Write and understand R

## Discover a package function

In an R file, type `stats::med` and request completion. Choose `median` and hover over it. The server looks up the installed package’s function and help. To inspect the parameters while writing a call, put the cursor after the opening parenthesis and request **Signature Help**.

```r
stats::median(c(1, 5, 8), na.rm = TRUE)
```

Package suggestions depend on the packages installed in the R library the server uses. With a long completion list, typing a few more letters narrows the results.

## Complete members from your script

```r
reading <- list(station = "North", temperature = 18)
reading$temperature
```

On the second line, remove `temperature` and request completion just after `$`. Both `station` and `temperature` are candidates. The server can infer those names from the list declaration before you run the script. The same idea applies to data-frame columns and to declared fields of classes the server recognizes. A name assembled at run time can provide too little information for the server to suggest it.

Some conversions can return different classes depending on the input. A suggestion marked `[possible receiver]` or `[possible member]` is shared by known return types. Hover or Signature Help can also indicate when the receiver is uncertain. In an R6 method, assigning `alias <- self` and then replacing `self$data` can leave completion on `alias$data$` showing the earlier fields.

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

To show compact function signatures in the completion list, enable `completion_label_details` in a client that displays them. For definition lookup and renaming across files, continue with the navigation guide. [Settings](../reference/settings) · [Navigate and refactor a project](./navigation)
