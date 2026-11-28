# Get started in five minutes

Use a two-line R script to complete a function call, read its signature, jump to its definition, and rename a parameter. You can try all four actions before running the script.

## Install and connect

In the R installation your editor will use, install the CRAN release.

```r
install.packages("languageserver")
```

Open a folder in your editor and create a file named `center.R`. Enable the R language client for that editor. The setup guide gives the steps for VS Code and Neovim, with links for other clients. [Set up your editor](./editors)

## Complete a call

Enter the first line below. On the second line, type just `cen` and request completion. Choose `center` from the suggestions.

```r
center <- function(values) values - mean(values)
center(c(2, 4, 6))
```

In VS Code use **Trigger Suggest**. In Neovim press `Ctrl-X` then `Ctrl-O` in Insert mode. Some editors insert the parentheses when you accept a function suggestion.

## See which argument to supply

Place the cursor immediately after the opening parenthesis of `center(` on the second line. Request **Signature Help**. The popup shows `center(values)`. Finish the call as shown above.

## Go to the definition

Place the cursor on the second-line `center` and invoke **Go to Definition**. The editor takes you to the function declaration on line one. This also gives you a quick way to check that your editor has connected to the server.

## Rename a parameter

Place the cursor on `values` inside `function(values)`, invoke **Rename Symbol**, and enter `x`. The parameter and both uses inside the function change together.

```r
center <- function(x) x - mean(x)
center(c(2, 4, 6))
```

Try the same editing tools on package functions, list members, or a call into another file. [Write and understand R](./writing-r) · [Navigate and refactor a project](./navigation)
