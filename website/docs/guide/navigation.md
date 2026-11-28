# Navigate and refactor a project

## Follow a function into another file

Open an empty folder in your editor and save the following two files in it.

**helpers.R**

```r
line_total <- function(price, quantity) {
    price * quantity
}
```

**report.R**

```r
source("helpers.R")
line_total(10, 2)
```

In `report.R`, put the cursor on `line_total` and invoke **Go to Definition**. The editor opens its declaration in `helpers.R`. If you have just opened the project, allow a moment for the server to index the sourced file and try again. From the declaration, invoke **Find References** to find the call in `report.R`. To work on the helper, put the cursor on its `quantity` parameter and invoke **Rename Symbol**. Renaming it to `units` changes both the declaration and the multiplication. The positional call still reads `line_total(10, 2)`.

## Find symbols across a workspace

Choose your editor's **Go to Symbol in Workspace** command and enter `line_total`. With the default `index_mode` of `auto`, the server indexes a package’s `R/` directory in the background. For standalone scripts, it starts with open files and follows literal `source()` and `sys.source()` paths. It can also follow statically known paths built with `file.path()` or `here::here()`. A path such as `source(readline())` becomes known only when R runs the script.

A workspace symbol query can start a broader background scan. An editor that displays progress may show **Indexing R workspace**. A name can appear in that project-wide search even when the current script does not bring its declaration into scope. The project root, include and exclude patterns, and file limits determine which files are discovered. [Indexing settings](../reference/settings)

## Generated package definitions

Put the cursor on `print` in `base::print` and invoke **Go to Definition**. When an installed function has no source-file location, the server generates a document containing its function body. Clients that support LSP text document content open a read-only view. Other clients open a temporary file. To change a package function, edit the package’s own source. Request its definition again to see an updated body.

## Extract an expression

In an R file, add a function that uses a calculated amount.

```r
invoice <- function(price, quantity) {
    subtotal <- price * quantity
    subtotal * 1.1
}
```

Select exactly `price * quantity`, open the code actions or refactoring menu, and choose **Extract expression to variable**. In this example the suggested variable name is `extracted_value`. An editor that supports linked snippet edits lets you change this name to `amount` in both places during the extraction. You can also rename it afterward to get the same result.

```r
invoice <- function(price, quantity) {
    amount <- price * quantity
    subtotal <- amount
    subtotal * 1.1
}
```

Select complete statements to try **Extract selection to function**. **Inline variable** can substitute a local value at its uses when doing so preserves evaluation order and scope. For example, a value computed by a call with side effects can make an inline action unavailable. [More refactoring examples](https://github.com/REditorSupport/languageserver/blob/v0.3.22/inst/examples/lsp-3-18.R)
