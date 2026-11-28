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

You can also invoke **Go to Definition** inside the quoted `"helpers.R"` path to open the file from its first line. Relative paths resolve against the owning workspace folder. This works for a quoted path to an existing text file, including files other than R scripts.

## Find references and rename a binding

```r
#' @param readings Numeric measurements.
spread <- function(readings) max(readings) - min(readings)
spread(c(2, 5))
```

Place the cursor on `readings` in `function(readings)` and run **Find References**. The two uses inside the body belong to this parameter. Your editor may include the declaration in the results. Now run **Rename Symbol** and enter `values`. The declaration, both body uses, and the attached `@param` name change together. An editor that supports **Prepare Rename** first asks the server which text can be renamed. You can also rename a backticked R identifier, and the server supplies quoting when the new name needs it.

**Development.** Change the call to `spread(readings = c(2, 5))` before starting the rename. Its named argument is included when the server can resolve the call to this function. If another function also takes `readings`, its own argument belongs to a different declaration. A proposed name that would capture an existing binding can cause Rename to decline the edit. For renames inside interpolated strings and data-masked expressions, see the [development examples](./development).

## Inspect incoming and outgoing calls

Enter these three functions in a single R file.

```r
normalize <- function(x) x / 100
score <- function(x) normalize(x)
report <- function(x) score(x) + normalize(x)
```

1. Put the cursor on the declaration of `normalize` and choose **Show Call Hierarchy**. With **Incoming Calls** selected, expand `normalize` to see `score` and `report`. Select a caller to visit the line that calls it.
2. Open the hierarchy on the declaration of `report` and switch to **Outgoing Calls**. Expand it to see `score` and `normalize`. Expanding `score` follows its own call to `normalize`.

The tree follows calls the server can identify in source. **Find References** also covers other uses, such as assigning a function to a variable.

## Read call counts with CodeLens

In the same example, enable **CodeLens** in an editor that displays it. Look above `normalize` for **2 calls** and above `score` for **1 call**. The client can request the count as the lens comes into view. In VS Code, click a nonzero count to peek at the call locations. Distinct call sites count separately, even if they are in one function. The server recognizes calls to a workspace function at bare magrittr pipe stages such as `x %>% normalize`.

**Development.** Call hierarchy and lenses also distinguish source methods on different objects. [Follow a specific member or method](./classes-and-methods#follow-a-specific-member-or-method)

## Find symbols across a workspace

Choose your editor's **Go to Symbol in Workspace** command and enter `line_total`. With the default `index_mode` of `auto`, the server indexes a package’s `R/` directory in the background. For standalone scripts, it starts with open files and follows literal `source()` and `sys.source()` paths. It can also follow statically known paths built with `file.path()` or `here::here()`. A path such as `source(readline())` becomes known only when R runs the script.

A workspace symbol query can start a broader background scan. An editor that displays progress may show **Indexing R workspace**. A name can appear in that project-wide search even when the current script does not bring its declaration into scope. The project root, include and exclude patterns, and file limits determine which files are discovered. [Indexing settings](../reference/settings)

## Work across an R package

Open the directory containing the package’s `DESCRIPTION`, `NAMESPACE`, and `R/` sources. With automatic indexing, a call in `R/report.R` can lead to a function in `R/helpers.R` even when the helper file is closed. The server reads package imports from `NAMESPACE` and packages listed in `Depends` when it determines which names are available. This also gives the linter information about imported names.

**Development.** On a formal parameter of a top-level package function, **Find References** can return its declaration, body uses, and matching named callers while other package files are still being indexed. A quoted operator name or a function assigned to chained aliases can also appear in package symbol searches.

## Generated package definitions

Put the cursor on `print` in `base::print` and invoke **Go to Definition**. When an installed function has no source-file location, the server generates a document containing its function body. Clients that support LSP text document content open a read-only view. Other clients open a temporary file. To change a package function, edit the package’s own source. Request its definition again to see an updated body.

**Development.** An installed method such as `pl$scan_csv()` can also open a generated definition when the server identifies the method but its original source is unavailable. [Classes and methods](./classes-and-methods)

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

Select complete statements to try **Extract selection to function**. The new function takes the inputs it needs from the enclosing scope. To try **Inline variable**, put the cursor on `amount` in the assignment `subtotal <- amount` and open code actions. The action substitutes a single-use local variable’s expression at its use. Review the edit to check when that expression will run. [Code actions and refactoring](./editing-actions)
