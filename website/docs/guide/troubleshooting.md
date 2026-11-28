# Troubleshoot a missing result

## I get no completions in an R file

1. Open a saved `.R` file and check that the editor identifies its language as R. Try the local function in the [quick start](./quick-start), using the explicit completion command after typing `cen`.
2. Open the client's language-server log. In VS Code, choose **View → Output** and select **R Language Server**. In Neovim 0.11+, run `:checkhealth vim.lsp` and look for the R client attached to the current buffer. [Editor setup](./editors)
3. Start the same R executable that the editor launches, from the same project folder. If the editor runs in a container, do this inside the container. Inspect the R installation, its library paths, and where the package is installed.

```r
R.home()
.libPaths()
find.package("languageserver")
```

If the package lookup fails, run `install.packages("languageserver")` in this R installation. A project that uses renv may give the editor a different library from a standalone R session. Restart the language server after installing and retry the local completion.

## Go to Definition misses another file

Open the folder that contains the project and try this pair of files. Save `helpers.R` with `twice <- function(x) x * 2`. In a sibling `main.R`, enter the following, then invoke **Go to Definition** on `twice`.

```r
source("helpers.R")
twice(3)
```

The literal path gives the server a dependency it can follow without running the script. Immediately after opening the file, allow a moment for indexing and try the jump again. A filename chosen at runtime can leave the target unknown. If this example works, check how your original file finds the helper, the selected workspace root, and your index settings. Package projects are recognized by a `DESCRIPTION` with a `Package` field and an `R/` directory. A development build also supports an explicit mapping for helpers loaded by a test runner. [Project navigation](./navigation) · [Development features](./development)

## Formatting is missing or lint findings never appear

Open a saved `.R` file and try the `score <- 1+2` example with **Format Document**. If multiple formatters are installed, select the one provided by the R language server. Check the backend selected in your editor settings or `.Rprofile`. For lintr or styler check `packageVersion("lintr")` or `packageVersion("styler")` in the R library used by the server. For Jarl or Air check the executable path with `Sys.which(c("jarl", "air"))` and its version with `jarl --version` or `air --version` in a terminal.

If formatting works on an entire R file but no command appears for a selection, check the backend's supported modes. If lint findings are missing, check the project's lint rules, exclusions, and `r.lsp.diagnostics`. An explicit R option in the server process takes precedence over the matching editor setting. [Backend support](./diagnostics-formatting) · [Settings](../reference/settings)

## A development example is missing

Run this in the R environment your editor uses.

```r
packageDescription("languageserver")[c("Version", "Repository", "RemoteSha")]
```

The development build can share a version number with CRAN. Use the repository and commit fields, when recorded, to identify the build. After installing an update, restart the server so it loads the new package. [Development build installation](./development)

## Capture a server log

Set these options in an `.Rprofile` that the language-server process reads. Restart the server, repeat the action once, then inspect `r-languageserver.log` in your home directory.

```r
options(
  languageserver.debug = TRUE,
  languageserver.log_file = path.expand("~/r-languageserver.log")
)
```

When reporting a problem, include the small R example, the cursor location, the editor command, your package and editor versions, and the relevant lines from the log. Remove the debug options and restart when finished. [Issue tracker](https://github.com/REditorSupport/languageserver/issues) · [Server startup](../reference/run)
