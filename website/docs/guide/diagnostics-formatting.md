# Diagnose and format code

Diagnostics mark a problem as you edit. Formatting rewrites the selected code when you ask. To try both with the defaults, install their R packages in the library your editor uses and restart the language server.

```r
install.packages(c("lintr", "styler"))
```

## Read a lint finding

Open a new folder with the default lint rules, save a file called `scores.R`, and enter this line.

```r
score <- 1+2
```

Pause after typing. Hover over the `+` or open your editor's diagnostics list. The `infix_spaces_linter` finding says to put spaces around infix operators. Editors that display links for diagnostics let you open the rule's explanation from the finding. [Rule documentation](https://lintr.r-lib.org/reference/infix_spaces_linter.html)

Select the line and invoke **Format Selection** (range formatting). With the default styler style, it becomes `score <- 1 + 2`, and the finding clears on the next lint pass. Use **Format Document** to apply the same style throughout an R script.

For a new buffer or a file whose path exists only on the client, lintr checks the text sent by the editor. It looks for project rules near the file if the directory is accessible to the server, then falls back to the workspace or server working directory. Filename-specific exclusions require an accessible path. [Configuring lintr](https://lintr.r-lib.org/articles/lintr.html)

## Choose a backend

| Backend | Files | What you can do |
| --- | --- | --- |
| lintr (default, ≥ 3.3.0-1) | R, R Markdown, Quarto | Lint R code in the current buffer. Configure rules in the project's `.lintr` or `.lintr.R`. |
| styler (default, ≥ 1.5.1) | R, R Markdown, Quarto | Format the document, a selection, or on typing when the client supports it. In a literate document, select R code within one chunk. |
| Jarl (≥ 0.7.1) | R | Lint the current R script using project rules in `jarl.toml`. |
| Air (≥ 0.12.0) | R | Format the whole R script using Air's project style. The server can also indent on Enter locally. For selection formatting, choose styler. |

Install the Jarl or Air executable on the machine where the language server runs. You can switch either backend independently. For example, put the following in an `.Rprofile` read by the server and restart it. [Install Jarl](https://jarl.etiennebacher.com/) · [Install Air](https://posit-dev.github.io/air/)

```r
options(
  languageserver.diagnostics_backend = "jarl",
  languageserver.formatting_backend = "air"
)
```

Run `Sys.which(c("jarl", "air"))` in the same R environment to check discovery. If an editor launched from the desktop has a different `PATH`, give the server the path to the executable.

```r
options(languageserver.backend_options = list(
  air = list(executable = "/absolute/path/to/air")
))
```

Jarl and Air receive your unsaved buffer and its filename, so each can find the project's configuration. Air reads `air.toml` or `.air.toml` for style choices. Clients with dynamic formatting registration can update their commands when a workspace setting changes. For a client that registers capabilities only at startup, select the backend in the server's R startup options before launching it.

See [settings](../reference/settings) for editor configuration and [custom adapters and formatting styles](../reference/adapters) to supply a styler style function or your own diagnostics or formatting backend.
