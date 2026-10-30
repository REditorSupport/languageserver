# languageserver: An implementation of the Language Server Protocol for R

[![R-CMD-check](https://github.com/REditorSupport/languageserver/actions/workflows/rcmdcheck.yml/badge.svg)](https://github.com/REditorSupport/languageserver/actions/workflows/rcmdcheck.yml)
[![codecov](https://codecov.io/gh/REditorSupport/languageserver/graph/badge.svg)](https://app.codecov.io/gh/REditorSupport/languageserver)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/languageserver)](https://cran.r-project.org/package=languageserver)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/languageserver)](https://cran.r-project.org/package=languageserver)
[![r-universe](https://reditorsupport.r-universe.dev/badges/languageserver)](https://reditorsupport.r-universe.dev/#package:languageserver)

`languageserver` is an implementation of the Microsoft's [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) for the language of R.

- [Installation](#installation)
- [Language Clients](#language-clients)
- [Services Implemented](#services-implemented)
- [Settings](#settings)
- [FAQ](#faq)
  - [Linters](#linters)
  - [Selecting diagnostics and formatting backends](#selecting-diagnostics-and-formatting-backends)
  - [Custom R adapters](#custom-r-adapters)
  - [Customizing server capabilities](#customizing-server-capabilities)
  - [Customizing formatting style](#customizing-formatting-style)

## Installation

A few dependencies are required beforehand:

```bash
# On Debian, Ubuntu, etc.
apt install --assume-yes --no-install-recommends build-essential libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev r-base

# On Fedora, Centos, etc.
dnf install --assumeyes --setopt=install_weak_deps=False @development-tools libcurl-devel libxml2-devel openssl-devel libuv-devel R

# On Alpine
apk add --no-cache curl-dev g++ gcc libxml2-dev linux-headers make R R-dev
```

`languageserver` is released on CRAN and can be easily installed by

```r
install.packages("languageserver")
```

The default diagnostics and formatting backends require optional R packages:

```r
install.packages(c("lintr", "styler"))
```

For a minimal source installation, install mandatory dependencies only and select
the CLI backends below, or disable diagnostics. Completion, navigation and other
language features work without lintr or styler. Selecting an unavailable backend
reports an actionable error; the server does not choose a different backend.

To try the latest features, install the daily development build from our [r-universe](https://reditorsupport.r-universe.dev) repository:

```r
install.packages("languageserver", repos = c(
    reditorsupport = "https://reditorsupport.r-universe.dev",
    getOption("repos")
))
```

Or install the latest development version from our GitHub repository:

```r
# install.packages("remotes")
remotes::install_github("REditorSupport/languageserver")
```

## Language Clients

Workspace discovery and indexing run in the background. Clients that advertise
LSP work-done progress support can show an **Indexing R workspace** indicator
while R files are discovered, indexed, and parsed. Reports include file counts
and a percentage once discovery completes. The client chooses whether to show
this in its status bar, progress panel, or another location.

The following editors are supported by installing the corresponding extensions:

- VS Code: [vscode-R](https://github.com/REditorSupport/vscode-R)

- Atom: [atom-ide-r](https://github.com/REditorSupport/atom-ide-r)

- Sublime Text: [R-IDE](https://github.com/REditorSupport/sublime-ide-r)

- NeoVim: NeoVim's LSP client with settings

    ```lua
    vim.lsp.config['r_language_server'] = {
    	settings = {
    		filetypes = { "r", "rmd" },
    	},
    }
    vim.api.nvim_create_autocmd("FileType", {
    	pattern = { "r", "rmd" },
    	callback = function()
    		vim.lsp.start(vim.lsp.config["r_language_server"])
    	end,
    }
    ```

  or, if you use [coc.nvim](https://github.com/neoclide/coc.nvim), you can do one of two things:
  
  - Install [coc-r-lsp](https://github.com/neoclide/coc-r-lsp) with:

    ```vim
    :CocInstall coc-r-lsp
    ```

  - or install the languageserver package in R

    ```r
    install.packages("languageserver")
    # or install the developement version
    # remotes::install_github("REditorSupport/languageserver")
    ```

    Then add the following to your Coc config:

    ```json
    "languageserver": {
        "R": {
            "command": "/usr/bin/R",
            "args" : [ "--no-echo", "-e", "languageserver::run()"],
            "filetypes" : ["r"]
        }
    }
    ```

- Emacs: [eglot-mode](https://elpa.gnu.org/devel/doc/eglot.html) (native LSP client)

    ```elisp
    (use-package ess :ensure t)
    (add-hook 'ess-r-mode-hook 'eglot-ensure)
    ```
    To check if it is working, open an R file, place the cursor on a line and run `M-x ess-eval-line`.

- Emacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode)

- JupyterLab: [jupyterlab-lsp](https://github.com/jupyter-lsp/jupyterlab-lsp)

- [BBEdit](https://www.barebones.com/products/bbedit/): preconfigured in version 14.0 and later; see the [BBEdit LSP support page](https://www.barebones.com/support/bbedit/lsp-notes.html) for complete details.

- [Nova](https://nova.app): [R-Nova](https://github.com/jonclayden/R-Nova)

## Services Implemented

`languageserver` is still under active development, the following services have been implemented:

- [x] [textDocumentSync](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_synchronization)
- [x] [publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_publishDiagnostics)
- [x] [hoverProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_hover)
- [x] [completionProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion)
- [x] [completionItemResolve](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#completionItem_resolve)
- [x] [signatureHelpProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_signatureHelp)
- [x] [definitionProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition)
- [x] [referencesProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_references)
- [x] [documentHighlightProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentHighlight)
- [x] [documentSymbolProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol)
- [x] [workspaceSymbolProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_symbol)
- [x] [codeActionProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_codeAction)
- [x] [codeLensProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_codeLens)
- [x] [documentFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_formatting)
- [x] [documentRangeFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rangeFormatting)
- [x] [documentOnTypeFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_onTypeFormatting)
- [x] [renameProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename)
- [x] [prepareRenameProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename)
- [x] [documentLinkProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentLink)
- [x] [documentLinkResolve](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#documentLink_resolve)
- [x] [colorProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor)
- [x] [colorPresentation](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation)
- [x] [foldingRangeProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_foldingRange)
- [x] [selectionRangeProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_selectionRange)
- [x] [prepareCallHierarchy](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareCallHierarchy)
- [x] [callHierarchyIncomingCalls](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#callHierarchy_incomingCalls)
- [x] [callHierarchyOutgoingCalls](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#callHierarchy_outgoingCalls)
- [x] [prepareTypeHierarchy](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareTypeHierarchy)
- [x] [typeHierarchySupertypes](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#typeHierarchy_supertypes)
- [x] [typeHierarchySubtypes](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#typeHierarchy_subtypes)
- [x] [semanticTokens](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens)
- [x] [linkedEditingRange](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_linkedEditingRange)
- [x] [executeCommandProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_executeCommand) (refactoring documentation)
- [x] [inlineValueProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_inlineValue)
- [x] [inlayHintProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_inlayHint)

Inline values are a debugger-facing feature rather than normal editor
annotations. When execution is paused, a compatible editor and debug adapter
request variable lookup ranges from the language server, evaluate them in the
selected stack frame, and render the resulting values beside the source. They
have no visible effect when the client or R debug adapter does not request
`textDocument/inlineValue`.

R6 member inference has a known limitation after field assignments inside a
method. The written reference keeps its class context, but field information
is not synchronized across aliases or the saved receivers for `private` and
`super` methods. For example, after `alias <- self; self$data <- list(new = 1)`,
completion on `alias$data$` may still use the earlier `data` value. The same
can occur when a private or superclass method returns `self$data`.

## S3 method information

Argument completion, hover and signature help select an ordinary S3 method when
the input object's dispatch classes can be inferred from source or package
metadata. This includes literal classes, aliases, simple source factories,
package datasets and guarded constructor summaries such as `data.frame()` and
`lm()`. Namespace-qualified and piped calls use the same resolver.

For example, `summary(iris, ...)` uses `summary.data.frame`, while
`predict(lm(mpg ~ wt, data = mtcars), ...)` uses `predict.lm`. Method-specific
argument names, literal argument choices and parameter documentation are shown
without executing document code, constructors, methods or defaults. See
[the demo script](inst/examples/s3-dispatch.R).

With datawizard installed, `datawizard::rescale(iris, ...)` uses its registered
`rescale.data.frame` method and offers arguments such as `select`, `exclude`
and `append` before the document is executed.

Unknown or ambiguous classes retain the generic's information. Dynamic method
registration, opaque reflection and unsupported constructor transformations
also retain the generic fallback. Group/internal generics and additional
parameters forwarded through sophisticated `NextMethod()` chains are outside
this inference path. A method's extra arguments are offered only when the
generic can accept them.

## Refactoring

Select an expression and choose **Extract expression to variable**, or select
complete statements and choose **Extract selection to function**. Extraction
keeps the selected code in its original scope and identifies the inputs needed
by an extracted function. A local variable can also be replaced with its value
using **Inline variable** when doing so preserves evaluation and binding behavior.

These actions use static analysis and are offered only when the transformation
can be determined safely. Unsupported selections, ambiguous bindings, and code
whose evaluation would change may have no available action. Review the proposed
edits before applying them.

In editors supporting snippet workspace edits, eligible extractions let you edit
a name linked between the new declaration and its use. Edits overlapping multiline
strings or quoted names use ordinary text edits to preserve their contents.
Ordinary edits use a unique generated name, which can be changed with Rename.
Compatible editors also offer a documentation link in the refactoring menu.

Try [the LSP 3.18 demo script](inst/examples/lsp-3-18.R) for signature help,
linked extraction names, multiline string preservation, generated package
definitions and refactoring documentation.

## Generated package definitions

When a package function has no source-file location, Go to Definition shows its
generated body. Editors supporting LSP text document content open it as a
read-only document owned by that R language server. Requesting the definition
again refreshes the body if it has changed. Generated bodies share a bounded
cache; an evicted document can be restored by requesting its definition again.
Other editors receive the existing generated temporary file, whose edits have
no effect on the package function.

## Settings

`languageserver` exposes the following settings via LSP configuration.

settings | default | description
----     | -----   | -----
`r.lsp.debug`  | `false` | increase verbosity for debug purpose
`r.lsp.log_file` | `null` | file to log debug messages, fallback to stderr if empty
`r.lsp.diagnostics` | `true` | enable diagnostics from the selected backend
`r.lsp.diagnostics_backend` | `"lintr"` | diagnostics backend: `lintr` or `jarl`
`r.lsp.formatting_backend` | `"styler"` | formatting backend: `styler` or `air`
`r.lsp.backend_options` | `{}` | per-CLI-backend executable, timeout in seconds (default 10), and environment overrides
`r.lsp.inlay_hints_minimum_arguments` | `2` | minimum supplied arguments before parameter-name inlay hints are shown
`r.lsp.inlay_hints_minimum_argument_length` | `2` | minimum argument-name length for an inlay hint, excluding an initial `.`
`r.lsp.rich_documentation` | `true` | rich documentation with enhanced markdown features
`r.lsp.snippet_support` | `true` | enable snippets in auto completion
`r.lsp.max_completions` | 200 | maximum number of completion items
`r.lsp.s3_dispatch` | `true` | use static S3 dispatch for argument completion, hover and signature help, independently of member completion
`r.lsp.member_completion` | `true` | infer `$` members and S4 `@` slots for completion, signature help and hover from document syntax and cached package metadata without executing document code
`r.lsp.lint_cache` | `false` | toggle caching of lint results
`r.lsp.parse_delay` | `0.15` | seconds to debounce parsing after an edit
`r.lsp.diagnostics_delay` | `0.75` | seconds to debounce diagnostics after the current parse
`r.lsp.parse_cache_max_mb` | `64` | maximum memory used by cached document parse versions
`r.lsp.diagnostics_cache_max_mb` | `16` | maximum memory used by cached diagnostics
`r.lsp.index_mode` | `"auto"` | eagerly index package workspaces; lazily index open files and static source dependencies otherwise; `"full"` forces workspace discovery, `"lazy"` defers it, and `"off"` restores legacy package-only loading
`r.lsp.index_include` | `"**/*.R"` | glob or character vector of globs included in workspace indexing
`r.lsp.index_exclude` | common VCS, dependency, cache, and output directories | glob or character vector of globs excluded from workspace indexing
`r.lsp.index_max_files` | `10000` | maximum number of eligible R files discovered per workspace
`r.lsp.index_max_file_size_mb` | `2` | maximum size of a file included in the workspace index
`r.lsp.index_batch_size` | `20` | maximum number of shallow summaries built in one idle batch
`r.lsp.index_time_budget_ms` | `25` | approximate event-loop budget for each shallow-index batch
`r.lsp.index_persistent_cache` | `true` | persist validated summaries for full indexes; lazy indexes stay in memory
`r.lsp.server_capabilities` | `{}` | override server capabilities defined in [capabilities.R](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R). See FAQ below.
`r.lsp.link_file_size_limit` | 16384 | maximum file size (in bytes) that supports document links

These settings could also specified in `.Rprofile` file via `options(languageserver.<SETTING_NAME> =  <VALUE>)`. For example,

```r
options(languageserver.snippet_support = FALSE)
```

will turn off snippet support globally. LSP configuration settings are always overriden by `options()`.

Package member models, startup datasets, S3 catalogs, documentation summaries and
rendered help share a persistent cache under `tools::R_user_dir("languageserver", "cache")`.
It keeps up to 512 entries and 256 MiB, removing older entries as needed. Package
file, loaded namespace, dependency, registry, R version and server implementation changes invalidate
cached results; incompatible or corrupt entries are rebuilt. Document-dependent
inference remains local to the current document and its metadata revision.
File contents are checksummed on first use and after changes to file identity,
size, modification time or change time; warm lookups reuse the checksum. Help
uses the actual database location, and checks its identity before using memory
or disk results. Immutable help database copies live under
`file.path(tempdir(), "languageserver-help")` for the session, avoiding stale
bytes in R's own lazy-load cache without relying on internal R APIs. Rendered
help and documentation memory caches are bounded. Server keys cover the cache
schema, loaded R/native implementation, functions and inert constants, so
incompatible server versions or development builds do not share entries.
Conversions with unresolved input classes can offer the common members of known
method results as suggestions, marked `[possible receiver]` or `[possible member]`.
These suggestions do not establish the receiver's type; known conflicting method
outcomes and local method overrides suppress them. Signature help and hover also
use unambiguous common members and display a note when the receiver is uncertain.
Possible results propagate through member calls, assignments and aliases so
completion and provider information remain available on derived objects.
Set `options(languageserver.package_cache_dir = "/path/to/cache")` or the
`R_LANGSVR_PACKAGE_CACHE_DIR` environment variable to choose another cache root;
set `options(languageserver.package_cache_dir = FALSE)` to disable disk caching.
These options are also passed to metadata workers. Cold member analysis starts
with a 250 ms allowance and extends the same traversal as needed, up to two
seconds of CPU time and five seconds elapsed. This allows for CPU contention
without restarting analysis. Node and depth bounds remain in place. Completed
receivers, including uncertain results, are reused across completion, signature
help and hover. Completed binding summaries also survive edits to member names,
call arguments and following code, and can serve requests at different cursor
positions. Source-prefix fingerprints are prepared by the parser; preceding
code, attachment-context or package-metadata changes invalidate those summaries.
They remain in a bounded in-memory cache for the current document. Source
closures and mutable object identities are excluded from reuse across edits.

Shared analysis caches also reuse completed S3 class summaries across requests
and edits, checking package metadata, attachment/registration context and exact
peer-source contents. Budget-exhausted and mutable scratch-state results are
excluded. Local signature and documentation facts use their exact source text
as keys and survive reparses; persistent parse workers reuse unchanged comment
blocks. Package NAMESPACE declarations are read for current content and reuse
their parsed facts when that content is unchanged. These bounded caches are
allocated on demand and store serialized results for independent requests.

Automatic indexing recognizes a package by its `DESCRIPTION` with a `Package`
field and its `R/` directory. Package indexing runs in the background and
prioritizes package sources. A workspace containing standalone scripts starts
without recursively discovering files or loading a saved whole-workspace index.
Opening a script indexes its statically resolvable `source()` and `sys.source()`
dependencies recursively. Opening a nested package source activates that
package's `R/` namespace without scanning neighboring projects. Closing standalone scripts
or removing source relationships releases files that are no longer relevant.

Workspace symbol searches, global references, and global renames request a full
background scan when needed and wait for the required source files. Local
references remain local. Cancelling a query cancels its reply; the shared index
may continue building for subsequent queries. Completion, hover, and signature
help do not wait for unrelated workspace files. A requested full index is retained
until the language server is restarted or indexing settings change. Dynamic source expressions
remain unresolved, and full discovery does not merge unrelated scripts into a
single execution scope. Include/exclude patterns and file limits still apply.
The file limit is shared by automatic discovery across activated packages.
Files loaded for open buffers or required static source dependencies remain
available even when they exceed that limit and do not consume additional discovery
slots. Automatically discovered files retain their slots until they are removed.

Project indexing is deliberately two-tiered. Package `R/` files, open files,
and the transitive dependencies of static `source()` or `sys.source()` calls
receive full semantic parsing. Other discovered scripts receive only a lightweight symbol
and source-call summary, so they appear in workspace symbol search without
polluting completion, definition, references, or rename in unrelated scripts.
Static paths built from string literals, `file.path()`, and `here::here()` are
recognized; project code is never executed to resolve a path.

## FAQ

### Linters

The default backend requires [lintr](https://github.com/r-lib/lintr) >= 3.3.0-1.
Specify rules in `.lintr` or `.lintr.R`, including the `.github/linters/`
project locations supported by lintr; see the lintr
[documentation](https://lintr.r-lib.org/articles/lintr.html).

### Selecting diagnostics and formatting backends

Install [Jarl](https://jarl.etiennebacher.com/) >= 0.7.1 and
[Air](https://posit-dev.github.io/air/) >= 0.12.0 separately, then select them:

```r
options(
    languageserver.diagnostics_backend = "jarl",
    languageserver.formatting_backend = "air",
    languageserver.backend_options = list(
        jarl = list(executable = "/path/to/jarl", timeout = 10),
        air = list(executable = "/path/to/air", timeout = 10,
            env = c(XDG_CONFIG_HOME = "/path/to/config"))
    )
)
```

Without an executable override, commands are resolved on `PATH`. Equivalent LSP
settings use the names in the table above, with JSON objects for `backend_options`
and `env`. R options take precedence over workspace settings. Rule/style settings
belong in `.lintr`, `jarl.toml` or `air.toml`. Relevant R backend options and
`languageserver.formatting_style` are copied into workers for each invocation.

Jarl and Air receive current editor contents over stdin, including unsaved changes,
with the original absolute filename for configuration discovery. The working
directory is the nearest existing directory of the document. Pathless buffers use
the applicable workspace directory, or the server's working directory in a
folderless session, and a synthetic `__languageserver_untitled__.R` filename that
is not created. Lintr retains its existing text-only handling of pathless buffers.

Jarl currently supports R diagnostics; Air supports full-document R formatting.
CLI literate-document diagnostics return no results. Unsupported formatting modes
or document types return an explicit request error. Clients supporting dynamic
formatting registration, including vscode-R, receive updated providers after
workspace settings arrive and whenever the backend changes. Clients using static
capabilities retain the modes selected at startup; configure the backend through
R options before starting those clients. Their handlers still enforce the current
backend after a workspace setting change, but restarting with the same late
workspace settings cannot update their static capabilities.
On-type formatting with Air uses the server's local indentation fallback for
newlines, without starting a CLI. Lintr and styler retain their R
Markdown/Quarto, selection, and on-type behavior. Air follows its project formatting
settings rather than overriding them with LSP `tabSize`/`insertSpaces`. CLI edits use
LF line endings and otherwise preserve the formatter's terminal-newline output.

Changing the backend, execution options, or native configuration cancels obsolete
work and refreshes diagnostics. Configuration files are watched where supported;
a bounded background poll also refreshes idle buffers, including after a nearer
configuration file is created or deleted. A missing backend produces one
availability notification per relevant configuration state; explicit formatting
requests receive an error.

### Custom R adapters

An R option can supply an adapter descriptor instead of a built-in name:

```r
options(languageserver.diagnostics_backend = list(
    id = "my-linter",
    document_types = "r",
    needs_workspace_globals = FALSE,
    config_files = "my-linter.toml",
    run = function(request) {
        # Inspect request$content and return plain LSP Diagnostic lists.
        list()
    }
))
```

Formatting descriptors additionally declare `modes`, a subset of `document`,
`range`, `ranges`, and `on_type`, and return plain LSP TextEdit lists. Document
types are `r` and `literate`. Requests contain immutable buffer contents (a vector
of lines), URI, original/virtual path, language, workspace root, working directory,
resolved options, and formatting mode/ranges or on-type position/character. Ranges
and on-type positions use zero-based `row`/`col` code-point coordinates; returned
LSP positions must use UTF-16. Diagnostics receive the descriptor's `id` as their
source, keeping lintr code actions restricted to lintr diagnostics.

Adapters requesting workspace globals receive `request$globals`, including package
imports and static source dependencies. Configuration filenames in `config_files`
are checked in document ancestors; absolute paths are also supported. Use a stable,
unique ID and replace the descriptor when captured mutable configuration changes.
Custom functions must be serializable into an R worker and must not publish LSP
notifications or construct response objects. JSON settings select named built-ins;
custom descriptors are supplied through R options.

### Customizing server capabilities

Server capabilities are defined in
[capabilities.R](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R).
Users could override the capabilities by specifying the LSP configuration setting
`server_capabilities` or
`options(languageserver.server_capabilities)` in `.Rprofile`. For example, to turn off `definitionProvider`, one could either use LSP configuration

```json
"r": {
    "lsp": {
        "server_capabilities": {
            "definitionProvider": false
        }
    }
}
```

or R options

```r
options(
    languageserver.server_capabilities = list(
        definitionProvider = FALSE
    )
)
```

### Customizing formatting style

The default formatting backend uses [`styler`](https://github.com/r-lib/styler). It uses `styler::tidyverse_style(indent_by = options$tabSize)` as the default style where `options` is the [formatting
options](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_formatting).

The formatting style can be customized by specifying `languageserver.formatting_style` option which
is supposed to be a function that accepts an `options` argument mentioned above. You could consider to put the code in `.Rprofile`.

[`styler::tidyverse_style`](<https://styler.r-lib.org/reference/tidyverse_style.html>) provides numerous arguments to customize the formatting behavior. For example, to make it only work at indention scope:

```r
options(languageserver.formatting_style = function(options) {
    styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
})
```

To disable assignment operator fix (replacing `=` with `<-`):

```r
options(languageserver.formatting_style = function(options) {
    style <- styler::tidyverse_style(indent_by = options$tabSize)
    style$token$force_assignment_op <- NULL
    style
})
```

To further customize the formatting style, please refer to [Customizing styler](https://styler.r-lib.org/articles/customizing_styler.html).
