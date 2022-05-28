# languageserver: An implementation of the Language Server Protocol for R

[![Gitter](https://badges.gitter.im/REditorSupport/community.svg)](https://gitter.im/REditorSupport/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Build Status](https://travis-ci.org/REditorSupport/languageserver.svg?branch=master)](https://travis-ci.org/REditorSupport/languageserver)
[![Github Action](https://github.com/REditorSupport/languageserver/workflows/build/badge.svg?branch=master)](https://github.com/REditorSupport/languageserver)
[![codecov](https://codecov.io/gh/REditorSupport/languageserver/branch/master/graph/badge.svg)](https://app.codecov.io/gh/REditorSupport/languageserver)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/languageserver)](https://cran.r-project.org/package=languageserver)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/languageserver)](https://cran.r-project.org/package=languageserver)
[![r-universe](https://reditorsupport.r-universe.dev/badges/languageserver)](https://reditorsupport.r-universe.dev/ui#package:languageserver)

`languageserver` is an implementation of the Microsoft's [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) for the language of R.

- [Installation](#installation)
- [Language Clients](#language-clients)
- [Services Implemented](#services-implemented)
- [Settings](#settings)
- [FAQ](#faq)
  - [Linters](#linters)
  - [Customizing server capabilities](#customizing-server-capabilities)
  - [Customizing formatting style](#customizing-formatting-style)

## Installation

A few dependencies are required beforehand:

```bash
# On Debian, Ubuntu, etc.
apt install --assume-yes --no-install-recommends build-essential libcurl4-openssl-dev libssl-dev libxml2-dev r-base

# On Fedora, Centos, etc.
dnf install --assumeyes --setopt=install_weak_deps=False @development-tools libcurl-devel libxml2-devel openssl-devel R

# On Alpine
apk add --no-cache curl-dev g++ gcc libxml2-dev linux-headers make R R-dev
```

`languageserver` is released on CRAN and can be easily installed by

```r
install.packages("languageserver")
```

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

The following editors are supported by installing the corresponding extensions:

- VS Code: [vscode-R](https://github.com/REditorSupport/vscode-R)

- Atom: [atom-ide-r](https://github.com/REditorSupport/atom-ide-r)

- Sublime Text: [R-IDE](https://github.com/REditorSupport/sublime-ide-r)

- Vim/NeoVim: [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) with settings

    ```vim
    let g:LanguageClient_serverCommands = {
        \ 'r': ['R', '--slave', '-e', 'languageserver::run()'],
        \ }
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
            "args" : [ "--slave", "-e", "languageserver::run()"],
            "filetypes" : ["r"]
        }
    }
    ```

- Emacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode)

- JupyterLab: [jupyterlab-lsp](https://github.com/jupyter-lsp/jupyterlab-lsp)

- [BBEdit](https://www.barebones.com/products/bbedit/): preconfigured in version 14.0 and later; see the [BBEdit LSP support page](https://www.barebones.com/support/bbedit/lsp-notes.html) for complete details.

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
- [ ] [codeLensProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_codeLens)
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
- [ ] [semanticTokens](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens)
- [ ] [linkedEditingRange](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_linkedEditingRange)
- [ ] [executeCommandProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_executeCommand)

## Settings

`languageserver` exposes the following settings via LSP configuration.

settings | default | description
----     | -----   | -----
`r.lsp.debug`  | `false` | increase verbosity for debug purpose
`r.lsp.log_file` | `null` | file to log debug messages, fallback to stderr if empty
`r.lsp.diagnostics` | `true` | enable file diagnostics via [lintr](https://github.com/r-lib/lintr)
`r.lsp.rich_documentation` | `true` | rich documentation with enhanced markdown features
`r.lsp.snippet_support` | `true` | enable snippets in auto completion
`r.lsp.max_completions` | 200 | maximum number of completion items
`r.lsp.lint_cache` | `false` | toggle caching of lint results
`r.lsp.server_capabilities` | `{}` | override server capabilities defined in [capabilities.R](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R). See FAQ below.
`r.lsp.link_file_size_limit` | 16384 | maximum file size (in bytes) that supports document links

These settings could also specified in `.Rprofile` file via `options(languageserver.<SETTING_NAME> =  <VALUE>)`. For example,

```r
options(languageserver.snippet_support = FALSE)
```

will turn off snippet support globally. LSP configuration settings are always overriden by `options()`.

## FAQ

### Linters

With [lintr](https://github.com/r-lib/lintr) v2.0.0, the linters can be specified by creating the `.lintr` file at the project or home directory. Details can be found at lintr [documentation](https://lintr.r-lib.org/articles/lintr.html).

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

The language server uses [`styler`](https://github.com/r-lib/styler) to perform code formatting. It uses `styler::tidyverse_style(indent_by = options$tabSize)` as the default style where `options` is the [formatting
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
