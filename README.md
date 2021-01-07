# languageserver: An implementation of the Language Server Protocol for R

[![Gitter](https://badges.gitter.im/REditorSupport/community.svg)](https://gitter.im/REditorSupport/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Build Status](https://travis-ci.org/REditorSupport/languageserver.svg?branch=master)](https://travis-ci.org/REditorSupport/languageserver)
[![Github Action](https://github.com/REditorSupport/languageserver/workflows/build/badge.svg?branch=master)](https://github.com/REditorSupport/languageserver)
[![codecov](https://codecov.io/gh/REditorSupport/languageserver/branch/master/graph/badge.svg)](https://codecov.io/gh/REditorSupport/languageserver)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/languageserver)](https://cran.r-project.org/package=languageserver)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/languageserver)](https://cran.r-project.org/package=languageserver)

`languageserver` is an implementation of the Microsoft's [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) for the language of R.

It is released on CRAN and can be easily installed by

```r
install.packages("languageserver")
```

The development version of `languageserver` could be installed by running the following in R:

```r
# install.packages("devtools")
devtools::install_github("REditorSupport/languageserver")
```

## Language Clients

These editors are supported by installing the corresponding package.

- VSCode: [vscode-r-lsp](https://github.com/REditorSupport/vscode-r-lsp)

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
        # devtools::install_github("REditorSupport/languageserver")
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

- JupyterLab: [jupyterlab-lsp](https://github.com/krassowski/jupyterlab-lsp)

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
- [ ] [codeActionProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_codeAction)
- [ ] [codeLensProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_codeLens)
- [x] [documentFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_formatting)
- [x] [documentRangeFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rangeFormatting)
- [x] [documentOnTypeFormattingProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_onTypeFormatting)
- [x] [renameProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename)
- [x] [prepareRenameProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename)
- [x] [documentLinkProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentLink)
- [x] [colorProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentColor)
- [x] [colorPresentation](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_colorPresentation)
- [x] [foldingRangeProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_foldingRange)
- [ ] [selectionRangeProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_selectionRange)
- [ ] [prepareCallHierarchy](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareCallHierarchy)
- [ ] [callHierarchyIncomingCalls](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#callHierarchy_incomingCalls)
- [ ] [callHierarchyOutgoingCalls](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#callHierarchy_outgoingCalls)
- [ ] [semanticTokens](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens)
- [ ] [linkedEditingRange](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_linkedEditingRange)
- [ ] [executeCommandProvider](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspace_executeCommand)

## Settings

`languageserver` exposes the following settings via `workspace/didChangeConfiguration`

```json
{
    "r.lsp.debug": {
      "type": "boolean",
      "default": false,
      "description": "Debug R Language Server"
    },
    "r.lsp.diagnostics": {
      "type": "boolean",
      "default": true,
      "description": "Enable Diagnostics"
    }
}
```

## FAQ

### Linters

With [lintr](https://github.com/jimhester/lintr) v2.0.0, the linters can be specified by creating the `.lintr` file at the project or home directory. Details can be found at lintr [documentation](https://github.com/jimhester/lintr#project-configuration). The option `languageserver.default_linters` is now deprecated in favor of the `.lintr` approach.

### Customizing server capbabilities

Server capabilities are defined in [capabilities.R](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R). Users could override the settings by specifiying `languageserver.server_capabilities` option in `.Rprofile`. For example,
the following code will turn off `definitionProvider`,

```r
options(
    languageserver.server_capabilities = list(
        definitionProvider = FALSE
    )
)
```

Please only use this option to disable providers and do not enable any providers that have not been implemented. Changing any other entries may cause unexpected behaviors on the server.

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

### Using persistent cache for formatting by styler

With [`styler`](https://github.com/r-lib/styler) v1.3, the formatting of top-level expressions
can be cached by [`R.cache`](https://github.com/HenrikBengtsson/R.cache), which significantly improves the formatting performance by skipping the expressions that are known in cache to be already formatted. By default, the cache only works within the current session.
To make it work across sessions, user must run the following command to perform a one-time authorization to create a permanent directory in user home in an *interactive* R session:

```r
R.cache::getCachePath()
```

The first time the command is run, it will ask user whether to create a permanent cache directory. Type `Y` and enter, the cache directory will be created, and then all cache operations will be done across sessions so that formatted expressions could be remembered globally.

To check if a permanent directory is used or not, run the following command:

```r
styler::cache_info()
```
