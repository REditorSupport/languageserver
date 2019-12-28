# languageserver: An implementation of the Language Server Protocol for R

[![Build Status](https://travis-ci.org/REditorSupport/languageserver.svg?branch=master)](https://travis-ci.org/REditorSupport/languageserver)
[![Github Action](https://github.com/REditorSupport/languageserver/workflows/build/badge.svg?branch=master)](https://github.com/REditorSupport/languageserver)
[![codecov](https://codecov.io/gh/REditorSupport/languageserver/branch/master/graph/badge.svg)](https://codecov.io/gh/REditorSupport/languageserver)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/languageserver)](https://cran.r-project.org/package=languageserver)
[![](http://cranlogs.r-pkg.org/badges/grand-total/languageserver)](https://cran.r-project.org/package=languageserver)

`languageserver` is an implement of the Microsoft's [Language Server Protocol](https://microsoft.github.io/language-server-protocol) for the language of R.

It is released on CRAN and can be easily installed by
```
install.packages("languageserver")
```

The development version of `languageserver` could be installed by running the following in R
```
source("https://install-github.me/REditorSupport/languageserver")
```

## Rmarkdown

The R package [`knitr`](https://github.com/yihui/knitr) is required to enable languageserver for Rmarkdown files. `languageserver` doesn't specify `knitr` as a dependency however, users may need to install it manually.

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
    
    or use [coc-r-lsp](https://github.com/neoclide/coc-r-lsp) with [coc.nvim](https://github.com/neoclide/coc.nvim)

- EMacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with settings
    ```elisp
    (lsp-register-client
        (make-lsp-client :new-connection
            (lsp-stdio-connection '("R" "--slave" "-e" "languageserver::run()"))
            :major-modes '(ess-r-mode inferior-ess-r-mode)
            :server-id 'lsp-R))
    ```

## Services Implemented

`languageserver` is still under active development, the following services have been implemented:

- [x] textDocumentSync (diagnostics)
- [x] hoverProvider
- [x] completionProvider
- [x] signatureHelpProvider
- [x] definitionProvider
- [ ] referencesProvider
- [x] documentHighlightProvider
- [x] documentSymbolProvider
- [x] workspaceSymbolProvider
- [ ] codeActionProvider
- [ ] codeLensProvider
- [x] documentFormattingProvider
- [x] documentRangeFormattingProvider
- [x] documentOnTypeFormattingProvider
- [ ] renameProvider
- [ ] documentLinkProvider
- [ ] executeCommandProvider

## Settings

`languageserver` exposes the following settings via `workspace/didChangeConfiguration`

```js
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
