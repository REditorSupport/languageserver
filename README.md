# languageserver: An implementation of the Language Server Protocol for R

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

## Language Clients

These editors are supported by installing the corresponding package.

- VSCode: [vscode-r-lsp](https://github.com/REditorSupport/vscode-r-lsp)

- Atom: [atom-ide-r](https://github.com/REditorSupport/atom-ide-r)

- Sublime Text: [LSP](https://github.com/tomv564/LSP)

- Vim/NeoVim: [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) with settings
```vim
let g:LanguageClient_serverCommands = {
    \ 'r': ['R', '--quiet', '--slave', '-e', 'languageserver::run()'],
    \ }
```

- EMacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with settings
```elisp
(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
			 '("R" "--quiet" "--slave" "-e" "languageserver::run()"))
(add-hook 'R-mode-hook #'lsp-R-enable)
```

## Services Implemented

`languageserver` is still under active development, the following services have been implemented:

- [x] textDocumentSync (diagnostics)
- [x] hoverProvider
- [x] completionProvider
- [x] signatureHelpProvider
- [ ] definitionProvider
- [ ] referencesProvider
- [ ] documentHighlightProvider
- [ ] documentSymbolProvider
- [ ] workspaceSymbolProvider
- [ ] codeActionProvider
- [ ] codeLensProvider
- [x] documentFormattingProvider
- [x] documentRangeFormattingProvider
- [ ] documentOnTypeFormattingProvider
- [ ] renameProvider
- [ ] documentLinkProvider
- [ ] executeCommandProvider


## Diagnostics settings

User can specify the default linters in `.Rprofile`. Please note that this setting
is ignored if a `.lintr` file is found.

```r
options(languageserver.default_linters = lintr::with_defaults(
    line_length_linter = lintr::line_length_linter(100),
    object_length_linter = NULL,
    object_name_linter = NULL,
    commented_code_linter = NULL
))
```
