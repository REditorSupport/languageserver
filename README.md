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

- EMacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with settings
```elisp
(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
			 '("R" "--slave" "-e" "languageserver::run()"))
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


## FAQ

### Linters

With [lintr](https://github.com/jimhester/lintr) 1.0.3, the linters can be specified by creating the `.lintr` file at the project or home directory. Details can be found at lintr documentation.
