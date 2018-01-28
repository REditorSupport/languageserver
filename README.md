# languageserver: An implementation of the Language Server Protocol for R

`languageserver` is an implement of the Microsoft's [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) for the language of R.

The development version of `languageserver` could be installed by running the following in R
```
source("https://install-github.me/REditorSupport/languageserver")
```

## Language Clients

These editors are supported by installing the corresponding package.

- VSCode: [vscode-r-lsp](https://github.com/REditorSupport/vscode-r-lsp)

- Atom: [atom-ide-r](https://github.com/REditorSupport/atom-ide-r)

- Sublime Text: [LSP](https://github.com/tomv564/LSP) with settings
```js
"rlangsvr": {
    "command": [
        "R",
        "--quiet",
        "--slave",
        "-e",
        "languageserver::run()"
    ],
    "enabled": true,
    "languageId": "r",
    "scopes": [
        "source.r"
    ],
    "syntaxes": [
        "Packages/R/R.sublime-syntax",
        "Packages/R-Box/syntax/R Extended.sublime-syntax"
    ]
}
```

- Vim/NeoVim: [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim) with settings
```vim
let g:LanguageClient_serverCommands = {
    \ 'r': ['R', '--quiet', '--slave', '-e', 'languageserver::run()'],
    \ }
```

- EMacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode) (untested)

## Services Implemented

`languageserver` is still under active development, the following services have been implemented:

- [x] textDocumentSync (diagnostics)
- [ ] hoverProvider
- [x] completionProvider
- [x] signatureHelpProvider
- [ ] definitionProvider
- [ ] referencesProvider
- [ ] documentHighlightProvider
- [ ] documentSymbolProvider
- [ ] workspaceSymbolProvider
- [ ] codeActionProvider
- [ ] codeLensProvider
- [ ] documentFormattingProvider
- [ ] documentRangeFormattingProvider
- [ ] documentOnTypeFormattingProvider
- [ ] renameProvider
- [ ] documentLinkProvider
- [ ] executeCommandProvider
