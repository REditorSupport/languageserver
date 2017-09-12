# languageserver: An implementation of the Language Server Protocol for R

`languageserver::server()` reads and writes from STDIN/STDOT following Microsoft's [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md).

```r
R --quiet --slave -e 'languageserver::server()'
```

## Language Clients

- [LSP](https://github.com/tomv564/LSP) is a universal Sublime Text client
- [vim-lsp](https://github.com/prabirshrestha/vim-lsp) is a universal Vim/Neovim client for the Language Server Protocol
- [lsp-mode](https://github.com/emacs-lsp/lsp-mode) is a universal Emacs client for the Language Server Protocol

Atom and VSCode do not have universal clients, but they have extensive supports for LSP:
- [Atom](https://github.com/atom/atom-languageclient)
- [VSCode](https://code.visualstudio.com/docs/extensionAPI/language-support)
