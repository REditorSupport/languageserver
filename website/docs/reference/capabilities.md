---
pageClass: feature-index
---

# Editor features and providers

Choose an editor action below to see an R example. Actions are available in **CRAN 0.3.22** unless marked **Development**. The development build adds two navigation providers and extends existing features, including completion inside glue strings and navigation to data-frame columns. [Development examples](/guide/development) · [Release history](/releases/)

## Which editor features can I use?

### Write and read R

| Editor action | Language Server Protocol method |
| --- | --- |
| [Complete a name, argument, value, or member](/guide/writing-r) and [inspect the selected suggestion](/guide/writing-r#inspect-a-completion) | `textDocument/completion`, `completionItem/resolve` |
| [Hover over a symbol for its definition, signature, or documentation](/guide/writing-r#inspect-a-completion) | `textDocument/hover` |
| [See a call’s signature and active parameter](/guide/writing-r#match-arguments-as-you-type) | `textDocument/signatureHelp` |
| [Show parameter names beside arguments and inspect a hint](/guide/reading-code#show-parameter-hints) | `textDocument/inlayHint`, `inlayHint/resolve` |
| [Highlight occurrences of the symbol under the cursor](/guide/reading-code#highlight-a-symbol) | `textDocument/documentHighlight` |
| [Color code by its role in R](/guide/reading-code#read-semantic-highlighting), across a file, after edits, or in a range | `textDocument/semanticTokens/full`, `textDocument/semanticTokens/full/delta`, `textDocument/semanticTokens/range` |
| [Show a swatch and choose a new color literal](/guide/reading-code#edit-a-color) | `textDocument/documentColor`, `textDocument/colorPresentation` |
| [See variable values beside the source during a debugging session](/guide/reading-code#see-values-while-debugging) | `textDocument/inlineValue` |

### Explore a file or project

| Editor action | Language Server Protocol method |
| --- | --- |
| [Open the outline and jump to a section or declaration](/guide/reading-code#navigate-the-outline) | `textDocument/documentSymbol` |
| [Search for symbols across the workspace](/guide/navigation#find-symbols-across-a-workspace) | `workspace/symbol` |
| [Go to a source definition](/guide/navigation#follow-a-function-into-another-file) | `textDocument/definition` |
| [Inspect a generated definition for an installed package function](/guide/navigation#generated-package-definitions) | `workspace/textDocumentContent`. The server can request `workspace/textDocumentContent/refresh` when the content changes. |
| [Find references to a binding](/guide/navigation#find-references-and-rename-a-binding) | `textDocument/references` |
| [Discover callers and callees in Call Hierarchy](/guide/navigation#inspect-incoming-and-outgoing-calls) | `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls` |
| [Show and open a call-count CodeLens](/guide/navigation#read-call-counts-with-codelens) | `textDocument/codeLens`, `codeLens/resolve` |
| [Browse parent and child classes in Type Hierarchy](/guide/classes-and-methods#browse-a-type-hierarchy) | `textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`, `typeHierarchy/subtypes` |
| [Detect a file link and open its target](/guide/reading-code#follow-a-file-link) | `textDocument/documentLink`, `documentLink/resolve` |
| [Fold code or progressively expand a selection](/guide/reading-code#fold-code-and-expand-a-selection) | `textDocument/foldingRange`, `textDocument/selectionRange` |

### Change code

| Editor action | Language Server Protocol method |
| --- | --- |
| [Check a rename target and update its uses](/guide/navigation#find-references-and-rename-a-binding) | `textDocument/prepareRename`, `textDocument/rename` |
| [Edit a roxygen parameter tag and its formal together](/guide/reading-code#link-a-parameter-to-its-documentation) | `textDocument/linkedEditingRange` |
| [Fix or suppress a finding, Fix All, extract, or inline code](/guide/editing-actions) | `textDocument/codeAction` |
| [Open the refactoring guide from a code-action menu](/guide/editing-actions#open-refactoring-help) | `workspace/executeCommand` with client support for `window/showDocument` |

### Diagnose and format

| Editor action | Language Server Protocol method |
| --- | --- |
| [See lint findings in the buffer or the Problems list](/guide/diagnostics-formatting#read-a-lint-finding) | The server sends `textDocument/publishDiagnostics` to the editor. |
| [Format an entire document](/guide/diagnostics-formatting) | `textDocument/formatting` |
| [Format a selection or multiple disjoint selections](/guide/diagnostics-formatting#format-one-or-more-selections) | `textDocument/rangeFormatting`, `textDocument/rangesFormatting` |
| [Format while typing](/guide/diagnostics-formatting#format-as-you-type) | `textDocument/onTypeFormatting` |

### Development providers

| Editor action | Language Server Protocol method |
| --- | --- |
| [Go to the source class of an object](/guide/classes-and-methods#open-the-type-or-implementation) | `textDocument/typeDefinition` |
| [Go to the selected implementation of a generic](/guide/classes-and-methods#open-the-type-or-implementation) | `textDocument/implementation` |

For features built on these providers, see the guides to [data frames and Polars](/guide/data-analysis), [classes and methods](/guide/classes-and-methods), and [R Markdown and Quarto](/guide/literate). The [development guide](/guide/development) describes additions to the released providers.

## How does the editor keep results up to date?

- **Open buffers.** The editor sends `textDocument/didOpen`, incremental `textDocument/didChange`, `textDocument/didSave`, and `textDocument/didClose`. Positions use UTF-16. The server reads unsaved text for completion and navigation and drops superseded work after edits.
- **Projects.** Adding or removing a workspace folder sends `workspace/didChangeWorkspaceFolders`. Changes to watched files arrive through `workspace/didChangeWatchedFiles`. The server uses these to update project information and lint or formatter configuration. Clients supporting work-done progress can display indexing updates through `window/workDoneProgress/create` and `$/progress`. [Workspace indexing](/reference/settings#workspace-indexing)
- **Settings.** The editor sends `workspace/didChangeConfiguration` when its settings change. For clients supporting dynamic registration, the server uses `client/registerCapability` and `client/unregisterCapability` to update the available formatting commands. If a selected backend is unavailable, `window/showMessage` can display the reason. [Server settings](/reference/settings)
- **Session.** The editor and server negotiate capabilities with `initialize` and `initialized`. Clients can cancel a pending request with `$/cancelRequest`, change protocol tracing with `$/setTrace`, and end a session with `shutdown` and `exit`. [Collect a server log](/guide/troubleshooting#capture-a-server-log)

## Why can two editors show different results?

- **Installed package source.** Go to Definition can show a generated function body when a definition has no source location. An editor supporting LSP text document content can open it as a read-only virtual document and refresh it. Other clients receive a generated temporary file. Editing that copy has no effect on the installed function.
- **Formatting commands.** With dynamic registration the editor can update its formatting commands when the backend changes. A client with static capabilities learns the available modes when it starts. For that client, set the formatting backend in the server’s R profile and restart it. [Available formatting modes](/guide/diagnostics-formatting)
- **Refactoring help.** Editors supporting both code-action documentation and opening a URL can show a documentation link in the refactoring menu. [Navigation and refactoring](/guide/navigation)
- **Glue highlighting (development).** An editor that supports `workspace/semanticTokens/refresh` can recolor already-open documents when you change glue highlighting settings. [Glue templates](/guide/development#edit-r-inside-a-glue-string)

## Override an advertised capability

If another tool already supplies a feature, set the corresponding capability in the R profile read at server startup. For example, to leave Go to Definition to another provider, add this to `.Rprofile` and restart the server.

```r
options(languageserver.server_capabilities = list(
    definitionProvider = FALSE
))
```

For names and option shapes, consult the table used by the installed version. [CRAN 0.3.22 capabilities](https://github.com/REditorSupport/languageserver/blob/v0.3.22/R/capabilities.R) · [Development capabilities](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R)
