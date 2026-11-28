# Client capabilities

When an editor connects, it tells the server which Language Server Protocol (LSP) features it understands. The server replies with the operations it can provide. An editor may give an operation a different command name or require a display setting before you can see its results.

## Which editor features can I use?

| To do this | Look for this client feature |
| --- | --- |
| Complete names and arguments, inspect help, or check the next argument | Completion, Hover, and Signature Help. Snippet support lets the cursor land inside a completed call. Completion label details allow an inline signature or value preview. |
| Follow a name, find its uses, or change it throughout its scope | Go to Definition, Find References, document highlights, and Rename Symbol. Clients supporting Prepare Rename can check the target before asking for a new name. |
| Find a symbol across a project or inspect a file’s structure | Workspace Symbols, document symbols (Outline), folding, and selection ranges. Work-done progress lets the editor show indexing activity. |
| Explore relationships | Call Hierarchy shows incoming and outgoing calls. Type Hierarchy lets you explore supported class declarations. Clients with CodeLens can display call counts above function definitions. |
| Fix or restructure code | Code Actions provide applicable quick fixes, Fix All, Extract, and Inline actions. Clients that support snippet workspace edits can link the new name between an extraction and its use. |
| Read code with visual hints | Semantic highlighting uses the editor theme. Inlay hints show parameter names. Linked editing can update a roxygen `@param` name and the corresponding function parameter together. Each requires its corresponding editor feature. |
| Follow a file or inspect a color literal | Document links resolve file targets. Document colors and color presentations let the editor show swatches and offer replacement spellings. |
| See a variable’s value while stepping through code | Inline Values require a paused debugger and a client or debug adapter that requests variable lookups for the current stack frame. |

**Development.** Go to Implementation can open the selected S3 method for a statically understood call. Go to Type Definition can open the class declaration of an R6, RefClass, S4, or S7 receiver with a known source declaration. The editor must expose those commands. [Development features](/guide/development)

## Why can two editors show different results?

- **Installed package source.** Go to Definition can show a generated function body when a definition has no source location. An editor supporting LSP text document content can open it as a read-only virtual document and refresh it. Other clients receive a generated temporary file. Editing that copy has no effect on the installed function.
- **Formatting commands.** With dynamic registration the editor can update its formatting commands when the backend changes. A client with static capabilities learns the available modes when it starts. For that client, set the formatting backend in the server’s R profile and restart it. [Available formatting modes](/guide/diagnostics-formatting)
- **Refactoring help.** Editors supporting both code-action documentation and opening a URL can show a documentation link in the refactoring menu. [Navigation and refactoring](/guide/navigation)

## Override an advertised capability

If another tool already supplies a feature, set the corresponding capability in the R profile read at server startup. For example, to leave Go to Definition to another provider, add this to `.Rprofile` and restart the server.

```r
options(languageserver.server_capabilities = list(
    definitionProvider = FALSE
))
```

For names and option shapes, consult the table used by the installed version. [CRAN 0.3.22 capabilities](https://github.com/REditorSupport/languageserver/blob/v0.3.22/R/capabilities.R) · [Development capabilities](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R)
