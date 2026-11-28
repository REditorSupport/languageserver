# Server settings

To change a setting for one editor, send it in that client's R language server configuration. For example, this LSP configuration asks for inline signatures and value previews in the completion list.

```json
{
  "r": {
    "lsp": {
      "completion_label_details": true
    }
  }
}
```

Clients use different configuration files. In VS Code's `settings.json`, the corresponding key is `"r.lsp.completion_label_details"`. To set the same value in R, put the following in an `.Rprofile` read by the **server's R process**, then restart the language server.

```r
options(languageserver.completion_label_details = TRUE)
```

An explicit `languageserver.*` R option takes precedence over the corresponding workspace setting. The tables use the key after `r.lsp.` or `languageserver.` and show defaults for CRAN 0.3.22. The final table covers additions in development.

## Completion, help, and the outline

| Setting | Default | Effect |
| --- | --- | --- |
| `member_completion` | `true` | Infer members such as `obj$field` and S4 slots such as `obj@slot`. Also supplies member arguments, signatures, hover help, and definitions where the source is known. |
| `s3_dispatch` | `true` | Use inferred S3 methods for argument suggestions, signature help, and hover. Can be set independently of member completion. |
| `snippet_support` | `true` | Insert supported completions as snippets in clients that accept them. |
| `completion_label_details` | `false` | Show signatures and type or value previews alongside suggestions in clients that display label details. Selected-item details also work with the default. |
| `max_completions` | `200` | Maximum number of suggestions to return. |
| `rich_documentation` | `true` | Render installed-package help as rich Markdown when `rmarkdown` and Pandoc are available. Otherwise the server uses text help. |
| `inlay_hints_minimum_arguments` | `2` | Minimum number of supplied arguments for a call to show parameter-name hints. |
| `inlay_hints_minimum_argument_length` | `2` | Minimum number of characters in a parameter name for a hint. An initial dot is excluded. |
| `nline_to_break_succession` | `2` | Number of consecutive blank lines that separate section groups in the outline and folding ranges. |
| `link_file_size_limit` | `16777216` | Maximum size in bytes (16 MiB) of the target file when resolving a document link. |

## Diagnostics and formatting

| Setting | Default | Effect |
| --- | --- | --- |
| `diagnostics` | `true` | Publish diagnostics from the selected linter. |
| `diagnostics_backend` | `"lintr"` | Select `lintr` or `jarl` for diagnostics. |
| `formatting_backend` | `"styler"` | Select `styler` or `air` for formatting. |
| `backend_options` | `{}` | Per-backend overrides for the executable, timeout in seconds (10 by default), and named environment variables. |
| `lint_cache` | `false` | Allow lintr to cache results for files on disk. |
| `diagnostics_cache_ttl` | `5` | Seconds the server can reuse matching diagnostics in memory. Set to `0` to disable reuse. |
| `diagnostics_delay` | `0.75` | Seconds to wait before linting after a parse. |
| `parse_delay` | `0.15` | Seconds to wait before parsing after a text change. |
| `diagnostics_cache_max_mb` | `16` | Memory budget in MiB for a workspace’s diagnostics cache. |
| `parse_cache_max_mb` | `64` | Memory budget in MiB for a workspace’s cached document parses. |

The server creates the two memory budgets when it opens a workspace. Set them in the server’s R profile before opening the project. For installation commands and the formatting modes available with each backend, see [Diagnose and format code](/guide/diagnostics-formatting). To supply an R function as the formatting style or a custom backend, see [Custom styles and adapters](/reference/adapters).

## Workspace indexing

| Setting | Default | Effect |
| --- | --- | --- |
| `index_mode` | `"auto"` | `auto` selects `full` in a package and `lazy` in a script folder. `full` discovers files across the workspace. `lazy` starts with open files and their static source dependencies. `off` disables workspace indexing and uses the legacy package-loading behavior. |
| `index_include` | `"**/*.R"` | Workspace-relative glob or list of globs to include. Matching ignores case. |
| `index_exclude` | See below | Workspace-relative glob or list of globs to exclude. |
| `index_max_files` | `10000` | Maximum number of eligible R files discovered per workspace. |
| `index_max_file_size_mb` | `2` | Maximum size in MiB of each file included in the index. |
| `index_batch_size` | `20` | Maximum number of shallow file summaries to build in one idle batch. |
| `index_time_budget_ms` | `25` | Approximate time budget in milliseconds for each batch. |
| `index_persistent_cache` | `true` | Save validated summaries from a full index on disk. Lazy indexes live in memory. |

By default, indexing skips directories matched by these globs.

```json
[
  "**/.git/**", "**/.svn/**", "**/.hg/**", "**/renv/**", "**/packrat/**",
  "**/.Rproj.user/**", "**/.cache/**", "**/node_modules/**", "**/build/**", "**/dist/**"
]
```

Package metadata and rendered help use a separate disk cache rooted at `tools::R_user_dir("languageserver", "cache")`. Set the R option `languageserver.package_cache_dir` to a directory or to `FALSE` to disable this cache. The environment variable `R_LANGSVR_PACKAGE_CACHE_DIR` can set the directory when the R option is absent.

## Logging and client capabilities

| Setting | Default | Effect |
| --- | --- | --- |
| `debug` | `false` | Log details about startup and server activity. |
| `trace` | `false` | Include protocol trace output. A client can also change tracing during a session. |
| `log_file` | `null` | Write logs to standard error by default. Supply a path to append to a file. |
| `server_capabilities` | `{}` | Override individual entries in the capabilities advertised to the client. See [Client capabilities](/reference/capabilities). |

## Development settings

These keys are available in [development builds](/guide/development).

| Setting | Default | Effect |
| --- | --- | --- |
| `glue_highlighting` | `true` | Use semantic tokens to color embedded R in recognized glue templates. |
| `glue_highlight_functions` | `{}` | Customize highlighting by exact call spelling. Set its value to the position of an unnamed template argument (starting at 1), a named argument, or `false` to disable highlighting for that spelling. |
| `project_helpers` | `{}` | Map caller globs to workspace-relative helper filenames for Go to Definition. Requires an active workspace index. |

For slow metadata workers, development builds also accept the `R_LANGSVR_TIMEOUT_SCALE` environment variable. A value of `3` gives those workers three times their usual time limit.

[Released defaults](https://github.com/REditorSupport/languageserver/blob/v0.3.22/R/settings.R) · [Development defaults](https://github.com/REditorSupport/languageserver/blob/master/R/settings.R)
