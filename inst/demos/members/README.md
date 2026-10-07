# Static member demos

These editable examples demonstrate completion, signature help and hover for
`$` members. Only the Polars demo needs an extra package; the list and environment
demos use base R, and R6 is already a languageserver dependency.

Install this branch into the R library used by VS Code, then restart the R
language server. For example, from the repository root:

```r
install.packages(".", repos = NULL, type = "source")
```

Keep the `member_completion` setting enabled (the default). Open a demo file;
there is no need to run its code in the R session. For package examples, allow
the background metadata preparation to finish after opening the file.

| Demo | Try completion after | Signature/hover to inspect |
| --- | --- | --- |
| [Polars query](01-polars.R) | `grouped$` → `agg`, `head`; `result$` → `collect`, `filter`; `pl$col("Species")$str$` → `to_uppercase` | `group_by(..., .maintain_order = FALSE)` and `.maintain_order` documentation |
| [List factory](02-list-factory.R) | `client$` → `config`, `request`; `response$` → `decode`, `status`; `decoded$data$` → `endpoint`, `path` | `request(path, timeout = 30)`, `decode(simplify = TRUE)`, and the literal `status` field |
| [Fluent environment](03-fluent-environment.R) | `pipeline$filter(...)$` → `collect`, `filter`, `limit`, `source`; `output$metadata$` → `cached` | `limit(n = 10L)` and `collect(format = "list")` |
| [R6 inheritance](04-r6-inheritance.R) | `dataset$select(...)$log(...)$` → inherited and child members; `preview$` → `data`, `rows` | `preview(n = 6L)`, `log(message, level = "info")` |

For completion, temporarily delete the method after the chosen `$` and trigger
completion. For signature help, place the cursor inside that method's argument
list and use VS Code's **Trigger Parameter Hints** command. Hover over the method
name; in the Polars example, also hover over `.maintain_order` to see its argument
documentation. The comments in each file identify more positions to try.

For example, the base-R list demo changes shape at each call:

```r
client$request("/records")$decode(simplify = FALSE)$data$path
#      request(path, timeout = 30)
#                          decode(simplify = TRUE)
#                                                    endpoint, path after data$
```

The provider reads syntax and inert metadata. It does not execute demo
expressions, arguments, constructors, methods or active getters. Loading package
metadata can run package initialization. The R6 `expensive` getter deliberately
throws if called, and is still offered by completion without being invoked.
Unsupported receiver or result patterns remain unknown; a matching bare
function name is not used as a substitute for member signature/hover.

To check the examples automatically from the repository root:

```sh
Rscript inst/demos/members/check.R
```

This checker reads the files as text and calls the actual completion, signature
and hover providers. It prints the observed members and signatures and checks
them against the examples above. It loads the installed languageserver from the
branch, or uses `pkgload` to load the checkout when available. Polars checks are
skipped when the package is absent. It never sources or evaluates a demo file.
This is reproducible provider output, not a recording of a manual VS Code run.
The [captured output](provider-output.txt) records a passing run with R 4.6.1 and
Polars 1.16.0.
