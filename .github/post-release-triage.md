# Post-CRAN triage: 0.3.19

Reviewed on 2026-09-14 against the released features, current source and tests,
and the open GitHub issues. The four items below were closed on that date;
the remaining items retain the follow-up requirements documented here.

## Closed as implemented or superseded

| Item | Evidence | Disposition |
| --- | --- | --- |
| [#94: extract/inline refactoring](https://github.com/REditorSupport/languageserver/issues/94) | [PR #753](https://github.com/REditorSupport/languageserver/pull/753), released in 0.3.19, adds extract-variable, extract-function, and single-use local-variable inlining. [Refactoring tests](../tests/testthat/test-refactor.R) cover free variables, live-out bindings, edit versions, unsafe contexts, and literate-cell boundaries. | Closed as implemented. Applicability remains conservative; arbitrary R transformations are not promised. This reconciles GitHub with the existing NEWS entry. |
| [#307: semantic tokens](https://github.com/REditorSupport/languageserver/issues/307) | Semantic tokens shipped in 0.3.17; 0.3.19 adds cached tokens and deltas. [Provider tests](../tests/testthat/test-semantic-tokens.R) cover full, range, delta reconstruction, UTF-16 positions, and function declarations. | Closed as implemented; specific semantic-token bugs remain in their own issues. |
| [#396: diagnostic code actions](https://github.com/REditorSupport/languageserver/issues/396) | [PR #746](https://github.com/REditorSupport/languageserver/pull/746), released in 0.3.19, provides preferred fixes for common linters and conflict-aware `source.fixAll`. [Code-action tests](../tests/testthat/test-code-action.R) cover assignment, commas, logical constants, layout, suppression, and overlapping fixes. | Closed as implemented. An upstream lintr edit API would be a separate integration enhancement. |
| [Draft PR #397](https://github.com/REditorSupport/languageserver/pull/397) | Its initial diagnostic-fix implementation is superseded by merged PR #746. | Closed as superseded; the original proposal remains available in the PR history. |

## Keep open or close after follow-up

### #621: multi-root workspaces — retain the profile-isolation requirement

[PR #719](https://github.com/REditorSupport/languageserver/pull/719), released
in 0.3.18, implements workspace-folder routing. The
[workspace tests](../tests/testthat/test-workspace.R) verify symbol lookup,
dynamic folder addition, and fallback-workspace cleanup. This satisfies much
of [#621](https://github.com/REditorSupport/languageserver/issues/621).

The issue also asks for each folder's `.Rprofile` and settings to be respected.
[`workspace_startup_packages()`](../R/workspace.R) currently shares one cached
startup-package list across workspaces, and [task workers](../R/task.R) start
with common process/profile options rather than a folder-specific startup
directory. Keep the issue open for this remaining work. Acceptance should use
two folders with different profiles and library paths and verify that package
resolution and diagnostics remain isolated, including after folder removal
and addition. Folder-symbol tests alone do not establish profile isolation.

### #726: diagnostics configuration — distinguish the original fix from new cases

The original [#726 report](https://github.com/REditorSupport/languageserver/issues/726)
used languageserver 0.3.16 with lintr 3.3.0.1. Explicit `parse_settings = TRUE`
shipped in 0.3.17 through
[PR #706](https://github.com/REditorSupport/languageserver/pull/706), as noted
in the existing issue reply. The stabilization changes additionally preserve
the path for nested configuration and exclusions in saved and new `.R`,
`.Rmd`, and `.qmd` buffers. The
[configuration regression tests](../tests/testthat/test-diagnostics-configuration.R)
exercise these cases and restore lintr settings after failures.

Close with the original fix and the stabilization release version once these
additional changes land. If the reporter still reproduces the saved `.R`
case, request their `.lintr`, exact paths, and installed package versions;
do not assume every configuration report has the same cause.

### #735: Quarto semantic tokens — await editor confirmation

[PR #752](https://github.com/REditorSupport/languageserver/pull/752) scopes
providers to literate R cells. Parsing the exact document from
[#735](https://github.com/REditorSupport/languageserver/issues/735) now produces
semantic tokens only on line 19 (`print("hello")`, using one-based lines);
YAML, prose, math, and cell metadata produce none. The
[literate-document tests](../tests/testthat/test-literate.R) also exercise
region isolation and Quarto requests through the server.

Keep open pending confirmation in Cursor/VS Code with the Quarto extension
and languageserver 0.3.19 or later. A maintainer already requested confirmation
of #752 on 2026-08-24; avoid posting a duplicate request. If it persists, obtain
the extension versions and actual semantic-token response to distinguish
server output from the client's embedded-document mapping.

### #731 and #687: startup and completion performance — retain reproducible workloads

[#731](https://github.com/REditorSupport/languageserver/issues/731) reports
a worker startup timeout with Windows, R 4.6.0, and an `renv` profile. Workers
now start asynchronously and task failures are contained, but these changes
do not prove that slow `renv` activation completes successfully. Retain the
issue until a minimal project is exercised on that platform, recording cold
startup time and whether worker readiness and diagnostics eventually succeed.

[#687](https://github.com/REditorSupport/languageserver/issues/687) reports
repeated 20-second completion stalls on a remote cluster. 0.3.19 improves
package-resolution reuse, caching, and scheduling, but local warm-provider
timings cannot establish a fix for remote filesystem and package startup
costs. Use the [typing benchmark](../inst/benchmarks/README.md) with cold-start
and diagnostics-enabled measurements. Ask for a minimal package list, remote
filesystem details, and timings before and after idle periods if the problem
persists; close only after reproducing the workload or receiving confirmation.

### PR #665: formatting-style documentation — review independently

[PR #665](https://github.com/REditorSupport/languageserver/pull/665) adds a
`styler.equals::equals_style()` example for
[#664](https://github.com/REditorSupport/languageserver/issues/664). It does
not repair the Neovim setup. Keep it separate from the current README fixes
and verify the suggested package API and intended style before merging it.
The existing README already demonstrates disabling the assignment-operator
rewrite directly; avoid presenting the new dependency as required.

## Applying this review

Track the remaining items by their concrete next action: profile isolation,
regression verification, or an environment-specific reproduction. Include the shipped version and
linked tests when resolving a feature request. Do not close a reproducible
bug merely because a related feature or performance improvement shipped.
