# Plan for the R languageserver documentation site

Publish a searchable guide at `https://reditorsupport.github.io/languageserver/` for people who write R in an editor. The home page should let a reader choose **Set up my editor** or **Explore the features**. Someone who already has the server running should be able to go straight to an example.

Use a standalone VitePress project under `website/`, following vscode-R’s approach to Markdown, local search, page outlines, and “Edit this page” links. The two sites can share familiar navigation. The languageserver site should own the R examples and explain how the server arrives at a result. Link to the vscode-R guide for VS Code commands, settings UI, and screenshots. [vscode-R proposal](https://github.com/REditorSupport/vscode-R/pull/1942) · [vscode-R language service guide](https://reditorsupport.github.io/vscode-R/features/language-service)

## What to publish first

| Page | What the reader will do |
| --- | --- |
| Get started in five minutes | Install `languageserver` from CRAN in the R library their editor uses, connect the editor, and try completion, Signature Help, Go to Definition, and Rename in one file. |
| Set up your editor | Start with tested paths for VS Code and Neovim. Give each the same connection check. Link to Eglot and lsp-mode for Emacs and the other clients listed in the README. For readers configuring another LSP client, give the stdio launch command `R --no-echo -e "languageserver::run()"`. |
| Write and understand R | Complete a function after `stats::` or a list member after `$`, read hover help, and discover a function’s arguments. Use the bundled S3 example to show why a data frame and a numeric vector offer different arguments, and why an unknown input shows the generic signature. |
| Navigate and refactor a project | Open a folder, jump from a call to its definition, find its callers, and rename a parameter. Then select an expression and use **Extract expression to variable**, showing the edited code. Explain how the workspace root, static `source()` paths, and indexing affect cross-file results. |
| Diagnose and format code | Reproduce a lint finding, find its explanation, and format a selection. Give the default `lintr` and `styler` installation steps here. Show how to select Jarl or Air and which document types and formatting actions each supports. |
| Edit R Markdown and Quarto | Open an `.Rmd` or `.qmd` and show where R assistance applies by moving the cursor between an executable R chunk and prose. Complete a name and format a selection within the chunk. |
| Browse all editor features | Look up each provider by editor action or protocol method, jump to a worked example, and check whether it ships on CRAN or requires a development build. Use the release history to look up an older installed version. |
| Troubleshoot a missing result | Start from symptoms such as “I get no completions,” “Go to Definition misses another file,” or “formatting is unavailable.” Walk through the editor’s R executable, `.libPaths()`, workspace root, selected backend, and server logs as appropriate. |
| Reference | Look up settings by task, client capability requirements, and the `languageserver::run()` entry point. Link directly to the R help and release notes. For each setting, show its default and where to set it. An explicit `options(languageserver.*)` value in the server process takes precedence over the corresponding client setting. |

## A reader’s first five minutes

After `install.packages("languageserver")` and editor setup, open `center.R` in a folder and enter this example. All four actions work from the source text before the reader runs the script.

```r
center <- function(values) values - mean(values)
center(c(2, 4, 6))
```

1. While entering the second line, type `cen` and request completion. Choose `center`.
2. Just inside `center(`, request **Signature Help**. The displayed parameter is `values`.
3. Finish the call and invoke **Go to Definition** on the second-line `center`. The editor takes you to the function on the first line.
4. Invoke **Rename Symbol** on the `values` parameter and rename it to `x`. The parameter and both uses in the body change together.

The release-tagged 0.3.22 server returned these results in a direct LSP check. Editor setup and command discovery are part of the launch checks below.

Use this pattern for feature pages. Give the reader the code, the cursor position, the editor action, and the expected result. For example, in the S3 demo, Signature Help offers `format` for `present(frame, …)` and `scientific` for `present(values, …)`. Label any client-specific steps beside the relevant action. [S3 example in the released package](https://github.com/REditorSupport/languageserver/blob/v0.3.22/inst/examples/s3-dispatch.R) · [Refactoring examples](https://github.com/REditorSupport/languageserver/blob/v0.3.22/inst/examples/lsp-3-18.R)

## Keeping examples accurate

- The quick start should work with the current CRAN release. On 28 November 2026 that is 0.3.22. Completion inside `glue()` and Go to Definition for configured, closed project helpers are examples for a **Development** section, with the r-universe installation instructions beside them. Promote each example when its change ships on CRAN. Check the release tag and `NEWS.md` because master can report the same package version as the release. [CRAN package](https://cran.r-project.org/package=languageserver) · [Development notes](https://github.com/REditorSupport/languageserver/blob/master/NEWS.md) · [Installation instructions](https://github.com/REditorSupport/languageserver#installation)
- When a development example is missing, have the reader run `packageDescription("languageserver")[c("Version", "Repository", "RemoteSha")]` in the R installation their editor uses. The repository and commit, when recorded, help identify which build is installed.
- Use the README and bundled examples as the starting inventory. Check settings against `R/settings.R`, advertised features against `R/capabilities.R`, and expected behavior against the corresponding provider tests. For example, the README lists a document-link size limit of 16,384 bytes, while the implementation sets 16 MiB. Resolve discrepancies during migration. Keep each setting’s default in one reference entry and link to it from the guides. [Settings](https://github.com/REditorSupport/languageserver/blob/master/R/settings.R) · [Capabilities](https://github.com/REditorSupport/languageserver/blob/master/R/capabilities.R)
- Cover all released and development providers at launch, including R6, reference-class, S4, and S7 members, call and type hierarchies, and data-frame columns in data-masked code. Tie each example to a released version or mark it **Development**. Add or revise the relevant example in the PR that changes user-visible behavior.

## Delivery and publication

1. **Build the site and the first walkthrough.** Put Markdown and VitePress configuration under `website/docs/`, pin the site toolchain and commit its pnpm lockfile. Set the Pages base to `/languageserver/`. Exclude `^website$` in `.Rbuildignore` and check that `R CMD build` omits the directory. Add a dedicated Pages workflow that builds changes to the site or workflow on pull requests with read-only permissions.
2. **Complete the launch guides.** Try the quick start in VS Code and Neovim using the CRAN release. Build with a frozen lockfile and check internal links and fragments. Serve the production build under `/languageserver/` and test a direct visit to a nested page, its assets, mobile navigation, and a local search for “rename” that opens the intended section.
3. **Publish and make the site discoverable.** The repository has GitHub Pages enabled with GitHub Actions as its source. Upload the static site as a Pages artifact and deploy from `master` on relevant pushes or manual dispatch. Give only the deploy job `pages: write` and `id-token: write`, using the `github-pages` environment. Link the guides from the README and `DESCRIPTION`, retain existing README section anchors, and set the repository homepage after checking the live quick start and search. [Reference Pages workflow](https://github.com/REditorSupport/vscode-R/blob/c5e0ba3bbabdecff7dd79eb442a033f8653e9910/.github/workflows/pages.yml)
