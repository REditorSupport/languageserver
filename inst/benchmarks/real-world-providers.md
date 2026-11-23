# Real-world provider audit

The audit compared the server at `ee27ece` with the changes in this branch on R 4.6.1, macOS arm64. The November 23, 2026 snapshot covers the top 100 packages by monthly downloads on r-packages.io. The CRAN release, rank, download count and archive checksum of each package are in the [source manifest](ranked-packages.csv). [Ranking](https://r-packages.io/top-packages) · [Ranking data](https://r-packages.io/api/top?n=100)

## Ranked package sources

The 100 verified source archives contained 2,687 implementation files and 509,136 physical lines under `R/`. BH ships headers and contributed zero R implementation files. An independent walk of base R's parsed expressions counted distinct literal assignments per file, including chained assignments, quoted targets and standalone brace blocks.

| Check | Baseline | Branch |
| --- | ---: | ---: |
| Expected bindings available when the file is closed | 19,116 / 19,286 | 19,286 / 19,286 |
| Files missing an expected binding in the closed-file index | 47 | 0 |
| Source files with parser errors | 0 | 0 |
| Lexical definition selected the exact formal in the full-source sweep | 5,212 / 5,648 | 5,648 / 5,648 |
| Go to Definition selected the exact local formal over stdio | 46 / 53 | 53 / 53 |
| References included the same declaration | 49 / 53 | 53 / 53 |
| Rename returned an edit at the same declaration | 46 / 53 | 53 / 53 |
| Internal errors preparing type hierarchy in the package sample | 3 | 0 |

The 170 missing declarations affected 21 packages. Examples include `memoise <- memoize <- function(...)`, `RcppCapabilities`, and ggplot2's quoted `%+%` operator. In xfun's `serve_dir = function(dir, ...)`, navigating from the argument of `normalizePath(dir)` opened `base::dir`. The enclosing-scope fix also restored local navigation in knitr, mime and highr. Type hierarchy failed on package hook names such as magrittr's `.onLoad`. Re-enabling S3 dispatch also returned a generic signature too early in datawizard. The first request now waits for the method metadata and includes `select` and `exclude`.

All files went through the full document parser, hierarchical outline, folding, semantic tokens, document colors and inline values. The default diagnostics backend completed on all 99 selected package files and all 764 example documents. The sweep checked 1,368,552 semantic tokens and validated token and fold positions against the source. A lexical navigation probe chose up to three distinct functions per file, located a body use by syntax ancestry, and checked its exact formal. It excluded functions that reassign the name. A separate stdio client opened one file in each of the 99 package workspaces. It preferred a file near 100 lines containing a named function. It negotiated the advertised providers, sent position, document and workspace requests, and exercised resolve, hierarchy and generated-content requests using their preceding responses. The branch completed 3,780 stdio requests across these packages without a protocol error. Eleven representative site documents supplied another 418 requests, including positive checks for extract refactoring, type definition, inheritance and implementation. Three prepare-rename requests on non-renamable sites were declined. Fifty-three files supplied a suitable formal and body use for the exact-position checks. Empty results are valid for many other requests.

## Documentation examples

The site runs collected parseable example input and discarded printed output. Examples on the same page shared a buffer so later requests could use earlier declarations. Some examples deliberately ask for a private field, use invalid runtime input, or redefine a class. A matching completion label, a nonempty hover and a verified declaration are different checks. [R6](https://r6.r-lib.org/) · [S7](https://rconsortium.github.io/S7/) · [R Polars](https://pola-rs.github.io/r-polars/)

- **R6 and S7.** The corpus contains 264 blocks from 54 pages, 2,678 lines and 544 member accesses. The per-page sweep exercised document, position, workspace, formatting and diagnostics providers. Workspace symbol search raised seven exceptions on the baseline when an open document had no bindings. S7 validator guards such as `if (length(self@start) != 1)` now yield the property hover and navigate to the declaration. At the 413 R6 member sites, completion hits rose from 366 to 387, hovers from 292 to 333, and definitions from 236 to 246. Signature hits rose from 172 to 196 across 211 calls. S7 hovers rose from 76 to 95 and definitions from 78 to 97 across 131 member sites. On the S7 homepage, Go to Implementation on the call to `inside()` selects the actual `method(inside, Range)` registration.
- **Polars.** The corpus contains 1,510 blocks from 710 pages, 9,042 lines, 5,486 member accesses and 4,893 method calls. The namespace model used Polars 2.1.0.9000 at `9ae9634` to match the website. Go to Definition returned a location at 4,859 accesses, up from 8. At calls, coverage rose from 2 to 4,853 of 4,893. Source-less installed methods can open a generated function body. Named completion items matched at 5,459 accesses, hover returned content at 5,446 and signatures matched at 4,877 calls. Inlay hints completed on all 710 pages, including eight that previously raised an exception. Outgoing call hierarchy completed for generated items whose documents had never been opened.

## Latency

These are local elapsed medians except where identified as CPU time. Startup includes process launch. The R6 startup workload alternated baseline and branch across three cold and three cached launches per revision. The shallow-index workload alternated five runs per revision over 195 files spanning 80,168 lines, taking each package's median-size and largest source file. The full parser timed three alternating before/after pairs on those files. The ggplot2 workspace-symbol query used the same 206 source files and checked that every repeat returned the same two matches.

| Workload | Baseline | Branch |
| --- | ---: | ---: |
| Initialize the R6 workspace, cold / cached | 3.69 / 1.13 s | 3.58 / 1.04 s |
| R6 startup through the end of indexing, cold / cached | 5.07 / 2.46 s | 4.74 / 2.38 s |
| Shallow index, 195 files, CPU / elapsed | 1.85 / 2.14 s | 1.80 / 2.08 s |
| Full parse, same 195 files, CPU / elapsed | 4.92 / 4.94 s | 5.01 / 5.03 s |
| Repeated ggplot2 workspace symbol query | 11.7 ms | 0.2 ms |
| Build the Polars namespace model, two independent preparations | 7.13 s | 7.00 s |
| Warm public completion in `pl$concat(s, s2, rechunk = FALSE)$n_chunks()`, CPU | 342 ms | 163 ms |
| Polars model, serialized bytes | 5,693,521 | 5,739,480 |

Cold cross-file References for the `allow_null` parameter in ggplot2 returned four locations in 14.52 seconds on the baseline and 14.37 seconds on the branch in an isolated repeat (three fresh stdio sessions per revision). Under concurrent test load the medians were 16.47 and 17.11 seconds. A source function's formal may have named callers throughout the workspace. Some Polars chains also remain slow, including `pl$concat(s, s2, rechunk = FALSE)$n_chunks()`. On a cold wire probe for attached `pl$scan_csv`, the first F12 returned empty in 49 ms. While metadata was pending, outline and semantic tokens took 1 and 2 ms. A retry found the generated body 10.73 seconds after process launch and the next request took 7 ms. Source-less fields and dtype constants often have no definition target. String-literal assignment targets participate in indexing and search. Rename declines edits that would update callers without their quoted declaration.

### Source-preserving R6 cache check

A coverage-instrumented installation exposed a cache regression in the initial patch. Synthetic R6 signatures retained a reference to the server implementation's source file. Serializing an instance then took 57,888,042 bytes, exceeding the 4 MiB receiver cache. Dropping that source reference reduced the instance to 4,735 bytes (4,978 bytes for the cache entry) and a generator to 12,791 bytes. Seven repeated member lookups took a median of 380 ms on the initial patch and at most 1 ms each after the fix. The regression test checks cache admission and navigation to the user's initializer and method.

## Replaying the source checks

Unpack the listed package versions into a directory containing one subdirectory per package. The current CRAN release lives under `src/contrib/<package>_<version>.tar.gz` and superseded releases under `src/contrib/Archive/<package>/`. Verify each downloaded archive against the manifest before comparing revisions. Install both server revisions in separate R libraries and run the same scripts and source tree for both.

```sh
R_LIBS=/path/to/base Rscript --vanilla inst/benchmarks/ranked-packages.R /path/to/sources base.csv
R_LIBS=/path/to/branch Rscript --vanilla inst/benchmarks/ranked-packages.R /path/to/sources branch.csv
R_LIBS=/path/to/base Rscript --vanilla inst/benchmarks/workspace-symbols.R /path/to/sources/ggplot2 geom_point
R_LIBS=/path/to/branch Rscript --vanilla inst/benchmarks/workspace-symbols.R /path/to/sources/ggplot2 geom_point
```

The CSV reports expected and missing names per file, shallow indexing time and full parsing time. Timing excludes download and extraction. The regression tests contain smaller examples with exact expected locations, method names, signatures and edits.
