# Provider performance

The [real-world provider audit](real-world-providers.md) covers the top 100
ranked CRAN packages and example code from the R6, S7 and Polars websites.

The performance work targets both time spent preparing a document and the
latency of interactive requests. The baseline is commit `9618b5b`. Measurements
below were taken locally with R 4.6.1 on macOS arm64; they are reproducible
workloads, not timing assertions in the test suite.

## Repository performance review

The table compares performance commit `622fe20` with `bcdff9e` on macOS arm64,
R 4.6.1. The corpus comprises all 89 R source files from that commit.
Both installed revisions read the same inputs. The checks compare semantic
tokens, definition names, referenced packages and member-analysis flags.
The provider benchmarks validate returned methods, completion items, actions
and edits. Times below are local elapsed medians.

| Workload | Before | After |
| --- | ---: | ---: |
| Parse the package's R sources, 41,261 lines | 5.11 s | 3.71 s |
| Parse a 4,000-line file of ordinary functions | 371 ms | 311 ms |
| Fix All, 1,000 diagnostics on separate lines | 3,248 ms | 70 ms |
| Complete a field after 1,000 calls with distinct inputs | 1,150 ms | 19 ms |
| Resolve an S3 call at a new position in a 1,000-file package | 131 ms | 17 ms |
| Extract two statements in 4,000 lines, first / repeat | 183 / 136 ms | 35 / 4 ms |
| Change ordinary prose in a 5,100-line Quarto file | 129 ms | 0.1 ms |
| Edit a closing fence in the same file | 140 ms | 14 ms |
| Classify semantic tokens, 2,000 assignments plus 2,000 embedded SQL lines | 77 ms | 3 ms |

Parsing spends a substantial share of its CPU time discovering members and
bindings. In baseline profiles of this package's `R/completion.R` and
`R/index.R`, member indexing accounted for 55–68% of worker time. The
worker now uses native assignment discovery and skips walking expressions
whose syntax rules out the package or member operations it is looking for.
The corpus timer includes classification checksums and excludes file I/O.
An earlier five-sample run took 4.31 s before and 3.67 s after, giving a
15–27% reduction in elapsed time across the two comparisons.

Fix All compares edits within groups of potentially overlapping ranges. The first extraction
still constructs XML on demand. Subsequent refactorings can narrow selection
and binding lookups to candidate positions. S3 resolution hashes peer files
for sources actually supplying bindings. Member completion follows each
required statement's dependencies once. The Quarto region model survives
ordinary same-line edits, and a structural edit examines candidate fence and
div markers across the document.

Follow-up measurements against `622fe20` cover two more editor requests.
Fix All for 600 disjoint assignments on one physical line fell from 1,449
to 49 ms after partitioning edits by both line and UTF-16 column. Editing a
Markdown heading in the same Quarto fixture fell from 12.5 to 0.1 ms after
narrowing the region check to structural markers. Both are three-sample medians.
Classifying three requested lines from a 250,000-line multiline literal also
reduced R allocations from 8.59 MiB to 2.54 KiB by expanding just those lines.

Batched multiline tokens reduced total R allocations for the SQL classification
from 273 to 5.4 MiB. For 2,000 functions with distinct parameter names, storing
only the scope/name pairs that exist reduced parameter-classification
allocations from 54 to 8.2 MiB. Worker parsing, synchronous document updates
and provider execution are measured separately. Provider times exclude editor
rendering, IPC and preparation of the accepted parse.

The baseline's warm 4,000-line provider workload measured roughly 0.5 ms for
definition, 1 ms for hover, 1.8 ms for completion and 2.4 ms for signature
help. In the end-to-end typing benchmark with 5,000 lines and workspace
indexing, 16 completions took a median of 15.1 ms before and 15.2 ms after.
The initial document-symbol response fell from 1.53 s to 1.27 s. Both runs
returned the same number of bytes and completed all six background requests.
On a cold, 500-file workspace with 50,000 lines, 30 lightweight probes on
the branch had a 0.5 ms median and a 71 ms maximum during discovery.

One uncached Polars 2.0.0 model preparation took 6.4 s.
A separate replay of static return inference took
4.5 s. On the 259-line `R/navigation.R`, default lintr took 301 ms on
first submission and styler took 995 ms. Formatting the resulting styled
buffer took 5 ms. These backend timings exclude process launch and queuing.

The main remaining interactive cost measured here was finding references to
methods across intervening calls. On the branch, 32 consumer files produced
1,025 verified locations. First References and Rename took 2.27 and 2.41 s,
respectively, and repeats took 23 and 26 ms. In a 12-consumer profile, resolving
individual receivers accounted for 89% of cold CPU samples, including 22%
of all samples spent building the member context. A follow-up experiment
could reuse package and import setup across sites in one document while still
checking receiver identity on each side of an intervening call.

Run these scripts from the branch against each installed revision. For the
corpus, point both runs at the same baseline source directory.

```sh
Rscript --vanilla inst/benchmarks/parse-corpus.R /path/to/baseline/R 5
Rscript --vanilla inst/benchmarks/code-actions.R actions.csv 1000 3
Rscript --vanilla inst/benchmarks/scoped-inference.R inference.csv 1000 1000 7
Rscript --vanilla inst/benchmarks/literate-edits.R literate.csv 100 10 5
Rscript --vanilla inst/benchmarks/semantic-shapes.R 2000 3
```

Set `R_LIBS` to the intended installation before each run. For end-to-end
transport and for additional cold and warm workloads, see the benchmark
instructions below.

## Completion label details

See the [completion label-details benchmark](completion-label-details.md) for
200-candidate signature lookup and rapid-edit completion workloads.

## Usage review workloads

The usage review adds four workloads that check their results before timing
the affected operation. Run installed revisions serially in separate libraries:

```sh
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/default-values.R defaults.csv 10000 3
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/rename-edits.R rename.csv 10000 3
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/cli-diagnostics.R diagnostics.csv 500 5000 3
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/text-changes.R changes.csv 500 10000 3
```

Default extraction converts the language argument chain once and collects
literals before flattening. Rename groups references once by URI before
building edits. CLI diagnostics reuse one index of the submitted UTF-8 bytes;
empty diagnostic batches skip indexing. Text changes invalidate pending disk
snapshots once before their background parse.

The usage-review baseline is `cf116dd`. Three local passes with R 4.6.1 on
macOS arm64 produced these medians:

| Operation | Before (ms) | After (ms) |
| --- | ---: | ---: |
| Extract 10,000 literal defaults | 850 | 1 |
| Complete 2,000 defaults | 88 | 41 |
| Construct 10,000 rename edits | 213 | 31 |
| Convert 500 diagnostics across 5,000 lines | 646 | 15 |
| Incremental edit with 10,000 pending disk files | 1.904 | 1.578 |

Recorded R allocations fell from 1.20 GB to 520 KB for default extraction and
from 400.52 MB to 612 KB for rename construction. These are total allocations
within each operation, not retained memory or server startup costs.

These scripts isolate extraction, edit construction, diagnostic range
conversion and notification handling respectively. They exclude worker
execution, reference resolution, document parsing, IPC and editor rendering.
The text-change workload also excludes parse task creation. Default and rename
scripts report R allocations when memory profiling is supported. Local timings
vary with hardware and load; regression tests assert behavior without elapsed
thresholds.

## Member references and rename

Run the same script against the installed PR base, the reviewed revision and
the updated package. The CSV records every sample and the expected and
returned counts. Each result must match the expected URI, UTF-16 start and end
positions. Rename also checks the replacement text and rejects extra edits.

```sh
R_LIBS=/path/to/base/lib Rscript --vanilla inst/benchmarks/member-navigation.R base.csv 128 32 3 6f42454 legacy all
R_LIBS=/path/to/reviewed/lib Rscript --vanilla inst/benchmarks/member-navigation.R reviewed.csv 128 32 3 56dd618 owner all
R_LIBS=/path/to/updated/lib Rscript --vanilla inst/benchmarks/member-navigation.R updated.csv 128 32 3 updated owner all
R_LIBS=/path/to/updated/lib Rscript --vanilla inst/benchmarks/member-navigation.R method.csv 32 32 3 updated owner method
```

The first three commands use a shared declaration and 128 consumer files.
Each consumer reads the shared member 32 times, an unrelated same-named member
32 times, and a same-named lexical variable 32 times. Each request returns the
declaration and 4,096 uses. Direct and chained accesses use separate fixtures.
The method case uses 32 consumer files and returns 1,025 locations. It puts an
opaque call between pairs of method calls and gives the unrelated owner a
field with the proposed replacement name.

Median elapsed milliseconds over three samples, measured serially on macOS
arm64 with R 4.6.1. A first request starts with an indexed workspace, accepted
parse results and fresh provider caches. A repeat is the next identical request.
The timer includes edit construction for Rename. It excludes initial file
discovery, parsing, transport, serialization of the response and editor display.

| Operation | Reviewed first | Updated first | Reviewed repeat | Updated repeat |
|---|---:|---:|---:|---:|
| Direct member References | 6,465 | 712 | 6,006 | 170 |
| Direct member Rename | 9,078 | 721 | 9,017 | 178 |
| Chained member References | 7,565 | 852 | 6,737 | 165 |
| Chained member Rename | 10,162 | 842 | 10,046 | 180 |
| Method References with opaque calls | 1,627 | 1,676 | 1,483 | 18 |
| Method Rename with opaque calls | 2,218 | 1,675 | 2,051 | 20 |

On the same 129-file workspace, first/repeat lexical References took 52/32 ms
on the base and 50/31 ms after the update. Lexical Rename took 69/42 ms and
64/43 ms respectively. The base returned zero results for the member requests.

First-time calls separated by opaque code still require individual receiver
resolution. Subsequent requests can reuse a verified source location for that
exact syntax site while its document, workspace and package metadata are
current. Unresolved sites are retried. Direct reads in an unchanged block can
share receiver resolution within the first request. Large result sets still
require one edit or location per matching use.

## S7 release providers

Install the base and candidate languageserver revisions in separate libraries.
Use the same R and S7 installations for both. Run the benchmark from the
candidate checkout so both revisions receive the same fixtures.

```sh
R_LIBS=/path/to/base:/path/to/S7-1.0.0 Rscript --vanilla inst/benchmarks/s7-providers.R baseline 15
R_LIBS=/path/to/candidate:/path/to/S7-1.0.0 Rscript --vanilla inst/benchmarks/s7-providers.R candidate 15
```

The following run used R 4.6.1 on arm64 macOS with S7 1.0.0. Figures are
median / p90 wall times in milliseconds for 15 samples per revision, with
five samples for the namespace index. Cold completion starts with an accepted
parse and clears the receiver cache. Warm batches reuse that document. Each
sample checks its result.

| Workload | Base `7ad3e4e` (ms) | Candidate (ms) |
| --- | ---: | ---: |
| Build S7 namespace member index | 98.21 / 104.72 | 105.70 / 107.95 |
| Parse 200 functions plus S7 declarations | 39.76 / 41.63 | 41.14 / 43.40 |
| Cold ordinary list completion | 0.63 / 0.67 | 0.68 / 0.80 |
| 10 warm ordinary list completions | 4.92 / 5.09 | 4.93 / 5.13 |
| Cold attached S7 property completion | 1.56 / 1.61 | 1.64 / 1.80 |
| 10 warm attached S7 completions | 9.18 / 10.83 | 9.87 / 11.49 |
| Cold property completion from a package import | 0.52 / 0.55 (empty) | 1.21 / 1.30 (2 properties) |
| Cold property completion on a method parameter | 0.96 / 1.01 (empty) | 1.76 / 1.94 (2 properties) |
| Index one file declaring a class, generic and method | 2.71 / 3.42 | 3.71 / 4.63 |
| Index 8 S7 files and 120 ordinary package files | 278.34 / 308.46 | 273.97 / 292.21 |
| 10 ordinary workspace symbol searches | 0.73 / 0.91 | 0.78 / 0.79 |
| 10 S7 workspace symbol searches | 0.71 / 0.86 (0 results) | 1.44 / 1.48 (16 per query) |

The individual S7 file took an additional 1.00 ms to index at the median.
Run-to-run variation in the mixed-file index exceeded the difference between
revisions. Earlier paired runs on this machine measured 262–276 ms on the
base, and 267–289 ms on the candidate.

For the oversized-namespace case, install ggplot2 and replace `15` with
`ggplot2` in each command. Each invocation starts a fresh R process and
disables the disk cache. On ggplot2 4.0.4, five alternating samples per
revision gave median worker preparation times of 1.743 s and 1.747 s (p90
1.776 s and 1.767 s). The base snapshot held 11 datasets in 253,424 bytes.
The candidate also retained 31 exported S7 values, using about 12.5 MB of
the 32 MiB snapshot budget. The benchmark checks that completion on
`ggplot2::class_ggplot()@` returns the 12 property names from the installed
`S7::prop_names()` API.

### S7 documentation-example performance (PR #879)

Use separate installed libraries for base `6c66f3b`, original PR `973071d`
and the performance follow-up. Run the same benchmark script from the
follow-up checkout for all three. It requires S7 and data.table.

```sh
R_LIBS=/path/to/base/lib Rscript --vanilla inst/benchmarks/s7-provider-latency.R base 9
R_LIBS=/path/to/pr/lib Rscript --vanilla inst/benchmarks/s7-provider-latency.R pr 9
R_LIBS=/path/to/fixed/lib Rscript --vanilla inst/benchmarks/s7-provider-latency.R fixed 9
```

These are median wall times in milliseconds on arm64 macOS, R 4.6.1,
S7 1.0.0 and data.table 1.18.6.1. Each revision ran in four fresh R
processes, interleaved with the other revisions. The table reports the
median of the four process medians. Each timed request checks its result.
Parsing, worker startup, and editor transport are outside the timed requests.

| Workload | Base | Original PR | Follow-up |
| --- | ---: | ---: | ---: |
| 150 data.table `:=` calls to `round`, inlay hints | 3.1 | 258.3 | 3.3 |
| Same hints with S7 attached | 3.3 | 324.9 | 3.8 |
| Mixed file, exported `fifelse` | 2.0 | 150.7 | 2.9 |
| Mixed file, qualified `data.table::shift` | 2.8 | 359.9 | 4.0 |
| Mixed file, a source-defined helper | 2.4 | 226.2 | 3.5 |
| S7 signature help inside a 340-statement function | 1.7 | 29.6 | 1.7 |
| First signature lookup across 96 method registrations | 1.8 | 26.1 | 24.3 |
| Signature help after an edit to the call in that file | 2.1 | 27.0 | 2.1 |
| Build navigation index for 1,200 quoted property reads | 41.6 | 64.5 | 46.2 |

All inlay viewports contain 150 statements. The `fifelse` and source-defined
helper cases reach the server's limit of 200 hints. Verified ordinary
functions can reuse their parameter names. Shadowed and method-dependent
calls still resolve in their lexical context. Tooltip resolution retrieves
contextual documentation for the requested hint.

The base returns the generic signature. Both PR versions return the selected
method's extra parameter. The first lookup still resolves the registration
history. Later edits to the call can reuse verified dispatch keys; changes
to the declarations or their metadata invalidate that result. Package peers
and sourced files can also invalidate the cache. Property-index allocations
recorded by `Rprofmem()` were 1.21, 45.62 and 1.22 MiB respectively.
Doubling the read count to 2,400 increased those figures to 2.41, 179.13
and 2.44 MiB.

As controls, the 128-file mixed workspace index took 269, 269 and 268 ms.
Parsing the base checkout's 90 R source files (42,482 lines) took 2.72,
2.90 and 2.75 seconds with identical result hashes from `parse-corpus.R`.
The expanded ggplot2 member snapshot retained 2.50 MB serialized in the
follow-up versus 2.42 MB on the base in a worker that also loaded data.table,
dplyr and shiny. The additional dispatch metadata is still present after
the lookup optimizations.

## Member metadata workers

Run these against separately installed revisions, without concurrent tests or
benchmarks:

```sh
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-preparation.R stages.csv 3 polars
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-tasks.R workers.csv 3 polars
```

The stage workload separates namespace loading, live identity validation,
registry extraction, model/return preparation, freezing and disk operations.
The worker workload uses a fresh process with startup profiles disabled for
every request, as production member tasks do. Cold rows have empty caches;
disk rows restore existing models; known rows validate identities already held
by the workspace and return no duplicate models. Worker timings include server
loading, namespace preparation and metadata transfer, but exclude scheduling,
document parsing and editor rendering. All validation uses neutral fixtures and
static inference; no data query or user document expression is executed.

Three serial local passes on R 4.6.1/macOS arm64 with Polars 2.0.0, comparing
`bb8eb83` with the member-preparation changes, produced these medians:

| Fresh worker workload | Before (ms) | After (ms) |
| --- | ---: | ---: |
| Cold member preparation | 10,529 | 7,156 |
| Existing disk model | 3,771 | 1,763 |
| Already-known model validation | 3,564 | 1,512 |

An isolated paired registry-extraction comparison also fell from 2,478 ms to
692 ms with identical complete input. Fresh live-identity validation fell from
approximately 2,440 ms to 437 ms with identical contexts and identity records.
Much of the validation cost came from materializing every cache key on each
eviction. The direct oldest-item path avoids that scan for unprotected entries;
protected entries and older collection APIs retain their previous behavior.
Memory budgets and live namespace/file checks are unchanged.

Repeated saves retain the same queued or running member task when its complete
request context is unchanged. Saves received during running work trigger one
fresh validation after that work completes. New package/library/workspace
contexts replace it immediately. These lifecycle checks use inert sessions and
assert task identity, callback safety, and fresh metadata capture without timing
thresholds.

Debug task lines now include `queued:` and `running:` alongside total time.
The running interval begins at dispatch and includes process startup, result
transfer and callbacks; it is not a measurement of worker CPU time alone.

## Member metadata cache

Run `R_LIBS=/path/to/library Rscript inst/benchmarks/member-metadata.R results.csv 10`
against an installed package with Polars and R6 available. An optional third
argument selects an R document. The benchmark prepares Polars, methods and R6
metadata in a clean worker, then measures consecutive completion, signature and
hover requests at each standalone `q`, `q1` and `q2` expression. It validates
the provider results and records decoded cache hits and restores. Preparation,
document parsing and explicit garbage collection are outside the timer;
context selection, restoration and inference are inside it.

Measured locally with R 4.6.1, Polars 1.16.0 and R6 2.6.1 on macOS arm64,
using the affected document's three query positions, ten passes and 90 requests:

| Provider | Before median / p95 (ms) | After median / p95 (ms) | Before / after restores |
| --- | ---: | ---: | ---: |
| Completion | 137 / 183.3 | 92 / 137 | 90 / 1 |
| Signature help | 128 / 172 | 86 / 128 | 90 / 0 |
| Hover | 129 / 166 | 86 / 128 | 90 / 0 |

The baseline is `6365c20`. Polars and methods decoded indexes together exceed
the 32 MB budget. The baseline restores all three packages on every request;
catalog selection restores Polars once, then uses decoded hits. Catalogs share
the existing snapshot budget, and full namespace indexes load on demand for
cross-package inference. These timings include the first restore and remain
machine-dependent; the regression tests assert cache behavior without timing
thresholds.

## S3 dispatch

Run `R_LIBS=/path/to/library Rscript inst/benchmarks/s3-dispatch.R results.csv 2000`.
An optional third argument adds comma-separated installed packages, for example
`datawizard,tibble`. The script checks the selected methods, measures cold
resolution and warm completion, hover and signature help in a large document,
and compares ordinary requests with S3 enabled and disabled. It also reports
workspace startup, metadata worker time and retained cache bytes. Five timing
batches capture timer noise; these are local measurements, without timing
assertions in the regression suite.

## Persistent package cache validation

Run `R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/package-cache.R results.csv 3 polars`.
The optional package argument permits the same comparison with other installed
packages. The script separates first namespace loading, uncached preparation,
cache creation, warm disk hits and loaded-namespace validation. It records CPU
and elapsed time and checks returned metadata; parsing and provider requests are
not included. Run revisions serially with isolated libraries on the same host.

Validation still inspects mutable namespace bindings and captured values.
Canonical function syntax and unchanged binding facts are reused within a
worker. Syntax memo keys are session-local, body/formals are checked on hits,
and namespace replacement or producer changes invalidate validation facts.
The worker memos are bounded at 64 MiB for syntax and 32 MiB for facts; neither
is a persistent cache of live functions or document state.

Serial local measurements on R 4.6.1/macOS arm64 with Polars 2.0.0 compared
`da70d5f` with the validation-memo changes (ms; three repeated passes and one cache-creation run):

| Operation | Before | After |
| --- | ---: | ---: |
| Uncached preparation | 6,830 | 6,744 |
| Cache creation | 7,224 | 6,778 |
| Warm disk hit | 373 | 211 |
| Loaded-namespace validation | 336 | 179 |

The initial loaded-namespace check remains necessary on every new worker.
Mutable bindings still need inspection; this improvement reuses canonical
syntax and derived facts rather than trusting only a package version or path.

## Adaptive inference budget

Run `R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/member-budget.R results.csv 3`.
Polars metadata is prepared once outside the timer. Each cold run receives the
same metadata and parsed source with fresh receiver, binding and invocation
caches; two repeated requests follow it. Only the budget policy changes: a fixed one-second
elapsed deadline versus the adaptive 250 ms initial allowance, capped at two
seconds of CPU and five seconds elapsed. Policy order alternates between passes.

Ordinary calls, long chains, wide argument lists, repeated conversions and
recursive code use neutral fixtures. Stalled cases inject 25 or 150 ms at every
inference clock checkpoint to model descheduling. They are controlled local
experiments, not measurements from a remote SSH host. No document expressions,
constructors or methods are executed. CPU/elapsed time, visited nodes, exhausted
budgets, timeout flags and retained results are recorded. Compare cold rows to
isolate the budget policy; warm rows show whether complete results can be reused.
An exhausted or timed-out traversal must not be reported as a fast success.
Completed binding prefixes can survive an exhausted request, allowing its retry
to finish without restarting the entire chain. A final warm request measures
reuse after that retry. Deadline checks occur at bounded traversal checkpoints;
elapsed results can include one checkpoint's overshoot and provider cleanup.

The same local setup, three passes and identical prepared metadata produced
these cold medians. Ordinary requests remained below the timer's 1 ms
resolution, and normal long chains took 685 ms with the fixed deadline versus
694 ms with the adaptive policy. With 25 ms stalls, the fixed policy stopped
around 1,016 ms on all three cold requests; adaptive analysis completed around
2,363 ms with about 879 ms of CPU work. Repeated
completed requests took approximately 1 ms. With 150 ms stalls, the first
adaptive request stopped around 5,181 ms at the wall limit, a retry reused
completed prefixes and finished around 4,428 ms, and the next request was below
1 ms. The fixed policy continued to time out around 1,070 ms on its third
request. These measurements show the deliberate cold-path tradeoff rather
than a lower latency for every request.

The 25,000-argument fixture stopped at 20,001 visits (including the exhausted
visit) in 213 ms, down from about 1,513 ms before exhausted argument work and
repeated list copying were removed. Depth-bound and recursive fixtures stopped
in roughly 4 and 14 ms. Regression tests separately cover CPU/wall limits,
explicit caller deadlines, cache invalidation and rejection of partial results.

## Design and implementation

1. **Remove repeated parse-tree scans.** Native parent/child indexes classify
   semantic function assignments and collect viewport candidates in linear
   time. Reference resolution uses a sorted scope sweep. Static source discovery
   walks the syntax in C without evaluating project code or deparsing each call.
2. **Index interactive lookups.** Binary token and line searches limit navigation
   and viewport requests to relevant data. Definitions and reference occurrences
   are indexed by name. A native interval query finds callers without comparing
   every call with every function definition.
3. **Reuse derived results.** Namespace symbol maps, function metadata, document
   symbols, folding ranges, and linked-editing ranges reuse cached results.
   Workspace scope caches track index revisions and document membership; scopes
   containing the same documents share an aggregate namespace. Native signature
   scanning handles strings, comments, nested expressions, and UTF-16 offsets.
   Local signatures parse the function header instead of its body.
4. **Keep editing responsive.** `didChange` leaves workspace indexing to the
   accepted background parse, which supplies definitions and source-call
   metadata without parsing the buffer again. Superseded parses suppress their
   callbacks immediately and have at most 100 ms to release a warm worker before
   it is retired. Further edits replace the pending task without extending that
   deadline. Diagnostics retain immediate cancellation.
5. **Preserve live state.** Parse-cache reuse checks the document URI because
   reference identities and flat symbol results contain it. Literate caches
   also check the original document content. File existence, inlay settings and
   function formals remain live; namespace/function replacement invalidates
   metadata. XML pointers stay in the main process.
6. **Include typing and transport costs.** The server drains a bounded batch of
   queued client messages before background callbacks. XML token indexing uses
   an explicit descendant axis to avoid quadratic node-set merging in flat
   scripts. Completion trims each candidate group before allocating metadata,
   and a native bracket-scan cache reuses unchanged preceding lines without
   depending on stale parse data. Native response serialization handles ordinary
   protocol values and the package's interface classes; unsupported values use
   the existing jsonlite encoder. Depth, byte-size, class, and attribute checks
   bound the native path, and UTF-8 output is checked against jsonlite.

## Example results

`providers.R` uses 1,000 four-line functions and a 40-line viewport. Warm requests
reuse the accepted parse; parsing itself is measured separately. The workspace
symbol workload uses 50,000 names across 500 documents. The hierarchy workload
uses 1,000 caller functions and 2,000 calls in 4,001 lines.

| Operation | Baseline (ms) | Updated (ms) |
| --- | ---: | ---: |
| Parse document | 1,286 | 172 |
| Token lookup near end | 24.640 | 0.002 |
| Definition, repeated local name | 68.850 | 15.000 |
| Hover, repeated local name | 71.350 | 15.150 |
| Signature help | 22.950 | 0.750 |
| Document symbols, warm | 50.000 | 0.043 |
| Folding ranges, warm | 61.400 | 0.044 |
| Inline values, viewport | 14.200 | 0.487 |
| Inlay hints, viewport | 13.250 | 0.325 |
| Completion | 1.150 | 0.850 |
| Semantic delta, 50,000 tokens | 21.900 | 0.078 |
| Workspace symbol aggregation, warm | 33.650 | 0.169 |
| Incoming call hierarchy | 4,832 | 30 |
| Outgoing call hierarchy | 62.400 | 0.650 |

The separate range benchmark measured semantic assignment classification at
1,174.5 ms before and 1.0 ms after. This was the largest shared parse bottleneck.
Constructing XML/navigation indexes in the main process still costs roughly
65 ms for the large fixture. Initial parsing, large responses, package startup,
uncached documentation, formatting and diagnostics can still take longer than
interactive lookups. These timings exclude IPC, JSON serialization and editor
rendering, and vary with hardware, document structure, and machine load.

## End-to-end typing

The warm provider benchmarks above do not capture first-keystroke latency in
long flat scripts. `typing.py` opens a generated script of assignments, waits
for its first document-symbol response, then queues document symbols, semantic
tokens, and folding ranges before typing eight progressively longer prefixes.
It sends an incremental edit and completion request together every 250 ms,
measuring until the completion response is decoded. A dedicated reader drains
server output throughout the pauses, as an editor would. Two rounds include a
fresh parse between typing sequences. Diagnostics are disabled.

These measurements compare the earlier performance-PR revision `7c63989` with
the typing fixes, using the same local R 4.6.1/macOS arm64 setup. Each completion
returns 200 items; all six background provider requests complete in each run.

| Workload | Before (ms) | Updated (ms) |
| --- | ---: | ---: |
| First completion, 5,000 lines, automatic workspace indexing | 1,348–1,390 | 17–31 |
| Later completions, same workload, median | 33.9 | 13.4 |
| First completion, 20,000 lines, workspace indexing disabled | 6,125–6,275 | 22–24 |
| Later completions, same workload, median | 42.1 | 14.0 |
| Slowest completion, 20,000-line run | 6,275 | 48.5 |

The 5,000-line run still had one later completion at 311 ms (previously 1,186 ms
at that point). This does not guarantee uniformly sub-50-ms responses. Initial
document preparation, including parsing and the first symbol response, took
1.8–2.0 seconds at 5,000 lines and 8.3–8.8 seconds at 20,000 lines. Main-thread
parse installation and other background work are not fully preemptible, and
editor rendering is outside the measurement.

Profiling also verified byte-identical serialization of actual 20,000-symbol
responses: flat symbols fell from 6,677 to 38.7 ms, and hierarchical symbols from
9,061 to 38.9 ms. Serializing 200 completion items fell from 19.9 to 0.18 ms.

## Reproduce

Install the baseline and updated revisions into separate libraries, and run
the same scripts against each. All scripts generate their own inputs. The R
scripts use package dependencies; `typing.py` additionally needs Python 3 with
its standard library.

```sh
R CMD INSTALL --library=/tmp/languageserver-before /path/to/baseline
R CMD INSTALL --library=/tmp/languageserver-after /path/to/updated
R_LIBS=/tmp/languageserver-before Rscript inst/benchmarks/providers.R /tmp/before.csv 1000
R_LIBS=/tmp/languageserver-after Rscript inst/benchmarks/providers.R /tmp/after.csv 1000
```

Create the library directories before installation. `providers.R` warms each
operation, calibrates fast workloads above the timer resolution, and reports
the median/minimum/maximum of five timing batches (three for parsing).
`range-providers.R` provides additional range-specific workloads; its command
line usage is documented at the top of the script.
`call-hierarchy.R /tmp/after.rds 1000 /tmp/before.rds` benchmarks incoming and
outgoing calls and checks complete response equality against a saved baseline.
Omit the third argument when recording that baseline.

Run the typing benchmark once per installed revision, without running other
benchmarks or tests concurrently:

```sh
python3 inst/benchmarks/typing.py /tmp/languageserver-after --lines 5000 --index auto --wait-parse --providers --rounds 2
python3 inst/benchmarks/typing.py /tmp/languageserver-after --lines 20000 --wait-parse --providers --rounds 2
```

It prints JSON lines for preparation, each completion, and transport counts.
Omit `--wait-parse` to type immediately after opening; `--open-delay`, `--pause`,
and `--rounds` control other typing scenarios. `--profile /tmp/typing.Rprof`
records an R sampling profile without enabling server debug logging.

### Incomplete expressions

Use the same scripts with each installed revision. Run them sequentially so
the timings do not include contention from another benchmark.

```sh
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/partial-parse.R results.csv 1000 9
python3 inst/benchmarks/typing.py /path/to/library --semantic-edits --lines 1000 --index auto --wait-parse --rounds 5
```

The first command measures worker parsing and warm viewport and outline
requests on valid and incomplete R and Quarto files. It compares the returned
definitions, symbols, and unchanged tokens against a valid control. Check
`actual_count` and `matches_reference` alongside the timing: some earlier
versions drop an entire document or cell on a parse error.

The second command measures real LSP responses while editing the last line.
The viewport includes that line and the preceding 40 lines. Each round uses
distinct text. With `--wait-parse`, `elapsed_ms`, `full_ms`, and `outline_ms`
measure time since the edit to the viewport, full tokens, and document symbols
respectively, requested in that order.

## Workspace startup

`workspace-loading.py` generates 3,000 R files with 1,000 lines each and sends
lightweight `completionItem/resolve` requests immediately after `initialized`,
then every 100 ms. It measures whether the event loop can handle requests while
loading the workspace. Use `--cached` to populate a valid persistent index
before startup, and `--package` to put the files in a package's `R/` directory.
Diagnostics are disabled; full package document parsing remains enabled.

```sh
python3 inst/benchmarks/workspace-loading.py /tmp/languageserver-after
python3 inst/benchmarks/workspace-loading.py /tmp/languageserver-after --cached
python3 inst/benchmarks/workspace-loading.py /tmp/languageserver-after --cached --package --requests 150
```

The benchmark prints each request's latency and a final summary. `--root`
reuses a specified fixture directory, `--keep-fixture` preserves a generated
directory, and `--files`, `--lines`, and `--symbols` control its contents.
Each file repeats 50 assignment names by default. The generated fixture also
contains a sampling profile and server stderr log.

Measured locally against `dc9c2307` with R 4.6.1 on macOS arm64, using 3 million
lines of code:

| Workspace | First request before / after (ms) | Maximum subsequent request after (ms) |
| --- | ---: | ---: |
| Scripts, persistent cache | 7,279 / 26 | 79 |
| Package, persistent cache | 18,445 / 22 | 99 |

An uncached script workspace also fell from 5,092 to 22 ms for the first
request. The cached runs used 100 probes for scripts and 150 for the package.
Timings vary with hardware, system load, file sizes, and index contents.

Discovery and cache decompression now run in the index worker. Results return
in small batches, which the server installs within the index time budget.
Package and sourced documents are read incrementally, and persistent catalog
updates are transferred in bounded batches. File generations, cache metadata,
source targets, and worker lifecycles prevent delayed discovery results from
overwriting edits or resurrecting deleted files. Installing an individual full
document parse can still cause shorter pauses.

Regression coverage checks old/new result equivalence, Unicode and UTF-16,
missing arguments, nested/overlapping scopes, cursor boundaries, literate and
incomplete documents, serialization, edits, live settings/files, namespace
replacement, and bounded worker cancellation. Benchmarks deliberately avoid
fragile elapsed-time thresholds in automated tests.

## Glue interpolation

Use the same scripts with baseline and candidate packages installed in separate
libraries. The parse benchmark includes ordinary R and 2% or 100% templates,
with both repeated and distinct expressions. It reports elapsed time, R
allocation, serialized parse size, and memory retained by the source map.

```sh
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-parse.R candidate 200,1000,3000 5 > parse.csv
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-providers.R references.csv 200,1000 3 references
R_LIBS=/path/to/library Rscript --vanilla inst/benchmarks/glue-providers.R rename.csv 200,1000 3 rename
python3 inst/benchmarks/typing.py /path/to/library --lines 1000 --rounds 4 --wait-parse --fresh-rounds --warm-request --pause .012
python3 inst/benchmarks/typing.py /path/to/library --lines 1000 --rounds 4 --wait-parse --fresh-rounds --warm-request --pause .012 --glue --glue-every 25
```

Set `--glue-every 1` for the dense typing fixture. Add `--burst` to send a new
edit and request before the last reply has arrived. For comparisons, record
returned completion items, reference locations or rename edits along with
latency. On the 1,000-call dense reference and rename fixture, master at
`d1d1e53` returns one location for the first query. With interpolation support
the same query returns 45, including the declaration.
