# Provider performance

The performance work targets both time spent preparing a document and the
latency of interactive requests. The baseline is commit `9618b5b`. Measurements
below were taken locally with R 4.6.1 on macOS arm64; they are reproducible
workloads, not timing assertions in the test suite.

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

## Reproduce

Install the baseline and updated revisions into separate libraries, and run
the same scripts against each. All scripts generate their own inputs and use
only dependencies already required by this package.

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

Regression coverage checks old/new result equivalence, Unicode and UTF-16,
missing arguments, nested/overlapping scopes, cursor boundaries, literate and
incomplete documents, serialization, edits, live settings/files, namespace
replacement, and bounded worker cancellation. Benchmarks deliberately avoid
fragile elapsed-time thresholds in automated tests.
