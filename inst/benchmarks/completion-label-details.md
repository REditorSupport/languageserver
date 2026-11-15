# Completion label-details benchmark

Run against an installed package, or pass a checkout with its native library built:

```sh
Rscript --vanilla inst/benchmarks/completion-label-details.R results.csv 20 . fixed
```

The harness checks that every request returns 200 candidates. The package case
uses 200 real base-package closures, with namespace preparation outside the timer.
“Cold” clears only the signature metadata cache before each request; “warm” runs
an untimed request first. Both leave the namespace loaded.

The rapid-edit case contains 200 local functions in a 250- or 10,000-line buffer.
Each request changes the final prefix and document version. Timing starts at the
lightweight completion provider; if it cannot respond, the harness includes a
full parse and ordinary completion before stopping the timer. Thus a missing
early response is not counted as a fast completion. Editor rendering, IPC,
background scheduling and parse debounce are not measured.

Measured on 2026-11-15 with R 4.6.1, aarch64-apple-darwin23, Darwin 27.2.0 arm64.
There are 20 measurements per row, comparing published PR revision `0720a4f`
with the review fixes in `d07a31a`. Times below are median / 95th percentile / maximum in ms;
first-call and allocation variation remain in the recorded samples.

| Workload | Inline | Cache | Before | After |
| --- | --- | --- | --- | --- |
| 200 package functions | off | cold | 2 / 9.8 / 100 | 2 / 9.9 / 103 |
| 200 package functions | off | warm | 2 / 2.1 / 4 | 2 / 3.1 / 4 |
| 200 package functions | on | cold | 15 / 22.8 / 37 | 15 / 26.6 / 37 |
| 200 package functions | on | warm | 9 / 11 / 11 | 10 / 12.1 / 13 |
| Rapid edits, 250 lines | off | edited | 7 / 31 / 87 | 7 / 29.8 / 101 |
| Rapid edits, 250 lines | on | edited | 74 / 306.2 / 594 | 31 / 33.1 / 54 |
| Rapid edits, 10,000 lines | off | edited | 131 / 140.8 / 193 | 133 / 146.1 / 185 |
| Rapid edits, 10,000 lines | on | edited | 947.5 / 1051.8 / 1067 | 226 / 256 / 275 |

With inline details enabled, all 200 signatures appeared in both revisions.
Those rapid-edit requests all fell back to a full parse before the fixes and
all returned through the lightweight provider afterward. The opt-in
headers and previews still add work, especially in larger buffers; these are
local measurements, not cross-platform latency guarantees.
