# Extended inference and coverage

The extended experiment substantially increases method-completion coverage
without executing document expressions, Polars examples, constructors, getters,
or native methods. It keeps the original engine and CSV audit as a reproducible
baseline. Production languageserver behavior is unchanged.

## Coverage on the same pinned corpus

| Input/engine | Articles | Reference examples | Article sites beyond roots | Reference sites beyond roots |
| --- | ---: | ---: | ---: | ---: |
| Initial prototype | 109 / 168 (64.9%) | 3,345 / 5,324 (62.8%) | 47 / 103 (45.6%) | 1,224 / 2,920 (41.9%) |
| Extended, package source only | 133 / 168 (79.2%) | 5,267 / 5,324 (98.9%) | 68 / 103 (66.0%) | 2,863 / 2,920 (98.0%) |
| Extended, plus installed `datasets` metadata | 167 / 168 (99.4%) | 5,324 / 5,324 (100.0%) | 102 / 103 (99.0%) | 2,920 / 2,920 (100.0%) |

The denominator remains identical: six canonical articles, all 713 Rd files
(706 with examples), and the same 5,598 actual `$` occurrences across the article,
reference and supplemental corpora. All article/reference blocks parse; two
intentionally incomplete development snippets still do not. There are zero
inference errors. No previously offered Polars-related site was lost. Repeated
fresh-process audit runs produce the same type/member availability records.

The optional data pass reads bounded entries from the trusted installed
`datasets` serialized database, which has no environment references. It extracts
class attributes and plain column storage types. It does not call `data()`, run
data scripts, invoke S3 accessors, or force the attached dataset promises. The
dataset names/types are derived from the installed database, not a table of
`mtcars`, `iris`, etc. This is optional package metadata acquisition outside the
request path; strict source-only results are reported separately.

The one remaining canonical-article miss is `flights$join(...)`: its input comes
from unavailable `nycflights13`. The two supplemental misses are intentionally
illustrative variables without established input shapes. All method names used
in the reference examples are offered, including the custom namespace example.
This means **member availability in this corpus**, not a proof of correct types,
runtime validity, arbitrary Polars code coverage, or editor cursor handling.

## What changed

`inference-v2.R` is a package-independent abstract interpreter using the existing
AST helpers. It adds:

- Actual argument shapes, exact/partial/positional formal matching and defaults;
  returned parameters, local closures and receiver-field propagation.
- Separate early-return and fall-through paths; joins of branch environments;
  literal conditions; `switch` arms; conservative handling of loop writes.
- Ordered S3 dispatch for known classes, `NextMethod`, operator dispatch, indexed
  member/list lookup, homogeneous element shapes, and bounded `lapply`/`Reduce`.
- Common members for known unions. An unknown alternative still prevents a
  definite result. Class-vector order remains separate from alternative types.
- Cache keys including parameter/receiver shapes. Captured closures, oversized
  literal inputs and deep shapes bypass caching. Recursion/depth/node cutoffs
  mark the traversal transient so affected results cannot poison shared caches.
- Lexical shadowing checks for builtins; unresolved imported roots remain
  unresolved, and explicit shadowing remains Unknown. Error handler return
  alternatives are included for `tryCatch` rather than ignored.

`polars-adapter-v2.R` derives more metadata from the current source:

- Exported environment roots, including selectors, and registry-valued properties
  such as `pl$api`; private package registries remain package lexical metadata.
- Dispatch branch member lists, exclusions and own-method precedence. Series
  delegated members retain the source method's formals, while their result comes
  from the actual delegation wrapper body and its captured raw Series receiver.
- Native method factories use the inner returned closure's signature/body.
  Result types follow native wrapper constructors; arguments are never passed
  to a native call.
- Constructors with literal class declarations, local method aliases, stored
  parameter fields and active-property descriptors. Namespace constructors copy
  the raw receiver from their input; Then/Selector namespace loops are analyzed.
- Datatype active-binding names from literal initialization loops and a
  guaranteed base-class suffix when subtypes are native dependent. Conditional
  subtype fields are not assumed for every DataType.
- S7 literal class/property descriptors for QueryOptFlags and PartitionBy.
- Literal registration effects derived from a registry setter and the
  constructors that consume that registry. The custom namespace factory is
  analyzed from its source, and the effect stays in the document context.

There is no individual `filter -> LazyFrame`, `sum -> Series`, datatype-name
list, or version-specific method-result table. The adapter still recognizes the
reviewed Polars/Savvy registry/dispatch/wrapping conventions. Those extraction
rules must be validated before accepting a different implementation.

## Runtime metadata uses the same engine

`runtime-inspection.R` now handles nested plain lists of registries, ordinary
closure bodies/formals, active-binding descriptors, cyclic environment IDs,
and bounded captured literal dependencies. A changed captured getter literal
changes its fingerprint. Arbitrary promises remain skipped.

`inspection_shape_index()` converts a snapshot into the same generic shape
model, so ordinary installed registry/factory bodies support chained inference
without a separate table. Active names can be offered with an Unknown value;
the generic reader does not invoke a getter to find that value. Specialized
extractors can use the stored getter syntax to derive a shape.

The runtime bridge is verified on a trusted generic fixture, not an installed
Polars namespace. r-polars is still unavailable in this environment. The
production acquisition path must handle installed lazy-load bindings, package
initialization policy and metadata generation as described in coverage.md.

## Verification and practical limits

`run-v2.R` passes **101 checks**. Expected types are specified separately for
constructors, eager/lazy groups, rolling/dynamic groups, Expr/Series namespaces,
Series own-method overrides, selectors/operators, Then chains, DataType conversion,
argument-dependent concatenation, lazy sinks, and `meta$pop()` elements. It also
checks the original seven cursor positions and assigned `q`, negative dispatch/
shadowing/branch cases, argument matching, closure cache isolation, native-output
mutation, document-local registration, runtime binding canaries and unforced
dataset promises. The original 60 checks, 25 inspection checks, and existing
20 completion-typing assertions pass. The full suite remains unrun because the
standard helper loader needs unavailable `mockery`.

One local R 4.6.1/macOS arm64 run measured roughly **0.62 s** for extended source
indexing and **4.0 ms** per warm original-query request over 100 repetitions.
The extended model does more argument/body work and the cursor wrapper currently
repeats some parse/inference work; this is slower than the narrow initial engine.
Long documents, LSP latency, retained memory, rapid edits and background indexing
still need production benchmarks. Node/depth/collection bounds prevent unbounded
work; they are not a measured latency guarantee.

The following still need formal implementation even though the documented
member-availability rate is high:

- Lexical binding IDs and import resolution, complete R argument semantics,
  nonstandard evaluation and dynamic dots; arbitrary user S3/S7 dispatch.
- A control-flow graph and alias/mutation model. Unknown loops invalidate their
  writes but do not model all side effects; constructor extraction recognizes
  reviewed top-level patterns instead of proving arbitrary constructor bodies.
- Transfer summaries with provenance, completeness and dependency IDs. Some
  reviewed builtin/package syntax wrappers remain name-recognized in this
  experiment. Production should resolve their bindings before applying rules.
- Structural validation of dispatch factories and constructor rules, installed
  Polars extraction, a released-version compatibility matrix and namespace
  reload/invalidation. The reference sweep alone does not test convention changes.
- R6 and general S7 object-system extraction; schema/column completion and
  runtime registration in another R process. S7 descriptors here only establish
  class/property surfaces; they do not emulate full S7 validation or dispatch.
- General cursor recovery/replacement ranges, large invalid buffers and literate
  document positions. Only the original query's recovered cursor positions have
  direct checks here.
- Independent type-precision review across the whole corpus. For example, known
  empty metadata and unknown values, partial records and unions need distinct
  production representations. Native scalar/schema returns remain Unknown.

## Formal implementation priorities

1. **Stabilize the generic metadata contract.** Represent Unknown, known empty
   records, partial/open records, ordered class vectors and type alternatives
   separately. Functions need stable binding IDs, formals, receiver/argument
   dependencies and source provenance. Extractors produce data in this contract;
   they do not supply callbacks that execute an inferred instance.
2. **Establish lexical and flow correctness.** Resolve callee bindings before
   applying intrinsic or package rules. Add control-flow and alias/mutation
   analysis, conservative unsupported effects and bounded summaries. Validate
   unrelated list/environment factories, R6 and S7 implementations alongside
   Polars so the core does not acquire package naming conventions.
3. **Prepare validated installed-package snapshots.** Combine reusable
   environment/list, S3, R6/S7 and native-wrapper extractors. Verify each
   convention before accepting its facts, and reject affected facts with a
   reason when validation fails. A background worker selects the user's R and
   library path, applies the explicit package-initialization policy, and records
   package/build and dependency fingerprints. Re-extract once per metadata
   generation; namespace changes and registry mutations invalidate dependent
   summaries. Keep the strict source/existing-binding mode available.
4. **Integrate the provider with document indexes.** Infer the receiver at `$`
   from the current accepted document state and cached package snapshot. Recover
   incomplete syntax within a budget and preserve UTF-16/literate positions,
   replacement ranges, backticks, signatures and documentation identity.
5. **Gate extensions on independent evidence.** Repeat this corpus audit using
   installed extraction on released and development builds. Check representative
   return types and signatures independently of member availability. Add fixtures
   for method additions/removals, changed wrappers, exclusions, shadowing,
   convention breaks and reloads. Require execution canaries and bounded
   long-document/rapid-edit benchmarks before enabling the provider.

These phases extend coverage through reusable extraction and transfer rules,
rather than adding exceptions for individual example chains. The experiments
show that broad coverage does not require a hand-maintained Polars API table;
coverage remains an audit gate alongside type precision, binding safety and
responsiveness.

## Reproduction

From the repository root:

```sh
Rscript inst/experiments/dollar-completion/run-v2.R /path/to/r-polars
Rscript inst/experiments/dollar-completion/audit.R /path/to/r-polars /tmp/audit-v2 v2
Rscript inst/experiments/dollar-completion/audit.R /path/to/r-polars /tmp/audit-v2-data v2-data
```

Use the same upstream revision pinned in coverage.md. Saved evidence is in
`audit-v2-results/` and `audit-v2-data-results/`; the initial `audit-results/`
is preserved. Each directory contains all `$` records and corpus/source/method
inventories, including beyond-root rates. `v2-results.txt` captures the extended
checks/timing. The generated `contexts.rds` remains a local replay artifact and
is not committed.
