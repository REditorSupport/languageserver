# Coverage audit and installed-package extraction plan

**Use extraction methods against the installed package, rather than shipping
its current API as a table.** Keep the general shape engine independent of
r-polars. An adapter must understand implementation conventions and validate
them, but should derive member names, function signatures, class relationships,
private receiver fields, and return summaries from the package it encounters.
This follows ordinary API changes automatically while the conventions remain
recognizable. It cannot guarantee complete or correct inference for arbitrary R
implementations, and a snapshot must be invalidated when its dependencies change.

The experiment establishes feasibility for the requested query. The full audit
shows that the initial adapter is a useful starting point, not broad r-polars
support. The recommendation here supersedes the initial manifest-first plan.
Manifests remain useful optional caches or contracts for opaque native returns.

**Subsequent work:** [robustness.md](robustness.md) documents the extended engine,
101 checks and repeated full audit. It reaches 98.9% reference-example member
availability from source alone, and 100% with extracted `datasets` metadata
(99.4% articles). The baseline below is preserved for comparison; several gaps
identified here are now implemented in the extended experiment.

## What is hard-coded, and where?

Production languageserver files are unchanged. The following describes the
isolated prototype and the proposed architecture, not an existing feature.

| Layer | Initial prototype | Proposed implementation |
| --- | --- | --- |
| Core shape engine | R AST transfer rules; named lists/environment factories; receiver name `self`; syntactic assumptions about `list`, `new.env`, and error functions | General R semantics, lexical callee resolution, parameter/receiver summaries, control flow, dispatch and serializable shapes. Receiver conventions belong in extracted summaries. No Polars class or method table. |
| Package selection | Package name `polars`, root `pl` | Recognize the package identity; derive exported root values from the selected namespace. Include `cs` and `pl$api`. |
| Registry discovery | Name `POLARS_STORE_ENVS`; reconstruct registry contents using prefixes from its source declaration | Inspect actual populated registry environments. The registry schema/name is an adapter convention; individual prefixes and members are data. |
| Class dispatch | Recognize `$.polars_*`; search for direct `names(registry)` statements | Inspect registered dispatch functions and recognize ordered member/registry/delegation branches, including exclusions and overrides. Their returned closures bind the receiver. Never invoke dispatch. |
| Native/public wrapping | Recognize `wrap.polars::`, `.savvy_wrap_`, native factory naming and class assignment | A reusable Savvy extractor recognizes wrapper syntax and derives actual native/public type mappings and output constructors. Unsupported native returns are Unknown or use a package contract. |
| Constructors/properties | Scan a limited list of constructor prefixes; Expr namespace loop special case | Analyze reachable constructor bodies, assignments, aliases, class vectors, active-binding descriptors and referenced registry contents. Do not construct instances or call getters. |
| Individual method results | No `scan_csv -> LazyFrame` or `filter -> LazyFrame` table | Derive summaries from actual function bodies, wrapper outputs and applicable argument/receiver shapes. |

There are still assumptions in an extraction method. For example, knowing that
`POLARS_STORE_ENVS` contains method registries is structural knowledge. That is
different from listing all LazyFrame methods and their types for version X. A
complete generic interpreter for arbitrary custom `$` implementations is not
practical; recognize documented patterns and expose unsupported cases explicitly.

Do not hide package rules in the core by treating a function named `wrap` or a
variable named `self` as intrinsically special. Resolve the package function and
its binding before applying the corresponding extraction/transfer rule. The
prototype does not yet satisfy this requirement in every path.

## Corpus and method

Pinned upstream:
[`56c957815377bb16738df35cfff130c2b2eb43a5`](https://github.com/pola-rs/r-polars/tree/56c957815377bb16738df35cfff130c2b2eb43a5),
September 28, 2026; development version `1.9000.9000.9000`. This is one revision,
not a release compatibility survey.

- Parsed all **127 `R/` source files**, indexing **2,051 named top-level
  definitions**. Inspected return summaries on every extracted public surface,
  including Expr/Series namespaces, eager groups, datatypes and selectors.
  The source index excludes unnamed initialization statements from its definition
  map, which is why some generated bindings are missing.
- Enumerated all **six canonical article files**: `README.Rmd`, the four
  vignettes, and `altdoc/reference_home.Rmd`. Extracted all 80 R chunks/fences,
  including disabled chunks. Generated `README.md` was excluded as a duplicate.
- Enumerated all **713 `man/*.Rd` files**. All **706 with examples** were
  converted to R text using `tools::Rd2ex()`, including `dontrun`, `donttest` and
  guarded `examplesIf` content. The seven pages without examples remain in the
  file inventory. These generated examples also capture the R source's roxygen
  examples, without counting them twice.
- Audited R fences in `DEVELOPMENT.md` and `NEWS.md` separately: 34 blocks.
  Test snapshots and internal development/test scripts are not user articles
  and are not included in these completion coverage denominators. There is no
  separate public examples directory at this revision.

In total: **820 R blocks**, **5,598 actual `$` AST nodes**, **zero inference
errors**. All article/reference blocks parse. Two supplemental development
snippets intentionally omit code (`# snip`) and do not parse; they are recorded
and excluded from the `$` denominator. No article, example, Polars method,
constructor, namespace getter, native call, or package initialization was run.

For each `$`, infer its receiver and ask whether the *exact documented member*
exists in that receiver's candidate set. Propagate straightforward assignments
in source order across article chunks and within each Rd example. A syntactic
origin marker follows `pl`, `cs`, exported `as_polars_*` calls and their aliases.
This marker chooses the denominator; it is not type evidence. Non-Polars `$`
sites, such as ordinary data-frame columns, are reported separately in the raw
data. Function parameters are Unknown instead of inheriting outer variable types.
Guarded documentation wrappers are unwrapped without evaluating their condition.

The audit deliberately uses the **unchanged initial inference/adapter**. It
does not silently add constructor, root or namespace fixes to improve the result.

## Measured member availability

| Corpus | Polars-related `$` sites | Member offered | Availability | Beyond direct roots | Beyond-root availability |
| --- | ---: | ---: | ---: | ---: | ---: |
| Canonical articles | 168 | 109 | 64.9% | 47 / 103 | 45.6% |
| All reference examples | 5,324 | 3,345 | 62.8% | 1,224 / 2,920 | 41.9% |
| Supplemental development/news | 48 | 26 | 54.2% | 2 / 22 | 9.1% |

Direct roots are `pl`, `cs`, `polars::pl`, and `polars::cs`. Root-member
availability is much easier than completing a method result, so report both
rates. Counts are occurrences, not unique methods; many reference examples repeat
the same DataFrame setup. Unknown setup types therefore cause cascading misses.

| Article | Offered / Polars-related sites |
| --- | ---: |
| README | 8 / 8 |
| Introduction (`vignettes/polars.Rmd`) | 50 / 85 |
| Performance | 15 / 29 |
| Custom functions | 4 / 6 |
| Reference overview | 32 / 40 |
| Installation | 0 / 0 (no relevant `$` sites) |

All seven cursor positions in the user's query still resolve, and its assigned
`q` resolves to LazyFrame. That narrow success does not imply coverage of eager
DataFrame construction or general expression composition.

## Where inference stops

Across all three Polars-related corpora:

| Outcome | Occurrences | Explanation |
| --- | ---: | --- |
| Member offered | 3,480 | Receiver known and requested name present |
| Unknown result | 1,772 | Upstream call/constructor/dispatch/body not summarized |
| Unbound receiver | 130 | Every case is the selector root `cs`, absent from the initial adapter's roots |
| Member missing | 158 | Every case is an active datatype constant installed in a loop, such as `Float64`, `String` or `Int64` |

Known receiver shapes alone provide a separate view of the limitations:

| Surface | Extracted methods | Non-Unknown result summaries | Interpretation |
| --- | ---: | ---: | --- |
| `pl` | 83 | 52 | Constructors/dispatch missing; active constants absent from member set |
| LazyFrame | 69 | 54 | Most native transformations work; collections/schema and some branches need more analysis |
| LazyGroupBy | 12 | 12 | Strong narrow support once the receiver is known |
| Expr | 202 | 198 | Strong native-wrapper support; composition into/out of namespaces remains weak |
| DataFrame | 81 | 57 | This assumes an already-known DataFrame receiver; documented constructors often fail first |
| Eager GroupBy | 12 | 1 | Constructor discovery and stored `df`/branch flow missing |
| Series | 29 | 14 | Incomplete method surface; dispatched Expr methods excluded by the parser accidentally |
| Selector | 203 | 198 | Requires a known receiver, which `cs` currently cannot supply |
| Then / ChainedThen | 204 each | 3 each | Active raw receiver and inherited namespace properties missing |

Most Expr namespace surfaces have **zero** known result summaries (List has
1/41). The prototype recognizes `pl$col("x")$str$` and can offer `to_uppercase`,
but cannot continue `...$str$to_uppercase()$alias(...)`. Namespace methods are
therefore a major chain-completion gap. Complete per-surface and per-method
results are in `audit-results/surfaces.csv` and `methods.csv`.

### Root values, datatype constants and constructors

[`R/zzz.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/zzz.R)
populates registries, including `pl`, `cs`, and `pl$api`.
[`R/datatypes-classes.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/datatypes-classes.R#L144)
installs primitive datatypes as active bindings. Inspecting actual names fixes
discovery of these additions automatically; inspect the getter body to derive
the returned DataType shape without obtaining its value. The native-dependent
datatype subtype class vector and conditional fields (`inner`, `fields`, Map
`key`/`value`) require a base/subtype model, not unconditional fields for every
DataType. Do not call `_get_dtype_names()` or `_get_datatype_fields()`.

`pl$DataFrame`, `pl$LazyFrame`, `pl$Series` and `pl$lit` depend on S3 generics
`as_polars_df`, `as_polars_lf`, `as_polars_series`, and `as_polars_expr`.
Their `UseMethod()` calls stop the initial engine. This accounts for many
downstream `df`/`lf` misses (828 and 147 unknown receiver occurrences respectively
across all corpora). Analyze registered methods, receiver/argument classes,
`NextMethod()` and returning paths. A call with an unknown user-defined class
can have an unobserved S3 method; do not assume every method returns the type
suggested by the generic's name. Known built-in argument shapes can narrow the
dispatch; optional package contracts may summarize an invariant return type.

### Expr namespaces and Series delegation

[`namespace_expr_str`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/expr-string.R#L4)
copies `x$\`_rexpr\`` to the namespace's private receiver. The initial constructor
scan recognizes a raw field only when the assignment is exactly `field <- x`,
so it loses the namespace's raw type. General parameter/field propagation fixes
this pattern and follows the native output wrappers afterward.

[`$.polars_series`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/series-s3-base.R#L18)
uses its own methods first, then Expr methods minus `METHODS_EXCLUDE`. The
initial adapter misses the piped `setdiff()` expression. Series namespace
dispatch also delegates to Expr namespace registries. Its member names can be
discovered, but its return summaries must follow
[`expr_wrap_function_factory`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/series-utils.R#L1):
it evaluates the Expr operation through a Series/DataFrame conversion and
returns the resulting Series. Copying Expr return types to these entries would
produce incorrect subsequent completion. Keep delegated signatures and result
transformations attached to the Series member identity.

### Eager groups, when/then, operators and argument dependence

`wrap_to_group_by`, `wrap_to_group_by_dynamic` and `wrap_to_rolling_group_by`
lie outside the initial constructor-prefix list. Their bodies declare their
shapes and retain the DataFrame in `self$df`; the grouped methods compose lazy
aggregation and collection. General constructor discovery plus flow-sensitive
branch joins is preferable to enumerating these helper names.

[`R/expr-whenthen.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/expr-whenthen.R)
assigns `then` through a local function alias and supplies `_rexpr` through an
active binding. Then subclasses also inherit Expr namespaces. The initial
scan loses these relationships. Retrieve descriptors and analyze their syntax;
do not invoke `otherwise()` to acquire an Expr.

Arithmetic and comparison methods in
[`R/expr-s3-operators.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/expr-s3-operators.R)
need general operator/S3 dispatch. For example, `(pl$col("a")^2)$mean()` cannot
be inferred merely by indexing the methods of the inner `col` call.
`pl$concat()` needs argument-dependent summaries: DataFrame, LazyFrame and
Series inputs can give different return shapes. Opaque arguments must not be
evaluated to choose one. Known unions may initially offer common members.

### S7, callbacks, custom namespaces and data schemas

QueryOptFlags and partition classes use S7; they require S7 descriptors and
dispatch support. QueryOptFlags' `$` behavior combines method registries and S7
properties, rather than an ordinary `$.polars_*` definition.

The custom-functions article and reference examples contain callback parameters
or illustrative variables whose input types are not established locally. Do not
invent their classes to raise the coverage percentage. Contextual callback
parameter summaries can improve supported APIs later. The exact audit does not
provide full call-graph/control-flow inference for these cases.

[`pl$api$register_series_namespace`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/api.R#L40)
stores a user-defined constructor in a mutable registry. Static literal
registration can be modeled from document syntax and the constructor's body.
An already-existing registration can be inspected without calling that
constructor, subject to the same binding rules. Arbitrary registration executed
in another R process is not visible to the server without a synchronization
mechanism. This is not solved by inspecting the installed package once.

Runtime column names, file contents and schema-dependent objects remain Unknown.
Static literal schema declarations could support a later column provider, but
are separate from method-surface inference and are not needed for the query.

## Runtime extraction experiment

`run-inspection.R` creates a trusted generic fixture with ordinary methods, a
getter, a delayed promise, a function default, and an environment cycle. The
getter/promise/default would write a marker and throw if invoked. Its **25 checks
pass**: names, literal class metadata, bodies/formals and active-getter syntax
are available; promises remain unforced; no marker is created. Extracted factory
syntax is passed to the same generic engine and supports a chained completion.
Method addition/removal and body replacement alter a metadata fingerprint and
the new body changes the inferred result without editing the extractor.

This verifies the inspection primitives and general inference composition. It
does **not** claim an installed r-polars integration: r-polars is not installed
in this environment, and the fixture is not a runtime copy of r-polars.
No extra production dependency has been added. The experiment uses rlang's
binding-kind inspection and R 4.6.1's `activeBindingFunction()`. Current rlang
requires R >= 4.0 while languageserver supports R >= 3.4, so formal implementation
must choose compatible native binding inspection or feature-gated fallback.
On a runtime without getter-descriptor access, expose the name with an Unknown
value; never fall back to calling the getter.

## Proposed installed-package extraction lifecycle

```text
selected R installation + library path + installed package identity
       -> background namespace/registry inspection
       -> adapter structural validation
       -> bounded static function/constructor/dispatch summaries
       -> serializable, dependency-tracked metadata snapshot
       -> cached document receiver inference
       -> member candidates/signatures at the cursor
```

1. **Choose the actual package.** Use the R executable/library paths selected
   for languageserver, the resolved installation directory and a build
   fingerprint. Version strings alone are insufficient for patched/development
   packages or multiple libraries. The user's separate R process may have loaded
   another build; do not claim that its live state has been inspected.
2. **Acquire metadata outside requests.** Inspect a namespace already available
   to the server, or use a dedicated vanilla worker to load the selected package
   without sourcing project files or user startup profiles. Package loading
   runs package initialization; r-polars initialization includes native work.
   Deliberate materialization of package lazy-load bindings may run
   package-defined expressions too. This is distinct from executing the query,
   its arguments, or user object methods. A strict no-initialization mode uses
   only existing ordinary bindings/source/metadata artifacts and skips deferred
   values. Lack of metadata yields Unknown until preparation succeeds.
3. **Inspect binding kinds first.** Enumerate names with `ls()`. Retrieve ordinary
   bindings only with non-dispatching base operations. Retrieve active-binding
   functions with the descriptor API and inspect their syntax. Skip arbitrary
   promises; any trusted package materialization must stay in the package worker.
   Avoid `$`, `.DollarNames`, `names()`/`[[` dispatch, constructors, native methods
   and completion hooks on inferred user instances. Reading function formals
   does not mean evaluating their default expressions.
4. **Derive structures and summaries.** Read populated registries, registered
   dispatch bodies, constructors, wrapper bodies, ordered class vectors,
   exclusions and lexical constants. Derive roots and active-property names from
   actual values/descriptors. Analyze referenced code with a work budget;
   closures carry stable metadata IDs and lexical dependencies, not executable
   callbacks. Do not recursively dump unrelated namespace environments or native
   pointers. Keep member availability separate from known result shape.
5. **Validate and cache per state.** Check adapter assumptions about registry
   types, dispatch precedence, receiver binding and wrapper constructors. Cache
   using extractor schema/version plus package/build identity and dependencies
   (member names, function bodies/formals, active descriptors, dispatch functions,
   referenced constants and constructor/native mappings). Retain provenance and
   Unknown reasons. Namespace replacement or dependency changes invalidate both
   package metadata and dependent document summaries. Bounded refresh/explicit
   reload is needed for mutable registries; completion requests read snapshots.

The initial fixture fingerprint covers syntax/literals. The extended reader
also captures bounded immediate lexical literals, including getter captures;
it skips promises and arbitrary closure dependency graphs. A production
fingerprint needs complete referenced dependency identities. Neither fixture
detects all possible semantic changes.

## How adapters evolve with r-polars

The source generator uses
[`dev/generate-r-files/s3methods/main.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/dev/generate-r-files/s3methods/main.R)
and class/Expr-subclass/Series-namespace tables to generate dispatch methods.
`DEVELOPMENT.md` documents the registries and wrapping conventions. These are
useful extraction contracts; the installed generated output is the input, so
users do not need the generator tables or the repository checkout.

| Package change | Expected extractor behavior |
| --- | --- |
| Add/remove/rename a method in an existing registry | Discover current names automatically; no method list edit |
| Change arguments/defaults | Read current formals; signature follows automatically |
| Change a native wrapper's explicit return constructor | Derive the new result mapping; invalidate dependent summaries |
| Add an Expr namespace using the recognized registration/constructor pattern | Discover registry entry and constructor shape automatically |
| Add a Series delegated method or change exclusions | Recompute dispatch surface and delegation summary from current bodies/constants |
| Reload/patch the same version or mutate registrations | Re-extract after namespace/dependency invalidation; version alone is insufficient |
| Replace registry schema, receiver binding, object system or wrapper generation | Structural validation fails for affected shapes; update extractor rules or add an object-system extractor |
| Make a result entirely opaque/data dependent | Keep Unknown or obtain a declarative package contract; do not run the method |

Thus "once at runtime" should mean **once per metadata generation**, with cached
results and dependency invalidation. It provides consistency with the inspected
package snapshot, not unconditional completeness or synchronization with every
live user object. Multiple extractors can share list/environment, R6, S3/S7 and
Savvy rules. A Polars adapter should mostly compose these reusable mechanisms.

## Formal implementation work and coverage gates

1. Add the generic shape/summary contract, lexical identity, control-flow joins,
   parameter/field propagation, S3/operator dispatch, constructor analysis and
   bounded caching. Validate with unrelated fluent factories and R6 APIs.
2. Add compatible binding inspection and the background metadata-worker policy.
   Extract roots and populated registries, then active datatypes, dispatch
   precedence/exclusions, namespace raw receivers and Series delegation. Preserve
   stable method identities for signatures, hover and Rd topics.
3. Cover S3 constructors and their built-in argument shapes; eager groups;
   Then/Selector inheritance and aliases; argument-dependent calls; S7 metadata;
   literal custom namespace registration. Unsupported user dispatch, reflection
   and data-dependent properties remain explicit Unknown categories.
4. Integrate `$` cursor recovery/provider and triggers as described in README.
   Reuse accepted document indexes, recover bounded incomplete syntax, preserve
   UTF-16 replacement ranges and backtick names, and respect literate R regions.
5. Re-run this complete audit on installed-package extraction for a released
   version and this development revision. Require all seven query positions and
   representative chains on **every supported surface**, including namespace
   results and Series delegated operations. Require complete discovery of
   statically declared root/registry members and a classified reason for every
   remaining example miss. Report root and beyond-root availability separately;
   do not set an arbitrary corpus percentage that conceals an unsupported surface.
6. Independently verify representative return types, dispatch overrides,
   union/Unknown behavior and signature identity; a returned candidate name is
   not proof that the receiver class is correct. Use trusted development-only
   comparisons where appropriate. Keep canaries for user arguments/defaults,
   constructors, active getters, promises, dispatch, completion hooks and native
   calls. Exercise API additions/removals and convention-breaking fixtures.
7. Benchmark long/incomplete documents, background indexing and rapid edits.
   Completion requests must use metadata snapshots and bounded AST work; target
   <10 ms warm provider work and preserve the existing typing checks. The narrow
   0.20 ms prototype query timing does not establish these guarantees.

Availability here is neither precision nor runtime validity. The prototype's
constructor scans, limited lexical analysis, partial dispatch and union behavior
can be incomplete or incorrect. This audit uses complete-code ASTs; it does not
validate every editor cursor/replacement range or stale-document situation.

## Reproduction and saved evidence

From the languageserver repository root, with the pinned source checkout:

```sh
Rscript inst/experiments/dollar-completion/run.R /path/to/r-polars
Rscript inst/experiments/dollar-completion/run-inspection.R
Rscript inst/experiments/dollar-completion/audit.R /path/to/r-polars /tmp/polars-audit
```

`audit-results/` contains the corpus/source inventory, all `$` records, per-file
and corpus summaries, direct-root breakdown, reasons, and method/surface results.
`source_line` in dollars.csv is the enclosing fence/examples start, not an exact
cursor line; `receiver` is shortened display text, while AST inference uses the
full expression. Supplemental parse failures are in blocks.csv. `audit.R` also
writes contexts.rds for local replay; this process-specific artifact is not
committed. `inspection-results.txt` captures the 25 inspection checks; the original
60 checks and existing 20 typing assertions also pass. Full repository tests
were not run because the standard helper loader requires missing `mockery`.
