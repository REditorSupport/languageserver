# Static completion of chained `$` methods

Research branch: `codex/r-polars-static-completion`. This branch contains an
isolated experiment and an implementation plan; it does not enable a production
completion provider or change existing LSP behavior.

**Recommendation: implement a general receiver/shape analysis layer, with
declarative metadata adapters for object systems and native wrappers.** The
r-polars example is feasible without evaluating the query, its arguments, or
its input files. Inferring the object's method surface is substantially easier
than inferring its data schema. Complete method names first; treat unknown
results conservatively.

## Experiment and reproduction

Run from the languageserver repository root with R and the repository's runtime
dependencies, plus `pkgload`. No installed r-polars, Rust compiler, or CSV is
required. The script sources only these experiment files; package and document
source are passed to `parse()`, never `source()` or `eval()`.

```sh
git clone https://github.com/pola-rs/r-polars.git /tmp/r-polars-completion-source
git -C /tmp/r-polars-completion-source checkout 56c957815377bb16738df35cfff130c2b2eb43a5
Rscript inst/experiments/dollar-completion/run.R /tmp/r-polars-completion-source
```

The analyzed upstream revision is
[`56c957815377bb16738df35cfff130c2b2eb43a5`](https://github.com/pola-rs/r-polars/tree/56c957815377bb16738df35cfff130c2b2eb43a5),
dated September 28, 2026, with package version `1.9000.9000.9000`. The languageserver
base is `0b195a3c6facf7c58f9735751b82aee539ea5090`. Other r-polars versions need
their own compatibility checks; historical implementations may use different
object and native-wrapper conventions.

Files:

- `inference.R`: package-independent AST transfer rules, named-list and
  environment shapes, closures, receiver binding, call summaries, and cursor
  completion. Contains no r-polars class names or method return table.
- `polars-adapter.R`: parses the registry declarations, `$` methods, public
  constructors, and generated native wrappers of this r-polars revision.
- `run.R`: executable experiments, negative cases, metadata mutation, delimiter
  recovery against languageserver, coverage counts, and timing.

## Results

All seven `$` cursor positions in the original example resolve correctly,
including the two inside incomplete argument lists. After the final expression
and at a subsequent `q$`, the inferred type is also `polars_lazy_frame`.

| Cursor after | Inferred receiver | Public candidates | Expected member |
| --- | --- | ---: | --- |
| `pl$` before `scan_csv` | `pl` | 83 | `scan_csv` |
| `pl$scan_csv(...)$` | `polars_lazy_frame` | 71 | `filter` |
| `pl$` before `col` inside `filter(` | `pl` | 83 | `col` |
| `...$filter(...)$` | `polars_lazy_frame` | 71 | `group_by` |
| `...$group_by(...)$` | `polars_lazy_group_by` | 13 | `agg` |
| `pl$` before `all` inside `agg(` | `pl` | 83 | `all` |
| `pl$all()$` | `polars_expr` | 211 | `sum` |

Additional successful cases include partial member prefixes, `polars::pl`,
`library(polars)` identified syntactically, `pl$col("x")$str$to_`, and the
following unrelated source-defined factories:

```r
list_factory <- function(x) {
  list(filter = function(p) list(collect = function() x), value = x)
}
list_factory(unresolved)$filter(predicate)$ # collect

env_factory <- function(x) {
  self <- new.env(parent = emptyenv())
  self$filter <- function(predicate) self
  self$group_by <- function(...) list(agg = function(...) self)
  self$collect <- function() list(value = x)
  self
}
env_factory(unresolved)$filter(predicate)$group_by("x")$agg()$
# collect, filter, group_by
```

The same engine also propagates a named-list shape through a parameter-returning
function. This establishes a general approach beyond r-polars' naming
conventions. R6 and arbitrary custom S3 `$` dispatch are not implemented in the
experiment.

The runner passes **60 checks**. They include an argument that would write a
marker file and throw if executed: completion succeeds, the file does not
exist, and the r-polars namespace remains unloaded. Unknown methods, unresolved
roots, shadowed `pl`, schema-dependent results, an unknown branch, early returns,
and recursion stop inference. Changing the parsed native `filter` wrapper from
returning a LazyFrame to returning a DataFrame changes the inferred completion
receiver accordingly. There is no hard-coded `filter -> LazyFrame` rule.

Exploratory coverage over methods in four registries:

| Surface | Methods inspected | Non-unknown return shape |
| --- | ---: | ---: |
| `pl` | 83 | 52 |
| LazyFrame | 69 | 54 |
| LazyGroupBy | 12 | 12 |
| Expr | 202 | 198 |

Candidate counts include known properties/namespaces and are not exhaustive API
counts: for example, the prototype does not extract every datatype constant
generated in a loop into `pl`. Coverage counts methods only. These counts
measure whether the prototype finds a static shape; they are
not a proof of correctness for every method or a runtime comparison.

One local run on macOS arm64, R 4.6.1, measured about **157 ms** to parse/index the
upstream R source, **0.20 ms** per warm request over 1,000 repetitions of the
example, and roughly **8.1 MiB** of retained metadata after the coverage sweep.
These measurements exclude LSP transport, completion-item construction, editor
rendering, large-document recovery, and cold package installation discovery.
The prototype parses the entire supplied prefix per request. Production must
reuse document indexes and bound recovery, especially in long scripts.

## Why the source contains enough information

The exported `pl` object is an environment, populated from declarations in
[`R/zzz.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/zzz.R#L25).
`POLARS_STORE_ENVS` describes the mapping of name prefixes to method registries.
The public `$` methods in
[`R/generated-s3methods-dollar.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/generated-s3methods-dollar.R)
retrieve methods from these registries and bind their `self` receiver. Therefore
`ls()` on a LazyFrame instance alone would miss much of its API.

The relevant call chain has explicit output constructors:

1. [`pl__scan_csv`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/input-csv-functions.R#L96)
   ends in `wrap(PlRLazyFrame$new_from_csv(...))`.
2. [`lazyframe__filter`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/lazyframe-frame.R#L435)
   ends in ``wrap(self$`_ldf`$filter(...))``.
3. [`lazyframe__group_by`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/lazyframe-frame.R#L199)
   ends in ``wrap(self$`_ldf`$group_by(...))``.
4. [`lazygroupby__agg`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/lazyframe-group_by.R#L40)
   ends in `wrap(self$lgb$agg(...))`.

Generated wrappers in
[`R/000-wrappers.R`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/R/000-wrappers.R#L4062)
explicitly wrap native results as `PlRLazyFrame`, `PlRLazyGroupBy`, or `PlRExpr`.
Public wrapper constructors assign literal class vectors and the private raw
receiver fields. The adapter composes those facts. Rust signatures independently
confirm `filter -> Self`, `group_by -> PlRLazyGroupBy`, and
[`agg -> PlRLazyFrame`](https://github.com/pola-rs/r-polars/blob/56c957815377bb16738df35cfff130c2b2eb43a5/src/rust/src/lazygroupby.rs#L18).
The experiment does not need to parse Rust or call `.Call()`.

`pl$col()` has argument-dependent branches, but its returning paths converge on
Expr after wrapping. `pl$all()` composes those Expr-returning calls. Expr namespace
getters are installed with active bindings; their names and shapes can be read
from the namespace registry and constructor source without invoking getters.

`infer_schema_files`, the CSV location, the predicate, and aggregate validity do
not affect these method surfaces. Completion may be useful even when the call
would fail at runtime. CSV column names, dynamic schemas, and custom registered
namespaces cannot generally be recovered from this information.

## Current languageserver integration gaps

- `src/token.c::scan_token_c()` intentionally suppresses a token immediately
  preceded by `$`. `Document$detect_token()` returns empty token/accessor fields
  for `pl$`, `pl$scan`, and `...$fil`; the runner verifies this baseline.
- `R/capabilities.R::CompletionOptions` advertises `.` and `:` triggers, without
  `$`.
- `R/completion.R::completion_reply()` selects scope/workspace/token/argument
  providers. It has no member provider or arbitrary receiver expression.
  Token fallback can suggest words present in the document, but cannot supply
  the object's method surface or bound method signatures.
- `PackageNamespace` in `R/namespace.R` caches top-level symbols, signatures,
  and docs. It cannot describe methods supplied by custom `$` dispatch.
- `parse_document()` already produces serializable parse indexes and function
  syntax. Invalid buffers yield an empty parse index, so a trailing `$` needs
  local syntax recovery from the current document version.
- `missing_closing_delimiters()` and the native bracket scanner recover the
  necessary delimiters for all seven example cursor positions. This can be
  reused; completion should not call the formatting/styler pipeline.
- Existing R6 type-hierarchy analysis can discover class members, but does not
  propagate instance and method-result shapes through expressions.

## General implementation design

### 1. Separate shapes, functions, and metadata discovery

Use a package-independent data model with stable IDs and serializable records:

```text
Shape = Unknown(reason) | Record(fields, open) | Instance(class_id)
      | Function(function_id, bound_receiver) | Union(alternatives)
Result = Shape + provenance + completeness
Field = name + kind + shape + function_id/signature + documentation_id
Summary = parameter/receiver constraints + result transfer + dependencies
```

`open` records can have additional unknown fields. Do not confuse a known empty
shape with Unknown. Unknown absorbs alternatives when deciding guaranteed
members; a known union can expose common members initially. Keep the ordered S3
class vector distinct from a union of alternative result classes. Track method
overrides and inherited members in the proper dispatch order.

The analysis API should resemble `infer_expression(node, lexical_context)` and
`members_of(shape, prefix)`. Metadata adapters produce records, not executable
callbacks from a package. LSP completion should consume the same records that
signature help, hover, and navigation can use later.

### 2. Analyze source with bounded transfer rules

Start with names, local assignments/aliases, named lists, function literals,
ordinary function calls, environment factories with literal member assignment,
constructor calls, `$` lookup, returned parameters, and returned receivers.
Native `|>` is normalized by R's parser. Other pipe forms need explicit syntax
rules later, rather than evaluation.

Build a control-flow model for explicit returns, branch joins, loops, and
unreachable error paths. Resolve the callee lexically before applying any
builtin rule; a user function named `list`, `new.env`, `stop`, or `wrap` must not
inherit a builtin summary. Bind formals using R's argument rules, model defaults
as syntax, and preserve closure/receiver identities with references rather than
copying whole objects. Unsupported mutation, dynamic member names, `get`,
`assign`, `eval`, arbitrary native calls, or unresolved dispatch produce Unknown.
Do not infer a return type by searching for any constructor somewhere in a body.

Memoize reusable summaries by function identity/source hash, receiver shape,
relevant argument shapes, and dependencies. Bound traversal nodes, recursion,
union size, elapsed work, and retained bytes. Recursive summaries need a bounded
fixed point or a conservative stop. A context-dependent recursion cutoff must
not poison shared caches with an otherwise resolvable Unknown.

### 3. Add object-system and native-wrapper metadata sources

- Generic source extraction handles ordinary lists/environments and explicit
  `structure(..., class = ...)` constructors.
- An R6 adapter extracts public methods, fields, active-property descriptors,
  inheritance, and `self` return summaries. Never create an R6 instance or invoke
  an initializer/getter. Keep private members available for analysis only.
- Custom S3 `$` APIs require a recognized registry/dispatch convention or
  declarative metadata. Arbitrary `$` methods can compute names from anything;
  generic source analysis cannot guarantee complete results for all R programs.
- The first specialized extractor targets r-polars/Savvy conventions. Generate
  member/return/signature metadata from the exact package source at build time;
  validate version/structure and keep its conventions out of the core engine.
- Define a declarative package metadata format for classes, members, method
  signatures, return shapes, inheritance, and namespace properties. Packages
  using opaque `.Call`/Rcpp dispatch can supply this without exposing native
  source analysis. Prefer an upstream-generated manifest over a growing
  languageserver table of individual method names and return types.

### 4. Support installed packages without requiring source checkouts

Installed R packages usually replace their source files with lazy-load databases.
The source-checkout adapter is an experiment input, not a deployable discovery
strategy. Prefer a versioned JSON manifest shipped under the installed package's
`inst` contents (e.g. `lsp/member-metadata.json`), which can be read without loading
the package. Validate it as data and do not execute manifest expressions.

For existing, already-loaded package namespaces, an optional discovery path can
inspect ordinary method registry bindings and `body()`/`formals()` of ordinary
closures without calling them. Reading an active binding or forcing a promise
can execute code: skip those bindings unless a separate, explicit trusted-package
policy applies. Names from `ls()` need no getter execution, but retrieving their
values is a separate operation. Never invoke `$`, `names()` methods,
`.DollarNames`, getters, or custom completion hooks on user-derived instances.

Namespace loading itself executes package initialization. Existing
`PackageNamespace` uses `asNamespace()`; this should not be presented as purely
static or reused blindly for the new feature. Do not load packages or evaluate
project code to discover member completions on request. If a supported package
has neither a manifest nor safely available metadata, return Unknown. A bundled
adapter/manifest needs exact-version compatibility checks and a maintenance
strategy; coverage across released versions remains to be established.

### 5. Resolve the current cursor and integrate LSP

Recover the receiver AST, member prefix, and replacement range from the current
buffer, including multiline calls, parentheses, comments, strings, backticks,
and whitespace around `$`. Use the existing bracket scanner to close local
incomplete syntax around a unique placeholder. Verify that the placeholder is
the member at the cursor in the parsed AST. Discard synthesized syntax afterward.
Respect R Markdown/Quarto R regions and UTF-16 positions.

Use accepted parse indexes for surrounding bindings; recover only a bounded
current expression when the buffer is incomplete. Earlier unrelated parse
errors must not require parsing the entire document prefix. Resolve symbols by
lexical scope, definition position, source/import relationships, and the current
document revision. Formals/local shadowing override imported roots.

Add a general `member_completion()` path before ordinary providers, and add `$`
to `CompletionOptions$triggerCharacters`. Known receivers return their matching
members; unresolved receivers may retain current token fallback without claiming
those tokens are verified fields. Respect fuzzy matching, ranking,
`max_completions`, and `isIncomplete`. Replace only the member prefix, quote
non-syntactic names with backticks, and insert method call snippets only when
appropriate. Store a stable member/function identity in completion `data`, not
just the label, so `sum` resolves against the inferred Expr or LazyFrame.

Follow up with bound method argument completion, signature help, and static doc
resolution using that same identity. A field named `sum` must not fall through
to namespace lookup for `base::sum`. Package manifests can map methods to existing
Rd topics such as `expr__sum` without running the method.

## Proposed implementation sequence and acceptance criteria

1. **Receiver parsing and generic shapes.** Implement the shape model, lexical
   binding index, current-buffer recovery, and a `$` provider for named lists
   and ordinary source-defined factories. Add end-to-end LSP checks for empty/
   partial members, multiline chains, aliases, shadowing, backticks, Unicode,
   comments/strings, invalid surrounding source, and literate documents.
2. **General summaries and R6.** Add return/control-flow analysis, parameter and
   receiver propagation, signature identities, and R6 metadata extraction. Use
   at least one unrelated fluent R6 API and the ordinary factories as acceptance
   fixtures. Test inherited/overridden methods and active properties without
   constructing objects. Keep unsupported reflection conservative.
3. **Declarative metadata and r-polars.** Settle the manifest schema/discovery
   policy and generate a fixture from a pinned released r-polars version as
   well as this development revision. Add the r-polars adapter with structural
   checks. All seven example cursor positions, `q$`, `polars::pl`, and Expr
   namespaces must pass without a CSV, installed Rust runtime, or user code
   execution. A metadata change must change results without editing inference
   rules. Establish how users with ordinary installed r-polars obtain metadata
   before enabling this provider by default.
4. **Responsiveness and shared features.** Move summary preparation to background
   package/document indexing, invalidate on accepted edits, dependency changes,
   package paths/versions, or namespace replacement, and use byte-bounded caches.
   Benchmark cold/warm requests in 5,000- and 20,000-line documents and rapid
   edits. Target under 10 ms for warm provider work; measure end-to-end latency
   separately and preserve the current typing benchmarks. Add method signatures,
   argument completion, hover, and documentation resolution using stable IDs.

Correctness checks should compare against independently specified fixtures and
generated manifest contracts. Keep execution canaries for user arguments,
constructors, initializers, active bindings, `$`/`.DollarNames` dispatch, and
native calls. Unknown cases must produce bounded responses. Runtime comparisons
may be done only in explicit development tests against trusted package fixtures;
the server's request path must never perform them.

## Limits of this prototype

This is a feasibility experiment, not a sound R type checker. Its package adapter
recognizes reviewed conventions in one revision; constructor scanning is not
general control-flow proof. The generic engine implements only straightforward
named-list/environment factories and limited tail branches. It does not cover
full argument matching, local-function lexical scopes at the cursor, explicit
returns, arbitrary mutations, R6, general S3 dispatch, inherited Expr properties,
version invalidation, non-syntactic prefixes, or large-buffer recovery. Builtin
recognition and error-path handling assume the fixture's reviewed bindings.

Some prototype summaries are deliberately argument-independent; others require
argument shapes. This distinction must become explicit in the formal summary
model. Properties such as LazyFrame `columns` can be offered by name, but their
value is Unknown: never call `collect_schema()` to learn it. Literal user schema
declarations could support a separate future column-name provider; CSV contents
and runtime-added namespaces remain outside static method completion.

The experiment runner passes with `pkgload::load_all(helpers = FALSE)`. The
existing `test-completion-typing.R` also passes all 20 assertions with test helper
loading disabled. The repository's standard helper loader currently needs the
missing suggested package `mockery`; the full test suite was not run. Production
files are unchanged. A captured experiment run is in `results.txt`.
