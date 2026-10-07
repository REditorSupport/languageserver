# Production implementation

The branch now enables a static `$` completion provider. `R/member-inference.R`
contains the generic bounded analyzer; `R/member-completion.R` consumes document
syntax indexes and constructs LSP items. `R/member-r6.R` extracts declarative R6
surfaces. `R/member-extraction.R` discovers declarative constructors, S3 dispatch,
registry membership, bound methods, namespace getters, S7 declarations, and
delegation factories.
No Polars package, root, class, registry, helper or factory name occurs in production
R/C code. Polars appears only in acceptance tests and the research corpus.
`R/member-metadata.R` prepares inert package snapshots in the existing package-resolution worker.

## General extraction and evolution

Installed-package preparation enumerates ordinary environment bindings and matches
closures/environment aliases by identity. It derives classes from constructor
syntax and registry precedence/exclusions from `$` branches. Standard S3
`UseMethod` declarations provide constructor input-class constraints. Method
factories are recognized through constructor field assignments or dispatch calls;
delegation summaries take their formals, captures and output body from the actual
closure factory. No per-version method/return table or package adapter is used.

Rules for base primitives and verified imports from R6, rlang and S7 remain
explicit language/library semantics. They do not describe a Polars API. An
unrelated package fixture uses different class, registry, pointer, receiver and
factory names and verifies nested namespaces, native wrappers, delegation,
signatures and API changes. Unknown implementations fall back conservatively.

Package changes to names, signatures and supported output bodies are picked up
when metadata is rebuilt. A changed package architecture can require new generic
extraction rules. This is automatic consistency within the supported patterns,
not a promise of universal compatibility. The source-only research path validates
a registry population recipe before reading its literal prefix map; production
installed-package extraction instead uses actual populated bindings.

The original seven query positions and assigned `q` resolve using installed
r-polars 1.16.0. The development source revision from the audit remains a
source-input fixture. Expr and Series namespaces preserve their distinct return
shapes; tests mutate wrapper syntax to verify that result types follow bodies.

## Acquisition and execution policy

The worker uses the server's selected R and library paths and disables system
and user startup profiles. It loads packages requested by the document and
materializes their namespace metadata, which can run package initialization.
It never sources a document or evaluates its arguments, constructors, methods,
active properties, completion hooks or examples. Native binding inspection
checks active and deferred bindings before retrieving ordinary values. Active
getter syntax is read only when the supported R API exposes it. Unforced
arbitrary registry/getter promises remain Unknown.

Metadata snapshots contain syntax, literal captures, names, classes and dependency
fingerprints, with package path/build identity. They are bounded to 32 MiB per
workspace; summary caches are limited to 4 MiB and 512 entries per index. Package
resolution and document saves refresh snapshots; obsolete package requests are
rejected. Results survive intervening edits when the requested packages match.
Completion requests read the last accepted snapshot. Registrations
made in a separate interactive R process are not observed. Arbitrary namespace
mutations in another process require a saved document/restart and do not imply
live synchronization.

The `member_completion` setting enables the provider and worker extraction. The
existing token provider remains the fallback for Unknown receivers. Known
receivers offer their static members, including backtick-quoted fields and method
snippets, with UTF-16 replacement ranges. Items store method IDs and signatures
so resolution does not confuse an Expr `sum` with `base::sum`.

## Bounds and current limits

The document parser records ASTs and source positions once per accepted revision.
Incomplete buffers recover complete statements without evaluating them. Cursor
recovery considers at most 129 lines / 64 KiB. Receiver inference has a 20,000
node, 64-level and 250 ms work bound; expensive cold summaries can fall back to
Unknown and are not cached after a cutoff. Warm 5,000-/20,000-line fixture member
requests measured below 3 ms locally; this is not an end-to-end latency guarantee.

This is a conservative supported-pattern analyzer, not a sound R type checker.
Unknown branch/loop writes, unsupported custom `$` dispatch and opaque native
results stay unresolved. Full NSE/dynamic dots, arbitrary alias mutation,
interprocedural side-effect proofs, all S7 semantics and schema-dependent column
completion remain outside the implemented subset. R6 initializers and active
properties are not used to infer fields; public declaration/inheritance metadata
is used instead. Cross-file fluent value flow is not yet analyzed. Package
metadata budget/schema failures produce a recorded preparation error and fallback;
architecture changes need reviewed generic extraction rules.

The full earlier audit measures the experimental analyzer's member availability.
Its 98.9%/100% rates must not be presented as production cursor/type coverage.
Production acceptance uses LSP tests, independent expected types, binding
canaries and installed/source extraction fixtures.

## Validation

- 94 member-provider assertions pass, plus live LSP tests for generic fluent
  factories and asynchronous installed-Polars preparation. Existing completion,
  typing, document/task, handler, workspace and cache tests pass.
- The complete source corpus run using the generic production engine retains
  133/168 article (79.2%), 5,267/5,324 reference (98.9%) and 44/48 supplemental
  (91.7%) members. It parses 820 R blocks with two intentionally incomplete
  blocks and zero inference errors. These measure member availability at real
  `$` AST nodes, not inferred-type precision or editor cursor completeness.
  Reproduce with `Rscript inst/experiments/dollar-completion/audit.R
  /path/to/r-polars /output/dir production`; captured CSVs are in
  `audit-production-results/`. All R implementation files are parsed by extraction;
  all canonical articles and generated Rd examples are included by the audit.
- `R CMD build` and `R CMD check --no-tests --no-manual --no-build-vignettes`
  pass with `_R_CHECK_FORCE_SUGGESTS_=false` (`covr` and `pacman` are unavailable).
  Native binding inspection compiles and passes canaries on R 4.6.1; older R API
  branches still need CI matrix validation.
- The full suite was run. Formatting/diagnostics/code-action/protocol failures
  also reproduce on the pre-implementation branch in this environment. The first
  generic-member request had a development JIT timing failure, fixed by starting
  its inference timer after compilation. Invalid empty assignment names caused
  a symbol-provider regression in the new index, fixed by skipping those names.
  The targeted final run passes. The full suite is not reported as passing.

## Further implementation work

1. Validate the native binding-inspection branches on the supported R release
   matrix, including getter and promise canaries. R 4.6.1 is validated locally.
2. Add package provenance to every abstract value and compose indexes across
   packages; the current provider selects a primary package and handles explicit
   namespace calls, but arbitrary cross-package fluent returns need more work.
3. Expand registry/delegation extraction through aliases, helper calls, conditionals
   and nonliteral registration. Add unrelated fixtures for every rule, preserving
   Unknown when input-dependent names or output schemas cannot be determined.
4. Add expected-type and cursor fixtures for currently unresolved article cases,
   then use those fixtures to extend NSE/dots and cross-file flow. Keep corpus
   member availability separate from type precision and latency measurements.
