# Compiler fixture suite organization

Status: proposal with an initial migration slice. Baseline:
xgo-dev/main@c1d5da2 (2026-08-29).

## Summary

The compiler fixture suites under `cl/_test*` have grown by accumulation.
They currently mix compiler instruction lowering, ABI checks, standard-library
behavior, runtime semantics, build-driver behavior, and external library
integration. Several fixtures validate the same generated instruction, while
some large fixtures make it difficult to tell which assertion protects which
compiler capability.

This proposal reorganizes the suites around explicit test contracts:

- one owner for each lowering, runtime-helper selection, ABI rule, metadata
  edge, or target-specific code path;
- small fixtures that expose the minimum Go construct needed to reach that
  path;
- direct unit tests for algorithms that do not require source-to-LLVM
  integration;
- no standard-library or `goplus/lib` dependency when a local declaration is
  sufficient;
- separate ownership for compiler IR, runtime semantics, standard-library
  compatibility, dead-code analysis, LTO, and package metadata.

The current baseline contains 265 fixture directories. The proposed layout
contains 145 fixture directories plus six direct CFG subtests in `cl/blocks`.
Using logical scenarios as the common unit, the suite changes from 265 to 151,
a reduction of about 43%, without merging unrelated behavior into large
replacement fixtures.

The detailed current-case inventory and migration mapping is in
[cl-test-fixtures-inventory.md](cl-test-fixtures-inventory.md).

### Initial migration slice

The implementation accompanying this proposal deliberately starts with cases
whose ownership can be changed without combining unrelated compiler paths. It
reduces the physical fixture count from 265 to 237. The seven
`cl/_testdefer` directories become six direct, table-driven `cl/blocks`
subtests, so the common logical-scenario count is 243.

This slice removes byte-identical or strict-subset fixtures, moves pure
standard-library and build-selection checks to their existing owners,
consolidates the small C vararg smokes, replaces thin external wrappers with
local C/C++ declarations, and adds a focused integer divide/remainder owner.
Its FileCheck assertions select only the instructions and ABI edges that
define each contract; runtime output continues to cover the surrounding
behavior. The proposed 145-directory/151-scenario layout remains the target
for later migrations after each destination owner exists.

The materially changed LIT owners in this slice declare their scope directly:

| Fixture | Scope | Primary instruction/ABI contract |
| --- | --- | --- |
| `arith-divrem` | `common` | guarded signed/unsigned division and remainder |
| `uint` | `common` | unsigned-width arithmetic and print widening |
| `strlen` | `common` | C varargs for zero, `size_t`, i32, and i64 arguments |
| `cppabi` | `common` | local C++ compile/link and `extern "C"` call ABI |
| `once` | `os (Darwin/Linux)` | POSIX `pthread_once` initializer and callback ABI |

## Context and problem

Six directories use the same normal compile/run/FileCheck harness but are
separated mostly by history:

- `cl/_testdata`
- `cl/_testgo`
- `cl/_testlibc`
- `cl/_testlibgo`
- `cl/_testpy`
- `cl/_testrt`

Other suites have distinct contracts:

- `cl/_testdefer` tests `cl/blocks.Infos` and does not generate LLVM IR.
- `cl/_testdrop` tests DeadcodeDrop reachability roots and fixed-point
  propagation.
- `cl/_testlto` tests link-time reachability, metadata roots, and static-itab
  transformations.
- `cl/_testmeta` tests package metadata sections and type or method identity.

The current organization has four recurring problems.

### Duplicate ownership

Examples include:

- `deferdispatch-wasm` and `deferdispatch-wasm-target` contain byte-identical
  Go input.
- `asm` is a strict subset of `asmfull`.
- `hello` and `strlen` both primarily check the same external `strlen` call.
- `printf` and `printval` differ only in C vararg arity.
- `map` is covered by the broader map-helper selection fixtures.
- `ptrtothislazynew` is covered by `ptrtothislazy`.
- several CGo fixtures generate the same wrapper slot-load and indirect-call
  sequence.

### Oversized fixtures

`cgofull`, `abimethod`, `reflectconv`, `reader`, `cursor`, and the
`_testdefer/gobuild` input combine many unrelated operations. A failure in one
of these fixtures produces a large golden diff and does not identify the
responsible compiler path.

The replacement must not trade many old cases for one new giant case. A
fixture may contain several short functions only when they exercise variants
of the same lowering or ABI rule.

### Misplaced behavioral tests

`_testlibgo` mainly validates calls into the Go standard library, which belongs
to `test/std`. Goexit lifecycle, nested panic replacement, SIGSEGV recovery,
and other runtime behavior belongs to `test/go` or the corresponding goroot
suite. `runextest` and `runtest` validate test-package selection and belong to
the build-driver tests.

### Accidental external dependencies

About 45 top-level fixture cases import `github.com/goplus/lib`. Most use it
only as a thin set of linkname declarations. This makes compiler instruction
tests dependent on an external module and obscures the declaration that
selects the LLGo lowering.

## Goals

1. Give every compiler lowering, helper selection, ABI rule, metadata edge,
   and target-specific path exactly one primary fixture owner.
2. Keep compiler fixtures small enough that their source and meaningful CHECK
   assertions can be reviewed together.
3. Preserve independent regressions even when they use the same Go syntax.
4. Remove standard-library, runtime-semantic, build-driver, and external
   integration coverage from the compiler fixture layer.
5. Cover all LLGo intrinsics either through one focused acceptance fixture or
   a stronger source-to-IR/unit test.
6. Run under every Go version selected by CI without a fixture overriding the
   selected toolchain.
7. Preserve target-specific checks for Darwin, Linux, Windows, wasm, and
   embedded compilation where the generated path is genuinely different.

## Non-goals

- Reducing the count by combining unrelated paths.
- Replacing `test/std`, `test/go`, goroot, runtime, or build-driver tests.
- Copying the Go compiler backend micro-optimization suite. LLGo needs
  frontend-to-LLVM correctness and LLGo ABI/helper selection, not instruction
  scheduling parity with cmd/compile.
- Requiring every low-level unit test to have a second acceptance fixture.
- Changing compiler behavior silently as part of a fixture-only migration.

## Test contract taxonomy

Every test must declare one primary contract.

| Contract | Primary owner | What it proves |
| --- | --- | --- |
| Source-to-pre-ABI IR | focused `cl` unit test | frontend instruction selection before target ABI expansion |
| Source-to-post-ABI IR | `cl/_testcodegen` | generated LLVM calls, values, control flow, and ABI shape |
| Runtime semantics | `test/go`, goroot, runtime tests | observable Go behavior, panic lifecycle, scheduling, GC |
| Standard-library compatibility | `test/std` | Go standard-library API and semantic compatibility |
| Build and package selection | `internal/build` tests | test packages, build tags, source-file collection, toolchain selection |
| LLGo intrinsic lowering | `cl/_testintrinsics` or stronger unit test | exact LLGo-only instruction mapping |
| Debug metadata | `cl/_testdebug` | DI types, parameters, scopes, and locations |
| Dead-code reachability | `cl/_testdrop` | roots and demand propagation without LTO |
| Link-time transformation | `cl/_testlto` | GlobalDCE, metadata roots, devirtualization, finite-name propagation |
| Package metadata | `cl/_testmeta` | metadata sections, cross-package identity, reflect/type edges |
| Block ordering/classification | direct `cl/blocks` tests | `DeferAlways`, `DeferInCond`, `DeferInLoop`, and `Info.Next` |

A runtime-output assertion may accompany an IR fixture when it is the smallest
way to validate the generated path, but output alone is not evidence that a
specific lowering was selected.

## Fixture ownership rules

### One primary compiler capability

Each fixture has one named owner such as `map/helper-selection`,
`defer/interface`, or `intrinsics/atomic-raw`. Related variants may share a
fixture when:

- they enter the same production lowering;
- the important difference is an opcode, type width, ABI shape, or target
  prefix; and
- each variant remains a short, independently labelled function.

Otherwise the fixture is split.

### Declare one target scope

Every new, moved, or substantially rewritten `LITTEST` fixture declares one
scope immediately after its marker:

```go
// LITTEST
// Scope: common
```

The allowed scope labels are:

| Scope | Contract | Allowed non-`CHECK` prefixes |
| --- | --- | --- |
| `common` | the asserted pre- or post-ABI invariant is identical on every supported target | none |
| `os (...)` | the invariant is selected by the operating-system ABI or runtime | OS prefixes such as `DARWIN`, `LINUX`, or `WINDOWS` |
| `arch (...)` | the invariant is selected by the architecture or calling convention | architecture prefixes such as `AMD64`, `ARM64`, or `WASM` |
| `os+arch (...)` | one capability genuinely depends on the OS/architecture interaction | exact prefixes such as `DARWIN-ARM64` or `LINUX-AMD64` |

Choose the narrowest scope that owns the capability. A fixture may use
portable `CHECK` directives for relationships shared inside an `os`, `arch`,
or `os+arch` contract, but it must not also become the owner of an unrelated
common capability. Split independent contracts instead of accumulating
several scopes in one source file.

`os+arch` is not a reason to split one inseparable capability mechanically.
It is retained when both dimensions change the same ABI or lowering, but its
scope comment must state why neither an OS-only nor an arch-only rule is
accurate. Exact GOOS/GOARCH prefixes are otherwise a review signal that the
fixture has mixed scopes.

FileCheck variables must be closed over by the same active prefix. A variable
defined by `DARWIN`, for example, is referenced only by `DARWIN` directives;
a value needed by shared checks is captured by a `CHECK` directive. Never
define a variable under `DARWIN-ARM64` or `LINUX-AMD64` and consume it from a
shared `CHECK-NEXT`: other supported architectures see the use without the
definition. This prefix-closure rule applies to labels, captures, `-NEXT`,
`-SAME`, and `-NOT` relationships.

Legacy fixtures need not receive mechanical scope comments. Declaring and
normalizing scope is mandatory when a fixture is added, moved into the
proposed layout, or materially rewritten.

### Size review threshold

The following are review thresholds, not incentives to compress code:

- about 100 non-generated Go source lines per fixture;
- about 40 meaningful positive IR assertions per fixture;
- one primary lowering owner;
- no full-function autogenerated snapshot when a short ordered set of CHECKs
  proves the contract.

Crossing a threshold requires either splitting the fixture or explaining why
the assertions form one inseparable ABI matrix.

### Prefer direct evidence

Check the runtime helper, LLVM operation, ABI attribute, symbol name, metadata
edge, or target dispatch that defines the contract. Do not call an unrelated
library merely to make a value printable.

### No external thin-wrapper dependency

C, C++, CGo, and LLGo intrinsic fixtures declare the required interfaces in
the fixture or a local declaration-only subpackage. Python tests use a
test-harness supplied synthetic root package and local
`LLGoPackage = "py.<module>"` declarations.

### Respect the CI-selected Go version

Fixtures must not install or select another Go toolchain. Version differences
use the existing versioned-golden mechanism or build constraints supported by
all CI versions. A fixture-local `go.mod` may describe module/package
boundaries only; it must not cause `go` toolchain auto-selection or bypass the
`GOROOT` used to launch the test suite.

## Proposed layout

The source-to-IR suites use recursive leaf-fixture discovery. Scope is the
first path component and capability is the second where a suite has capability
families. The test name is the relative leaf path, which keeps both dimensions
visible without requiring giant flat names.

```text
cl/
  _testcodegen/                 # 79 fixture leaves
    common/                     # Scope: common
      <capability>/<fixture>
    os/                         # Scope: os (...)
      <capability>/<fixture>
    arch/                       # Scope: arch (...)
      <capability>/<fixture>
    os-arch/                    # Scope: os+arch (...)
      <capability>/<fixture>
  _testintrinsics/              # 10
    <scope>/<fixture>
  _testdebug/                   # 3
    <scope>/<fixture>
  _testdrop/                    # 17
  _testlto/                     # 24
  _testmeta/                    # 12
  blocks/
    block_test.go               # 6 direct CFG table rows
```

The legacy normal suites are removed after their owners move:

- `_testdata`
- `_testgo`
- `_testlibc`
- `_testlibgo`
- `_testpy`
- `_testrt`
- `_testdefer`

Here `<scope>` is `common`, `os`, `arch`, or `os-arch`; the source comment uses
the corresponding `common`, `os (...)`, `arch (...)`, or `os+arch (...)`
label. The capability component under each `_testcodegen` scope is one of
`scalar`, `abi`, `interface`, `closure`, `defer`, `map`, `data`,
`concurrency`, `generics`, `reflect`, or `foreign`. The 79-leaf total is
budgeted by capability below; scope changes the path and prefix policy, not
the number of owners.

## Proposed normal fixture budget

| Capability | Fixtures |
| --- | ---: |
| Debug types, parameters, and lexical scopes | 3 |
| Scalar operations, control flow, and conversions | 10 |
| Package linkage, ABI, and layout | 11 |
| Interface, assertion, and equality | 9 |
| Closure and function values | 6 |
| Defer and panic code generation | 7 |
| Map lowering | 4 |
| Slice, string, array, and builtin lowering | 7 |
| Channel, goroutine, and select lowering | 3 |
| Generic instantiation and ABI | 7 |
| Reflect compiler bridges | 5 |
| LLGo intrinsics | 10 |
| C, CGo, C++, asm, and Python | 10 |
| **Total** | **92** |

The foreign-language budget is deliberately larger than a single fixture per
language so that complex paths remain small:

| Family | Fixtures | Contracts |
| --- | ---: | --- |
| C | 2 | direct/vararg/global calls; aggregate/complex ABI |
| CGo | 3 | wrapper/conversion; source/aggregate/errno; callback/defer |
| C++ | 1 | real `.cpp` compile, link, and `extern "C"` bridge |
| asm | 1 | no-argument, input, and output constraint variants in short functions |
| Python | 3 | module binding; call forms; values and containers |

## Defer organization

### Block analysis

`cl/_testdefer` is not a code-generation suite. It parses Go source and checks
the output of `cl/blocks.Infos`. Its seven fixture directories become six
direct table-driven graphs:

1. single-exit diamond;
2. multiple exits;
3. natural loop;
4. multi-node SCC with conditional exits;
5. self-loop, where loop classification wins over entry-always;
6. disconnected recover/shared subgraph.

The 737-line `gobuild` input and the non-defer `print` stress input are removed.
The existing self-loop and shared-subgraph unit tests join the same matrix.

### Code generation

The current defer-owned fixtures are reduced to seven focused leaves:

| New owner | Contract |
| --- | --- |
| `defer/kinds` | Always, conditional, and loop registration plus evaluation at the defer statement |
| `defer/closure` | saved code/environment pair and target closure ABI |
| `defer/closure-arg` | callable closure environment is distinct from closure-valued arguments and receivers |
| `defer/interface` | save itab slot and receiver at registration; invoke the saved pair during cleanup |
| `defer/recover` | recover-frame start/bind/end and activation token, including a negative non-recoverable callee |
| `defer/rethrow` | drain/free, restore the previous frame, rethrow, and terminate the block |
| `defer/named-result` | deferred closure updates the stable result slot before the final load/return |

Native/wasm continuation dispatch, Windows setjmp ABI, loop drainers,
range-over-func explicit defer stacks, and recover activation aliases already
have stronger `ssa` or source-to-IR unit tests. They do not need duplicate
full-function FileCheck fixtures.

## Standard-library and external-library ownership

`_testlibgo` is removed:

- atomic lowering moves to `_testintrinsics`;
- the map zero-result regression moves to the map suite;
- the unique compiler part of `deferpanic` moves to the defer suite;
- ordinary calls to bytes, errors, math, math/bits, net/textproto, os,
  strings, sync, and sync.WaitGroup remain owned by `test/std`.

`goplus/lib` imports are replaced as follows:

- LLGo intrinsics: local `//go:linkname ... llgo.*` declarations;
- C functions and globals: local declaration-only packages;
- CGo: local preambles and local C sources;
- C++: one real local `.cpp` file and an `extern "C"` bridge;
- Python: a synthetic root types package supplied by the test harness and
  local module declarations.

External integration such as sqlite, pthread wrapper behavior, LLVM demangle,
or Python application demos is not a compiler fixture contract.

## Missing Go lowering coverage

All approximately 36 Go SSA instruction families handled by `cl/compile.go`
have at least one fixture or source-to-IR unit test. The missing coverage is in
subpaths, not whole instruction classes.

Three new physical fixtures are proposed.

### `scalar/arith-divrem`

Use four short functions to check:

- signed quotient and remainder;
- unsigned quotient and remainder;
- divisor-zero comparison and `AssertDivideByZero`;
- a selected safe non-zero divisor before LLVM div/rem;
- the signed `minInt / -1` and `minInt % -1` Go-defined results;
- `sdiv`, `srem`, `udiv`, and `urem`.

### `scalar/builtin-minmax`

Use separate short integer, floating-point, and string functions. The current
`compareSelect` implementation uses an ordered comparison followed by select.
For floating point this does not preserve Go semantics for NaN and signed
zero. This is a compiler bug discovered by the audit, not only a missing test.
The compiler fix and focused regression test should land separately from the
mechanical fixture migration. The string function directly owns the
`StringLess` helper path and checks both `min` and `max` selection.

### `foreign/cpp-abi`

Compile and link a minimal real `.cpp` helper through an `extern "C"` bridge.
The current demangle fixture calls a Go wrapper and does not test the C++
source pipeline.

Existing owners are strengthened without adding directories:

- add an explicit `StringLess` check to the string comparison fixture;
- add `NewSlice3Bounds` to the bounds owner;
- relate the goroutine allocation root to `NewProc` in the goroutine owner.

## Missing LLGo-specific coverage

`llgoInstrs` contains 51 mappings:

- 34 currently have acceptance coverage;
- 4 have stronger source-to-IR/unit coverage and may remain unit-only:
  `boolToUint8`, `closureEnv`, Windows `setjmp`, and Windows `longjmp`;
- 13 lack an independent assertion.

The 13 gaps are:

- `allocCStr`;
- `stringData`;
- `pystr`;
- raw atomic xchg, and, nand, or, xor, max, min, umax, and umin;
- elimination of `_cgoCheckPointer`.

They are assigned to four small owners without adding more directories than
the ten-fixture intrinsic budget:

- `intrinsics/cstring`;
- `intrinsics/atomic-raw`;
- `foreign/python/values`;
- `foreign/cgo/check-pointer-noop`.

After migration, 47 mappings have acceptance coverage and four retain stronger
unit-only coverage: 51 of 51 have an explicit owner.

Of the 77 statically selected runtime helpers, `AssertDivideByZero` currently
has no test and `StringLess` has only indirect runtime-output coverage. The
new and strengthened fixtures give both helpers a direct owner.

## Special suites

### DeadcodeDrop

All 17 `_testdrop` cases remain. They cover distinct roots or propagation
edges: direct function/method roots, exported and unexported method identity,
interface slots and matching, cross-package interface flow, generic demand,
promoted wrappers, C export callbacks, reflect roots, and Source64 flow.

These cases may be shortened, but they are not merged solely because the final
IR contains similar calls.

### LTO

`_testlto` changes from 30 to 24:

- merge `globaldce_interface_slots` into the interface matrix;
- reduce the ten finite MethodByName string-flow variants to five owners:
  helper forwarding, string transformation, global/aggregate source, CFG
  finite-set propagation, and aggregate string ABI;
- retain the explicit unknown-name negative case even though its directory
  begins with an underscore.

Static-itab full and partial roots, type-id DCE, unexported identity,
metadata-only reflect roots, ABI fake-use, runtime integration, and pcline
remain separate.

### Package metadata

All 12 `_testmeta` capabilities remain because they emit different metadata
sections or identities. `interface_exported_var` and `methodinfo_imported`
replace standard-library packages with small local cross-package fixtures.

## Before and after counts

The baseline counts leaf fixture directories. The hidden
`_testlto/_globaldce_reflect_method_by_name_ltoplugin_string_abi_unknown` case
is included because the harness invokes it explicitly.

| Test layer | Before | After |
| --- | ---: | ---: |
| Normal compile/run/FileCheck suites | 199 | 92 |
| Block-classification fixture directories | 7 | 0 |
| Direct block-classification table rows | 0 | 6 |
| DeadcodeDrop | 17 | 17 |
| LTO | 30 | 24 |
| Package metadata | 12 | 12 |
| **Fixture directories** | **265** | **145** |
| **Logical scenarios** | **265** | **151** |

The fixture-directory reduction is 120, or about 45%. The logical-scenario
reduction is 114, or about 43%.

## Staged migration

### Phase 1: harness and ownership

1. Add recursive leaf discovery for `_testcodegen`, `_testintrinsics`, and
   `_testdebug`.
2. Preserve relative-path test names and normalize every migrated fixture into
   the `common`, `os`, `arch`, or `os+arch` scope and prefix rules.
3. Require a scope declaration for every new, moved, or materially rewritten
   `LITTEST`; do not mechanically annotate untouched legacy fixtures.
4. Add a test-only Python package injection mechanism.
5. Record a machine-readable or test-generated ownership inventory for LLVM
   operations, runtime helpers, intrinsics, ABI attributes, and target
   prefixes.

### Phase 2: exact duplicates and misplaced tests

1. Remove the byte-identical wasm defer fixture.
2. Merge the strict-subset asm, C-call, map, callback, and reflect fixtures.
3. Move test-package selection to `internal/build`.
4. Move standard-library behavior to `test/std` and runtime semantics to
   `test/go`.
5. Replace external thin wrappers with local declarations.

### Phase 3: split large fixtures

Split debug, method ABI, builtin, reflect, defer, CGo, and Python fixtures into
the proposed owners. Delete the large demo programs only after every unique
owner assertion exists.

### Phase 4: fill gaps

1. Add `arith-divrem`.
2. Land the min/max compiler fix and `builtin-minmax` as a separate semantic
   change.
3. Add the real C++ fixture.
4. Complete the 51-entry LLGo intrinsic ownership matrix.

### Phase 5: special suites

Convert block analysis to direct graph tests, consolidate the LTO finite-name
matrix, and replace standard-library metadata inputs with local packages.

Each phase must be independently reviewable and leave all suites passing. A
phase may increase the target count if it discovers a genuinely independent
lowering or ABI rule; it must not hide that rule to preserve the numeric
target.

## Validation and CI

At minimum, each migration phase runs:

```sh
go test ./cl/...
go run ./chore/litgen -u --check cl
go test ./...
```

CI must run the repository-supported Go versions and targets. The migration
must not introduce a nested toolchain selection that bypasses the Go version
chosen by the workflow.

Validation compares capability ownership before and after, not only pass/fail:

- Go SSA instruction families;
- LLVM opcode families;
- runtime-helper calls;
- LLGo intrinsic mappings;
- ABI attributes and calling conventions;
- declared common/OS/arch/OS+arch scope and prefix closure;
- metadata sections and edges;
- DeadcodeDrop and LTO roots;
- block classification and ordering.

For moved runtime or standard-library semantics, the destination test must
exist and run in CI before the compiler fixture is removed.

## Risks and mitigations

### A broad fixture may contain a hidden unique regression

Before removing it, compare its positive and negative assertions against the
new owner matrix and inspect the change history. Preserve the smallest source
construct that reproduces the regression.

### Short CHECKs may become too weak

Each shortened fixture checks the defining helper/opcode/ABI relationship and
at least one negative condition where accidental fallback is plausible.

### Recursive discovery may change ignore behavior

Use relative leaf paths as stable test IDs and convert every embedded-target
ignore entry before deleting the old runner.

### Multi-version Go output may differ

Prefer semantic regexes and versioned goldens over broad snapshots. Exercise
every supported Go version in CI before removing the old fixture.

### Removing external packages may stop exercising package classification

Local declaration packages must retain the relevant package kind, linkname,
`LLGoPackage`, CGo preamble, and source-file shape. Tests validate the
classification directly rather than relying on an external wrapper.

## Decision requested

Accept the ownership model, proposed layout, and 145-directory/151-scenario
target as the migration baseline. Implementation should proceed in staged
pull requests, with the min/max semantic fix kept separate from mechanical
fixture consolidation.
