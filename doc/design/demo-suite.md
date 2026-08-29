# Demo and integration suite organization

Status: proposal with a focused manifest, runner, and consolidation implementation.
Baseline: xgo-dev/main@c1d5da2 (2026-08-29).

## Summary

`_demo` currently serves three different purposes: copyable examples, compiler
and runtime regressions, and target or external-toolchain integration tests.
Those purposes need different review and validation contracts, but today one
shell script discovers most cases from directory shape and treats a zero exit
status as success.

This proposal keeps the useful coverage while making every case explicit:

- user examples stay small, documented, and copyable;
- regressions retain the smallest construct that historically failed and move
  to the test layer that owns that behavior;
- C, C++, CGo, Python, linker, and target smokes remain integration cases;
- embedded cases use positive target lists instead of two large exclusion
  lists;
- every printed result is either checked exactly or replaced by a deterministic
  in-program assertion;
- support packages are declared, so they are never executed as cases;
- heavy or interactive examples run in dedicated scheduled/manual profiles.

The baseline contains 122 logical case families: 74 Go, 28 C/foreign, seven
Python, and 13 embedded executables. The implemented ownership map contains
43: 17 Go, 16 C/foreign, two Python, and eight embedded. The reduction is 79
families, or about 65%. This is an ownership reduction, not a coverage
deletion: small cases in the same capability family now execute in one bounded
owner, while unique historical inputs remain direct assertions.

The complete current-case decision table is in
[demo-suite-inventory.md](demo-suite-inventory.md).

### Implemented consolidation

The manifest now accounts for every remaining Go-source directory: 33 runnable
cases, seven support directories, and 11 workflow-owned directories, for 51
physical source packages. The Linux/macOS host plan contains 32 cases and the
Windows host plan 31; full LTO, DeadcodeDrop, ESP32, ESP32-C3, and model plans
contain eight, six, five, six, and one case respectively.

Each merged owner's target profiles are the intersection of its retained
subcases' known-good profiles. `cabi`, `cppintf`, and `fcntl/getcwd` are therefore
host-only; `core-regressions` and `issue1538` keep ESP32-C3 but exclude their
historically failing ESP32 paths. A target no-op is not counted as coverage.

Historical regressions were not made conditional on another pull request.
The consolidated owners retain the exact #1608 return values, #1389
`build.Default`/importer path, #1598 embedded `types.Func`, #1559 unexported
method identity, #1538 conversion boundaries, and #1654 file/open inputs.
The dormant 1,162-line embedded libc and 385-line compiler-rt programs were
removed only after the active serial, GC, target-build, float, and export
workflows were kept as their explicit target owners.

## Current behavior

### Discovery is accidental

The host runner examines only first-level directories under `_demo/go`,
`_demo/py`, and `_demo/c` that directly contain a Go file. It finds 107
directories (73 Go, seven Python, and 27 C). Consequently:

- the stacktrace expected-failure and socket client/server programs require
  dedicated workflows rather than ordinary host discovery;
- adding or moving a Go file silently changes CI membership;
- helper packages have no explicit ownership.

Embedded discovery does the opposite: it recursively finds all Go-source
directories under `_demo/go` and `_demo/c`, then removes entries through
target-specific exclusion arrays. It currently discovers 111 directories,
ignores 91 for ESP32 and 83 for ESP32-C3, and runs only 20 and 28 respectively.
This also executes `cppintf/foo` and `cppmintf/foo` as if they were independent
programs even though they are support packages.

### Exit status backed by focused assertions

The old generic runner accepted any successful process, including demos that
only printed an unchecked value. Retained value-oriented cases now assert
their results in the program and exit nonzero on mismatch. A small number of
foreign/linker owners intentionally use successful compile/link/run as their
contract; their rationale is recorded in the inventory rather than encoded in
the runtime manifest.

The runner therefore needs only the command exit status. Unused generic
golden-output and expected-failure machinery would add schema, I/O, and tests
without covering a current case. Expected-failure and multi-process programs
remain explicitly workflow-owned. Increasing sleep time is not an acceptable
way to make a test deterministic.

### CI repeats work without declaring intent

The host set runs in five Go/OS lanes, then again under full LTO/GlobalDCE and
DeadcodeDrop. Python setup installs NumPy and PyTorch in every host lane; the
consolidated scientific owner therefore checks both integrations rather than
leaving either dependency unused. Embedded target coverage is hidden in the
legacy exclusion lists rather than stated as a capability matrix.

The runner must use the Go version selected by CI. It must not inspect or edit
a case `go.mod` to select another toolchain, set a different `GOROOT`, or permit
automatic toolchain downloads. Both runner and child commands use
`GOTOOLCHAIN=local`; module reads use readonly mode.

## Ownership model

Every generic runnable case has exactly one primary class in this design
inventory. The taxonomy is review documentation; only positive profiles and
GOOS values select what CI executes. Specialized embedded programs remain
workflow-owned instead of pretending to be generic runnable cases.

| Class | Purpose | Typical location |
| --- | --- | --- |
| `example` | documented, copyable user workflow | `_demo` |
| `regression` | minimum reproducer for a past compiler/runtime failure | `test/go`, `cl`, or focused integration case |
| `stdlib` | public standard-library behavior | `test/std/<pkg>` |
| `integration` | foreign source, linker, package, process, or ABI pipeline | explicit integration case |
| `optional` | heavy, networked, interactive, or multi-process example | scheduled/manual profile |

A case is retained because its primary contract is independent, not because
it happens to call a different standard-library symbol. Conversely, cases
that previously caught regressions are not deleted merely because a lower
layer now looks similar; the destination must reproduce the failure mode.

## Explicit manifest and runner

Add `_demo/manifest.json` with a versioned, strictly decoded schema. JSON keeps
the runner in the Go standard library and avoids another CI parser dependency.

The runtime manifest records only data used to plan or audit execution:

- stable ID and working directory;
- positive profiles, rather than target exclusions;
- supported `goos` values;
- support directories that contain Go sources but are not programs;
- a workflow owner for specialized scripts that remain outside the generic
  runner.

Capability, class, history, dependency setup, and successful-exit rationale
belong in this design and the inventory. Keeping them out of the runtime JSON
prevents descriptive metadata from expanding the CI parser and validator.

Profiles initially include `host`, `host-lto`, `host-deadcodedrop`, `esp32`,
`esp32c3-basic`, and `model`. LTO and DeadcodeDrop arguments are literal arrays
in the profile; the runner does not shell-split an environment variable.

The repository audit scans every Go-source directory under `_demo/{go,c,py,
embed,workflow}`. Each directory must appear exactly once as a runnable directory, a
support directory, or a workflow-owned directory. Unknown JSON fields,
duplicate IDs or paths, escaping paths, invalid profile/GOOS references, and
cases without a positive profile fail validation.

The proposed command is:

```sh
go run -mod=readonly ./chore/demorun --profile host --result result.md
```

It executes `llgo` directly with an isolated working directory and log,
`GOWORK=off`, `GOTOOLCHAIN=local`, one uniform per-case timeout, and stable
manifest-order failure reporting. For ordinary host cases it passes the
selected C, Go, and Python directories to one `llgo build -o <directory>`
invocation per source group. The explicit directory list, rather than
`./...`, preserves every profile and GOOS exclusion. Produced executables run
in a fixed worker pool which defaults to at most four concurrent cases
(bounded by `runtime.NumCPU`); `--jobs` remains an explicit override. The same
budget is shared across concurrent group builds, so the default host plan
assigns two package workers to the larger Go group and one each to C and
Python, instead of starting three separate four-worker compilers. A failed
group build is reported, but any executables it did produce still run, so an
independent package failure does not hide other results. Embedded and model
profiles retain their specialized per-case invocation.

The runner selects platforms through `runtime.GOOS`, not the GitHub-specific
`RUNNER_OS` spelling. The shell entry point passes `--jobs` only when
`LLGO_DEMO_JOBS` is explicitly set, so CI uses the bounded parallel default
instead of silently forcing one worker.

The production planning/execution core is 759 lines after the concurrent
batch-build integration, down from 794; including the small command adapter it
is 904 lines, down from 930. That reduction removes the unused generic output/failure
check framework and per-case timeout schema while retaining strict ownership
validation, filtering, stable reporting, shared builds, and cross-platform
scheduling. On an Apple M4 Max, an eight-case fake-command benchmark (10 ms
per case, five iterations) measured 94.4 ms with `--jobs=1` and 23.7 ms with
`--jobs=4`, about 4.0x faster. The planned CI binary execution count is reduced
from about 960 to 229. More importantly, shared host builds reduce the planned
`llgo` compiler invocations from 229 to about 75: three source groups in each
of five host lanes, 55 specialized embedded invocations, two LTO groups, two
DeadcodeDrop groups, and one model invocation.

## Implemented case budget

| Family | Before | After | Main change |
| --- | ---: | ---: | --- |
| Go | 74 | 17 | bounded core/tooling/stdlib owners plus one split-file reflect owner |
| C/C++/CGo/asm | 28 | 16 | preserve broad foreign ecosystems while merging duplicate executables |
| Python | 7 | 2 | one complete base-binding owner and one NumPy/PyTorch scientific owner |
| Embedded executables | 13 | 8 | retain every active specialized regression; remove dormant monoliths |
| **Logical case families** | **122** | **43** | **79 fewer (about 65%)** |

The 43-owner result is capability-preserving. Foreign wrapper calls that add a
distinct ecosystem boundary remain, but execute inside a related owner. Pure
duplicate command packages and dormant, unowned target monoliths do not.

### Go: 74 to 17

Fifteen non-reflect runnable owners remain, plus the workflow-owned expected
stacktrace failure:

```text
abimethod  core-regressions  export  tooling  stdlib-ecosystem
fileio  createtemp-1654  sysopen-1654  process  concurrency  timer
syscallraw  linkname  issue1538  ifaceconv
```

Reflection has one runnable owner so CI compiles and starts one package:

```text
reflect
```

Its source is split by function calls, dynamic type construction, method ABI,
conversion, pointer metadata, and value operations. It retains every
`reflectmake` and `reflectmethod` subtest that their original `main`
functions actually invoked, while excluding only their commented-out dead
subtests. The complete small/word/large receiver,
value/pointer/interface/embedded receiver, and scalar/small-aggregate return
ABI matrices therefore remain covered. Additional assertions retain
MakeFunc/FuncOf/New/NewAt, single/two/nested closure returns, named and C
function storage, empty five-input/four-output ABI slots, signaling-NaN and
slice/array conversion, all Copy directions and panic paths, struct Indirect,
promoted pointer mutation, VisibleFields/CanInterface, FieldByIndex, and
version-gated SliceAt paths.

Standard-library-only programs move to `test/std`; compiler instruction and
ABI cases move to `cl` or `test/go`. `go/export` remains a specialized suite
because it validates generated headers, C consumers, archive/shared modes,
callbacks, aggregate values, and runtime hooks and has repeatedly caught
integration failures. Its source should be split by ABI family, not combined
with unrelated demos.

The filesystem cluster becomes one bounded `fileio` case plus the independent
concurrent-temp and raw-open regressions. The process cluster becomes one
cross-platform case with controlled same-executable children for C `execlp`
and Unix Go `syscall.Exec`. Concurrency uses channel barriers rather than sleeps.
Timer tests use deterministic synchronization and generous deadlines rather
than printed timestamps or longer fixed delays.

### C and foreign: 28 to 16

Fifteen runnable owners plus the manual socket family remain:

```text
asmfullcall  cabi  cargs  catomic  cgofull  cppintf  ctime  fcntl
hello  netdbdemo  qsort  setjmp  stacksave  thread  llama2-c  socket
```

`qsort` remains a callback/reentry smoke. `llama2-c` remains in the
scheduled/manual model profile. `socket` remains a manual integration case
until a dedicated harness chooses a temporary port, starts the server, waits
for readiness, runs the client, applies timeouts, and validates the message.

Programs that only belong to an application-specific workflow live under
`_demo/workflow`: `model/llama2-c`, `socket/{client,server}`, and
`expected-failure/stacktrace`. This keeps `_demo/c` and `_demo/go` free of
packages that ordinary recursive builds must exclude.

`asmcall`, `helloc`, and `cppmintf` contribute their unique small subcases to
`asmfullcall`, `hello`, and `cppintf`. The large `cabi` matrix is reduced to one
representative per register, sret, callback, and export ABI family. Thin
`goplus/lib` wrapper demos move to that library's tests or are replaced by
local declarations at the compiler-fixture layer. The only distinct `go/cgo`
path, errno's two-result form, runs in `cgofull` beside its aggregate pointer,
callback, export, macro, and source-file paths.

### Python: seven to two

Retain two independent contracts. `basic` combines module values, fixed and
variadic calls, iterator expansion, scalar extraction, Python `Print`, and the
C variadic bridge, including `statistics.Mean`. `matrix` remains the sole
third-party scientific owner and validates both NumPy list/object-method
conversion and PyTorch `Tensor` conversion with stable extracted values.

### Embedded: 13 to eight

Eight existing automated regressions remain:

- renamed-symbol export verification;
- target-build `empty` and `defer` baselines;
- serial `chello`, `int64slice`, and `gc-runtime` cases;
- ESP32-C3 `float-1664` and `print-float-1723` regressions.

The 1,162-line dormant libc demo and 385-line compiler-rt demo are not kept as
monoliths. Active serial C, GC/runtime, target-build, float, and export
workflows plus the five-case ESP32 and six-case ESP32-C3 positive profiles provide the
same target/toolchain boundaries with checked outcomes. Dormant hello/write
duplicates are removed in favor of the canonical serial C hello.

`gc-runtime` has strong and recent regression history. It remains protected,
but should later be split into root-liveness, graph-marking, and allocator
stats cases; those are related but too large for a single maintainable source
file.

## History-sensitive cases

The following groups have repeatedly failed during implementation and must
retain direct owners:

- `go/export`, Go reflect construction/method/function bridges, raw syscall,
  interface promotion, float/integer conversion, concurrent temp creation,
  raw open, package linkname, and CGo errno/aggregate paths;
- C ABI sret/callback/export, C atomics, C++ interface thunks and multiple
  inheritance, setjmp, stack-save, pthread locality, fcntl, netdb, and socket;
- embedded startup, serial output discovery, int64 slice bounds, float output,
  target defer builds, generated export names, and GC root/allocator behavior.

When a case moves, preserve the relevant platform, build mode, target, and
assertion. Merely copying a source expression into a native unit test does not
replace a target or linker regression.

## Staged implementation

### Phase 1: equivalent manifest migration

1. Add strict manifest parsing, planning, owner-audit, and worker-pool tests.
2. Record the current runnable, support, and specialized-workflow directories.
3. Replace host globbing and embedded exclusions with positive profiles while
   preserving the actual current run set.
4. Keep the shell script only as a thin profile adapter.

### Phase 2: make evidence deterministic

1. Convert stable value output to in-program assertions.
2. Convert variable output and historical regressions to self-checking code.
3. Keep the stacktrace case under explicit expected-failure workflow ownership.
4. Add a dedicated socket harness or keep it explicitly manual.

### Phase 3: move and consolidate

1. Land destination tests before removing Go stdlib, compiler-lowering, or
   target regressions.
2. Consolidate the named Go, reflect, C, and Python groups within the size
   limits below.
3. Keep NumPy and PyTorch setup aligned with the merged scientific owner.
4. Move embedded regressions to `test/embed` and replace the dormant large
   suites with focused cases only if target-specific gaps remain.

### Phase 4: optimize CI profiles

Run documented examples once on the primary Ubuntu lane unless their contract
is platform-specific. Run compiler/link regressions in the supported OS and Go
version matrix. Run only cases whose contract is affected by LTO or
DeadcodeDrop under those profiles. Keep model and multi-process tests in their
dedicated profiles.

## Size and review rules

- one primary capability per case;
- about 100 non-generated source lines as a review threshold;
- related ABI variants may use several short functions, but no Cartesian
  product merely to increase apparent coverage;
- split a case when setup or failure diagnosis obscures the named capability;
- no external thin wrapper when a local declaration expresses the contract;
- no standard-library smoke in `_demo` solely because it prints a value;
- no output-only regression without a deterministic self-check or a documented
  compile/link/run integration rationale.

## Validation

The runner implementation must pass:

```sh
GOTOOLCHAIN=local go test ./internal/demotest ./chore/demorun
GOTOOLCHAIN=local go run -mod=readonly ./chore/demorun --check-manifest
go test ./...
```

CI then exercises all declared profiles on their supported platforms and both
supported Go versions. A migration phase compares the manifest's capability
owners and actual planned cases before and after, not only the number of
directories or successful processes.

## Other audit finding fixed here

`dev/local_ci.sh` previously invoked `_demo/embed/targetsbuild/build.sh`
without its required test-directory argument. The implementation made local CI run
both `empty` and `defer` explicitly, matching the GitHub workflow.

## Result

The explicit ownership/execution model is implemented with 43 logical owners and
33 generic runnable cases. Follow-up reviews should compare capability and
history mappings first, then package/profile counts and CI duration; raw file
count alone is not the acceptance criterion.
