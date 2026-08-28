# Demo and integration suite organization

Status: proposal. Baseline: xgo-dev/main@c1d5da2 (2026-08-29).

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
Python, and 13 embedded executables. The proposed ownership map contains 51:
23 Go, 13 C/foreign, four Python, and 11 embedded. The reduction is 71 cases,
or about 58%. This is an ownership reduction, not a coverage deletion: unique
historical regressions must exist at their destination before the old demo is
removed.

The complete current-case decision table is in
[demo-suite-inventory.md](demo-suite-inventory.md).

## Current behavior

### Discovery is accidental

The host runner examines only first-level directories under `_demo/go`,
`_demo/py`, and `_demo/c` that directly contain a Go file. It finds 107
directories (73 Go, seven Python, and 27 C). Consequently:

- `_demo/go/failed/stacktrace` is never run on the host;
- `_demo/c/socket/client` and `server` are never run on the host;
- adding or moving a Go file silently changes CI membership;
- helper packages have no explicit ownership.

Embedded discovery does the opposite: it recursively finds all Go-source
directories under `_demo/go` and `_demo/c`, then removes entries through
target-specific exclusion arrays. It currently discovers 111 directories,
ignores 91 for ESP32 and 83 for ESP32-C3, and runs only 20 and 28 respectively.
This also executes `cppintf/foo` and `cppmintf/foo` as if they were independent
programs even though they are support packages.

### Exit zero is too weak

The generic runner checks only whether `llgo run .` exits successfully. A demo
that prints an incorrect value still passes. This is particularly weak for
historical conversion, reflect, filesystem, timer, C ABI, and Python
regressions whose only evidence is output.

Four check contracts are proposed:

| Check | Contract |
| --- | --- |
| `self` | The program deterministically asserts its result and exits nonzero on mismatch. |
| `stdout` | Standard output exactly matches a checked-in golden; only CRLF is normalized. |
| `exit` | Successful compile/link/run is the capability; a rationale is mandatory. |
| `failure` | Nonzero exit is expected and stable stderr fragments are checked in order. |

`self` is preferred for time, randomness, files, maps, platform variation, and
any result that should not require per-platform goldens. Increasing sleep time
is not an acceptable way to make a test deterministic.

### CI repeats work without declaring intent

The host set runs in five Go/OS lanes, then again under full LTO/GlobalDCE and
DeadcodeDrop. Python setup installs NumPy and PyTorch in every host lane even
though a single tensor demo is the only reason for PyTorch. Embedded target
coverage is hidden in the exclusion lists rather than stated as a capability
matrix.

The runner must use the Go version selected by CI. It must not inspect or edit
a case `go.mod` to select another toolchain, set a different `GOROOT`, or permit
automatic toolchain downloads. Both runner and child commands use
`GOTOOLCHAIN=local`; module reads use readonly mode.

## Ownership model

Every retained case has exactly one primary class.

| Class | Purpose | Typical location |
| --- | --- | --- |
| `example` | documented, copyable user workflow | `_demo` |
| `regression` | minimum reproducer for a past compiler/runtime failure | `test/go`, `cl`, or focused integration case |
| `stdlib` | public standard-library behavior | `test/std/<pkg>` |
| `integration` | foreign source, linker, package, process, or ABI pipeline | explicit integration case |
| `target-smoke` | target startup, serial output, compiler-rt/libc link surface | `test/embed` |
| `optional` | heavy, networked, interactive, or multi-process example | scheduled/manual profile |

A case is retained because its primary contract is independent, not because
it happens to call a different standard-library symbol. Conversely, cases
that previously caught regressions are not deleted merely because a lower
layer now looks similar; the destination must reproduce the failure mode.

## Explicit manifest and runner

Add `_demo/manifest.json` with a versioned, strictly decoded schema. JSON keeps
the runner in the Go standard library and avoids another CI parser dependency.

Each case records:

- stable ID and working directory;
- class and primary capability;
- positive profiles, rather than target exclusions;
- supported `goos` values;
- support directories that contain Go sources but are not programs;
- dependency/setup metadata;
- one of the four check contracts;
- issue or history notes for regression-sensitive cases;
- a workflow owner for specialized scripts that remain outside the generic
  runner.

Profiles initially include `host`, `host-lto`, `host-deadcodedrop`, `esp32`,
`esp32c3-basic`, and `model`. LTO and DeadcodeDrop arguments are literal arrays
in the profile; the runner does not shell-split an environment variable.

The repository audit scans every Go-source directory under `_demo/{go,c,py,
embed}`. Each directory must appear exactly once as a runnable directory, a
support directory, or a workflow-owned directory. Unknown JSON fields,
duplicate IDs or paths, escaping paths, missing goldens, and cases without a
profile fail validation.

The proposed command is:

```sh
go run -mod=readonly ./chore/demorun --profile host --jobs 4 --result result.md
```

It executes `llgo` directly with an isolated working directory and log,
`GOWORK=off`, `GOTOOLCHAIN=local`, a per-case timeout, and stable manifest-order
failure reporting. It selects platforms through `runtime.GOOS`, not the
GitHub-specific `RUNNER_OS` spelling. The existing shell entry point can remain
as a thin compatibility wrapper during migration.

## Proposed case budget

| Family | Before | After | Main change |
| --- | ---: | ---: | --- |
| Go | 74 | 23 | consolidate integration regressions; move stdlib/lowering owners |
| C/C++/CGo/asm | 28 | 13 | retain independent ABI/link smokes; remove thin-wrapper demos |
| Python | 7 | 4 | core calls/values, Python-to-C bridge, and one NumPy integration |
| Embedded executables | 13 | 11 | retain eight active regressions; replace two large dormant suites with three focused smokes |
| **Logical case families** | **122** | **51** | **71 fewer (about 58%)** |

The 51 target is intentionally capability-preserving. If an explicit audit
shows the three proposed embedded libc/compiler-rt smokes are already covered
on the same targets, the embedded total can later fall from 11 to eight.

### Go: 74 to 23

Thirteen non-reflect owners remain:

```text
abimethod  cgo  export  fileio  createtemp-1654  sysopen-1654
process  concurrency  timer  syscallraw  linkname  issue1538  ifaceconv
```

Ten reflect owners remain small rather than becoming one large matrix:

```text
reflectcallfn  reflectfunc  reflectnamedfn  reflectmakefn  reflectmethod
reflectconv  reflectmake  reflectstructof  reflectpointerto  reflectvalue
```

Standard-library-only programs move to `test/std`; compiler instruction and
ABI cases move to `cl` or `test/go`. `go/export` remains a specialized suite
because it validates generated headers, C consumers, archive/shared modes,
callbacks, aggregate values, and runtime hooks and has repeatedly caught
integration failures. Its source should be split by ABI family, not combined
with unrelated demos.

The filesystem cluster becomes one bounded `fileio` case plus the independent
concurrent-temp and raw-open regressions. The process cluster becomes one
cross-platform case. Concurrency uses channel barriers rather than sleeps.
Timer tests use deterministic synchronization and generous deadlines rather
than printed timestamps or longer fixed delays.

### C and foreign: 28 to 13

Nine core PR integration owners remain:

```text
asmfullcall  cabi  cargs  cppintf  fcntl  hello
netdbdemo  stacksave  thread
```

`concat` and `qsort` remain documented Ubuntu smokes. `llama2-c` remains in
the scheduled/manual model profile. `socket` remains a manual integration case
until a dedicated harness chooses a temporary port, starts the server, waits
for readiness, runs the client, applies timeouts, and validates the message.

`asmcall`, `helloc`, and `cppmintf` contribute their unique small subcases to
`asmfullcall`, `hello`, and `cppintf`. The large `cabi` matrix is reduced to one
representative per register, sret, callback, and export ABI family. Thin
`goplus/lib` wrapper demos move to that library's tests or are replaced by
local declarations at the compiler-fixture layer.

### Python: seven to four

Retain four independent contracts:

- `callpy`: module import and fixed-arity scalar call;
- `max`: variadic Python call plus list/tuple iteration;
- `pi`: Python value extraction followed by a C variadic call;
- `matrix`: nested-list conversion and the sole third-party NumPy extension.

`print` is a subset of `callpy`, `statistics` duplicates the call/value bridge,
and `tensor` duplicates the container/extension smoke while forcing PyTorch
installation in every lane. Removing `tensor` removes the PyTorch dependency.

### Embedded: 13 to 11

Eight existing automated regressions remain:

- renamed-symbol export verification;
- target-build `empty` and `defer` baselines;
- serial `chello`, `int64slice`, and `gc-runtime` cases;
- ESP32-C3 `float-1664` and `print-float-1723` regressions.

The 1,162-line dormant libc demo and 385-line compiler-rt demo are not kept as
monoliths. If their target-link coverage is not already owned elsewhere, they
become three self-checking cases: one libc smoke, one integer compiler-rt
helper smoke, and one float/conversion compiler-rt smoke. Dormant hello/write
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

1. Add strict manifest parsing, planning, execution, and check tests.
2. Record the current runnable, support, and specialized-workflow directories.
3. Replace host globbing and embedded exclusions with positive profiles while
   preserving the actual current run set.
4. Keep the shell script only as a thin profile adapter.

### Phase 2: make evidence deterministic

1. Convert stable user output to exact goldens.
2. Convert variable output and historical regressions to self-checking code.
3. Add expected-failure ownership for the stacktrace case.
4. Add a dedicated socket harness or keep it explicitly manual.

### Phase 3: move and consolidate

1. Land destination tests before removing Go stdlib, compiler-lowering, or
   target regressions.
2. Consolidate the named Go, reflect, C, and Python groups within the size
   limits below.
3. Remove PyTorch setup after the tensor demo is gone.
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
- no output-only regression without a golden or self-check.

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

## Other audit finding

`dev/local_ci.sh` invokes `_demo/embed/targetsbuild/build.sh` without its
required test-directory argument. The script has required that argument since
December 2025, so local CI should explicitly run both `empty` and `defer`, as
the GitHub workflow does.

## Decision requested

Accept the explicit ownership/check model and the 51-case target. Implement
the manifest as an equivalent migration first, then perform deletion or moves
only after every unique regression has a deterministic destination owner.
