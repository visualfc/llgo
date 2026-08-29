# Demo suite inventory and migration map

Status: audit appendix for
[Demo and integration suite organization](demo-suite.md).
Baseline: xgo-dev/main@c1d5da2 (2026-08-29).

## Counting and decision terms

The count uses logical case families. A client/server pair is one
multi-process family, while independently built embedded programs are separate
cases. Support packages are not cases.

| Family | Baseline | Implemented |
| --- | ---: | ---: |
| Go | 74 | 17 |
| C/C++/CGo/asm | 28 | 16 |
| Python | 7 | 2 |
| Embedded executables | 13 | 8 |
| **Total** | **122** | **43** |

The implementation has 33 generic runnable owners, seven support packages,
and 11 workflow-owned source directories. The workflow set represents one
manual socket family, one expected-failure Go family, and eight embedded
executables. Thus 51 physical Go-source directories map to 43 logical case
families without treating support packages as programs.

The tables below preserve the initial per-case audit. Where an initial
`move` depended on a separate, not-yet-merged change, this PR instead keeps the
exact input in a bounded aggregate owner. The authoritative final groupings
are:

- Go core/tooling/ecosystem: 15 runnable owners plus the expected stacktrace
  failure;
- reflect: one `reflect` package split into capability-focused source files;
- foreign: 15 runnable owners plus the manual socket family;
- Python: `basic` and `matrix`;
- embedded: the eight active specialized workflow executables.

- **keep** retains an independent owner, usually after shrinking it;
- **merge** moves unique subcases into another bounded owner;
- **move** lands the regression in a more appropriate test layer first;
- **remove** has no independent capability after existing owners are checked;
- **replace** exchanges a broad or dormant case for focused deterministic
  evidence.

## Go: 74 to 17

### ABI, integration, directives, and concurrency

| Current case | What it currently exercises | Decision |
| --- | --- | --- |
| `abimethod` | value/pointer/embedded/generic method ABI | keep and shrink; absorb `aliasrecv` |
| `aliasrecv` | alias pointer receiver method set | merge into `abimethod` |
| `async` | custom future; timeout path is not called and completion is not awaited | remove |
| `atomicfn` | atomic function value call | merge into deterministic `concurrency` |
| `cabi` | interface field clear and result order, despite its name | move to a focused compiler ABI regression |
| `cgo` | compact C calls, aggregate pointer shapes, C2 errno paths | merge the distinct errno two-result path into `cgofull`; its other paths are duplicates |
| `defer` | capture, closure, and recover | move to focused defer/runtime tests |
| `export` | generated header, C consumer, archive/shared, callbacks and aggregates | keep specialized; split source by ABI family |
| `goroutine` | goroutine/channel smoke | merge into `concurrency` |
| `ifaceconv` | nil/non-nil conversion and assertion | keep and absorb the promotion regression |
| `ifaceprom-1559` | cross-package unexported promoted-method identity | merge into `ifaceconv`; preserve issue path |
| `issue1538` | signed/unsigned integer/float width conversion | keep a boundary-value matrix |
| `issue1538-floatcvtuint-over` | overflow subset of issue 1538 | merge into `issue1538` |
| `linkname` | directive placement and cross-file alias | keep; absorb `mainlink` |
| `mainlink` | main-symbol alias | merge into `linkname` |
| `mapclosure` | function values stored in maps/slices/globals | move to compiler closure/map fixture |
| `return-1605` | captured slice result before field reassignment | move to compiler return-ABI regression |
| `statefn` | named recursive function type | move to focused compiler test |
| `sync` | mutex/once/waitgroup/atomic/time mix | merge into deterministic `concurrency` |
| `syscallraw` | Unix raw syscall and Windows SyscallN ABI | keep and self-check |
| `sysopen-1654` | raw platform open/write regression | keep separately |

### Files, processes, time, and standard library

| Current case | What it currently exercises | Decision |
| --- | --- | --- |
| `checkfile` | Stat/Open/not-exist | merge into `fileio` |
| `commandrun` | `os/exec` output and working directory | merge into `process` |
| `complex` | `math/cmplx` calls | remove; `test/std` owner |
| `createtemp-1654` | concurrent CreateTemp/MkdirTemp regression | keep separately |
| `embedunexport-1598` | embedded `types.Func` implementing Object and Scope insertion | move to `test/std/go/types` |
| `failed` | nested generic panic stacktrace, currently undiscovered on host | move to an expected-failure integration test |
| `gobuild` | Import/ImportDir/overlay/build tags | move to `test/std/go/build` |
| `gobuild-1389` | issue 1389 build regression | merge into moved go/build owner |
| `goimporter-1389` | issue 1389 `ForCompiler` path | move separately to `test/std/go/importer` |
| `gotime` | `time.Now`/clock | merge into `timer` |
| `gotoken` | Token, Position, FileSet, identifiers | move assertions to `test/std/go/token` |
| `gotypes` | constructors, identity, assignability, method sets | move assertions to `test/std/go/types` |
| `logdemo` | standard `log` smoke | remove; `test/std` owner |
| `maphash` | seed/reset/write/string/bytes behavior | move to `test/std/hash/maphash` |
| `math` | standard `math` smoke | remove; `test/std` owner |
| `mimeheader` | `net/textproto.MIMEHeader` | remove; `test/std` owner |
| `mkdirdemo` | MkdirAll and cleanup | merge into `fileio` |
| `netip` | `net/netip` smoke | remove; `test/std` owner |
| `osfile` | create/open/read/write/seek/stat/truncate | become the bounded `fileio` owner |
| `oslookpath` | executable lookup | merge into `process` |
| `oswritestring` | exact WriteString/ReadFile content | merge into `fileio` |
| `randcrypt` | `crypto/rand` smoke | remove; `test/std` owner |
| `randdemo` | `math/rand` smoke | remove; `test/std` owner |
| `readdir` | directory entry order/type | merge into `fileio` |
| `runtime` | `runtime.GOROOT` smoke | remove; `test/std` owner |
| `syscall` | standard syscall file operations | move to `test/std/syscall`; remove thin wrapper |
| `sysexec` | executable lookup/process path | merge into `process` |
| `texttemplate` | `text/template` smoke | remove; `test/std` owner |
| `timedur` | Duration and Until | merge into `timer` |
| `timer` | timer stop/reset/afterfunc, currently timing/output based | keep and rewrite with channel barriers |

### Reflect: 23 to one

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `reflectcallfn` | Value.Call closure and multi-result ABI | merge into split-file `reflect` owner; absorb interface-wrapped result |
| `reflectifacecall` | reflected value returned through interface | merge into `reflectcallfn` |
| `reflectfunc` | Func kind, fields, parameters, and results | merge into `reflect`; absorb type/name metadata |
| `reflectfntype` | named function fields/parameters/results | merge into `reflectfunc` |
| `reflectname-1412` | named integer Name/Kind regression | merge into `reflectfunc` |
| `reflectnamedfn` | named Go/C function values, Set, Call, method | merge into `reflect`; absorb New/NewAt storage |
| `reflectnew` | New/NewAt function and closure storage | merge into `reflectnamedfn` |
| `reflectmakefn` | MakeFunc callback and closure result | merge into `reflect`; absorb conversion/empty aggregate ABI |
| `reflectfnconv` | named MakeFunc/FuncOf conversion | merge into `reflectmakefn` |
| `reflectempty` | zero-size struct/array parameter and result | merge into `reflectmakefn` |
| `reflectmethod` | Method/ByName, promoted/variadic and receiver ABI matrix | merge into `reflect`; retain its complete live ABI matrix |
| `reflectembed` | promoted embedded method calls | merge into `reflectmethod` |
| `reflectconv` | function conversion; most copied source is dead | merge the live conversion paths into `reflect` |
| `reflectmake` | dynamic Array/Slice/Map/Func/Chan type construction | merge into `reflect`; retain every subtest called by its original `main` |
| `reflectchanof` | ChanOf-to-PointerTo chain | merge into `reflectmake` |
| `reflectstructof` | dynamic StructOf function fields, Set, and Call | merge into the dynamic-type files in `reflect` |
| `reflectpointerto` | dynamic/named/multilevel pointer metadata | merge into `reflect`; absorb package/method-array metadata |
| `reflectpkgpath` | named unsafe pointer package and method metadata | merge into `reflectpointerto` |
| `reflectslice` | dynamic uncommon/method metadata array size | merge into `reflectpointerto` |
| `reflectcopy` | overlapping slice/string/array copy and panic | merge into the value-operation file in `reflect` |
| `reflectindirect` | nonpointer, pointer, nil pointer, struct Indirect | merge into `reflectcopy` |
| `reflectsliceat` | unsafe-backed SliceAt | merge into version-gated `reflectcopy` section |
| `reflectvisiblefields` | embedded field index traversal | merge into `reflectcopy` |

The implementation has one runnable reflect package, split into focused files
for function calls, dynamic arrays/slices/maps/structs/functions/channels,
pointer metadata, method ABI, conversion, and value operations. It preserves
all subtests called by the original `reflectmake` and `reflectmethod`
entrypoints, including the complete receiver-size and return-shape ABI
matrices; only their commented-out dead subtests stay absent. Together with 15
non-reflect runnable owners and the workflow-owned expected failure, Go has 17
logical owners.

## C, C++, CGo, and asm: 28 to 16

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `asmcall` | one raw asm input plus a nondeterministic benchmark | merge one short subcase into `asmfullcall` |
| `asmfullcall` | platform asm inputs, outputs, memory, real execution | keep core and self-check |
| `cabi` | C aggregate register/sret, callback, export | keep one self-checking representative per ABI family |
| `cabisret` | large aggregate return/allocation regression | merge its exact two 131072-iteration 9-float sret loops and 131072 appends into host-only `cabi` |
| `cargs` | pkg-config, RPATH negative path, Windows static link, argv | keep specialized integration |
| `catomic` | C atomic load/store/RMW/CAS | move to focused intrinsic runtime owner |
| `cexec` | platform process replacement and C argv | merge into the controlled-child process owner |
| `cgofull` | broad CGo preamble/source/macro/export/callback/Python mix | keep as the single CGo integration owner and absorb errno two-result checking |
| `concat` | documented Go-string-to-C-string and C vararg example | keep as primary-Ubuntu example |
| `cppintf` | real C++, vtable, C function bridge | keep; absorb multiple inheritance subcase |
| `cppmintf` | secondary-base `this` adjustment | merge into a bounded `cppintf` subcase |
| `cppstr` | thin external `std::string` wrapper | move to wrapper-library tests |
| `crand` | random/time wrappers and nondeterministic output | move to wrapper-library tests |
| `ctime` | platform time wrappers and changing output | move to binding tests |
| `fcntl` | POSIX/Windows descriptor operations | keep; assert all return values and clean up |
| `genints` | pure-Go closure and function values | remove; compiler fixture owner |
| `getcwd` | thin alloca/getcwd wrapper | move to binding tests |
| `hello` | minimal documented C call | keep; absorb the WASM C-FFI smoke |
| `helloc` | minimal C-FFI and explicit WASM workflow | merge into `hello`, retaining WASM validation |
| `linkname` | direct C-symbol linkname | remove; directive tests own it |
| `llama2-c` | model/file/argv/memory integration | keep scheduled/manual |
| `netdbdemo` | localhost resolver and Windows WSA lifecycle | keep with return/non-nil assertions |
| `qsort` | documented C-to-Go callback/reentry | keep on primary Ubuntu with sorted-result assertion |
| `setjmp` | platform setjmp/longjmp ABI | move to focused cross-platform runtime fixture |
| `socket` | two-process POSIX/Winsock data path, currently zero CI | keep manual until a deterministic harness exists |
| `stacksave` | real stack-save result versus machine stack pointer | keep core |
| `syncdebug` | Go mutex plus printed foreign mutex size | remove; it does not exercise the attempted pthread paths |
| `thread` | pthread-to-Go callback, TLS/locality, join result | keep core; assert every error/result |

The standalone implementation retains 15 runnable families plus the manual
socket family. Compared with the narrower initial proposal, `catomic`,
`cgofull`, `ctime`, `qsort`, and `setjmp` remain because they cross distinct
intrinsic, source-generation, wrapper, callback, or return-twice boundaries.
Their duplicate command packages are still merged: asm, sret, C++ multiple
inheritance/string, C time/random, descriptor/getcwd, and hello/helloc/concat
each have one runnable owner.

Runtime setup metadata is deliberately documented rather than parsed by the
runner:

| Owner | CI prerequisites |
| --- | --- |
| `cargs` | `pkg-config`, `cargs` |
| `cgofull` | `python3`, `python3-embed` |
| `py/basic` | `python3` |
| `py/matrix` | `python3`, `numpy`, `torch` |

The remaining compile/link/run-focused evidence also stays here rather than in
a generic check schema: `cppintf` owns C++ wrapper/vtable linking; `hello` owns
the minimal C vararg/stdout/stderr bridge; `cgofull` owns generated CGo source,
macro, export, callback, and Python-C-API linking in addition to its value
assertions; `go/export` owns generated artifacts and C consumers; and
`llama2-c` owns the scheduled model/file/runtime pipeline. Their command exit
status is the integration result, while value-oriented paths assert internally.

## Python: seven to two

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `callpy` | module import and fixed-arity scalar call | keep canonical core case |
| `matrix` | nested list conversion and NumPy call | keep sole third-party integration |
| `max` | variadic call and list/tuple iterator paths | keep small independent core case |
| `pi` | value extraction and C variadic bridge | keep |
| `print` | scalar Python print | remove; subset of `callpy` |
| `statistics` | math call and float extraction | merge into `basic` with a stable mean assertion |
| `tensor` | nested containers and PyTorch extension | merge into `matrix` with deterministic `tolist` value checks |

`callpy`, `max`, `pi`, and `statistics` are implemented as one `basic` owner. It retains
`math.Sqrt`, `math.Pi`, ordinary and iterator-expanded `std.Max`, `std.Print`,
scalar extraction, `statistics.Mean`, and the C variadic call. `matrix` remains
separate because NumPy and PyTorch object/list conversion form the scientific
third-party boundary.

## Embedded: 13 to eight

`_demo/embed` itself is not read by the generic demo runner. Its coverage comes
from specialized workflow scripts.

| Current executable | Primary evidence and current CI | Decision |
| --- | --- | --- |
| `esp32/hello` | board-UART hello, never referenced by CI | remove in favor of canonical serial hello |
| `esp32/write` | libc/write smoke with past fixes, now dormant | remove after focused libc ownership |
| `esp32/libc` | 1,162-line, 146-declaration libc program; no CI and weak failure reporting | replace, if needed, with one small self-checking libc target smoke |
| `esp32/rt` | 385-line compiler-rt builtin matrix; no CI and weak failure reporting | replace, if needed, with integer and float/conversion target smokes |
| `esp32c3/write` | single C printf, no CI | remove; serial hello owns it |
| `esp32c3/float-1664` | float/interface aggregate regression with exact startup output | keep |
| `esp32c3/print-float-1723` | Go print plus C variadic float regression | keep separately |
| `export` | renamed embedded export symbols checked with `nm` | keep specialized regression |
| `targetsbuild/empty` | target/toolchain baseline | keep |
| `targetsbuild/defer` | defer/panic target-build regression with high churn | keep separately |
| `testdata/esp32-serial/chello` | C serial baseline with expected tail | keep |
| `testdata/esp32-serial/int64slice` | target int64 slice-bounds width regression | keep |
| `testdata/esp32-serial/gc-runtime` | GC roots, graphs, allocator and stats; recent repeated fixes | keep; later split into three bounded cases |

All eight current automated regressions remain. The audit found that the
dormant libc/compiler-rt monoliths added no checked CI owner: active serial C,
GC/runtime, target-build, float, and export workflows plus the positive generic
target profiles retain the target/toolchain boundaries. No unchecked
replacement programs were added.

## Implemented profile sizes

| Profile / OS | Before | Implemented |
| --- | ---: | ---: |
| host Linux/macOS | 103 | 32 |
| host Windows | 101 | 31 |
| full LTO / GlobalDCE | 103 | 8 |
| DeadcodeDrop | 103 | 6 |
| ESP32 | 20 | 5 |
| ESP32-C3 | 28 | 6 |
| model | 1 | 1 |

Merged owners use the intersection of their retained subcases' known-good
profiles. In particular, `cabi`, `cppintf`, and `fcntl/getcwd` are host-only,
`core-regressions` retains ESP32-C3 but not ESP32, and `issue1538` retains
ESP32-C3 but not ESP32. Other focused owners continue to carry target coverage;
no bare-metal no-op is counted as execution of a merged capability.

With the current five host/embedded workflow lanes, this changes the generic
plan from about 960 binary executions to about 229: host 159, embedded 55, LTO
eight, DeadcodeDrop six, and model one. Ordinary host cases are built by
explicit selected-directory batches for C, Go, and Python, while LTO and
DeadcodeDrop each need only their C and Go groups. Embedded and model cases
remain specialized. This reduces planned `llgo` compiler invocations further,
from 229 per-case invocations to about 75, without changing the 229 executed
binaries. The exact wall-clock comparison belongs in CI because package caches
and runner hardware affect local timing.

## Current CI coverage facts

| Path/group | Host | ESP32 | ESP32-C3 | Specialized |
| --- | ---: | ---: | ---: | --- |
| generic host candidates | 107 | - | - | repeated for LTO/drop |
| recursively discovered C/Go source dirs | - | 111 | 111 | - |
| actually run after target exclusions | - | 20 | 28 | - |
| `workflow/expected-failure/stacktrace` | no | ignored | ignored | explicit expected-failure owner |
| `workflow/socket/{client,server}` | no | ignored | ignored | manual multi-process owner |
| embedded 13 executables | no | no | no | eight explicitly covered |

All five host workflow lanes currently run the host and both embedded generic
profiles. Compatibility lanes are allowed to fail but still execute. Full LTO
and DeadcodeDrop each rerun the host set. The new manifest preserves the
CI-selected Go version and makes every intentional repetition visible.

## Migration invariant

Deleting a path is allowed only after its independent capability has one of:

- a self-checking or golden-checked retained case;
- a focused compiler/runtime/standard-library destination test running in CI;
- an explicit specialized or optional profile whose limitations are stated.

A matching import or a successful process is not by itself equivalent
coverage.
