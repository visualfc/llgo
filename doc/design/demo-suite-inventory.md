# Demo suite inventory and migration map

Status: audit appendix for
[Demo and integration suite organization](demo-suite.md).
Baseline: xgo-dev/main@c1d5da2 (2026-08-29).

## Counting and decision terms

The count uses logical case families. A client/server pair is one
multi-process family, while independently built embedded programs are separate
cases. Support packages are not cases.

| Family | Current | Proposed |
| --- | ---: | ---: |
| Go | 74 | 23 |
| C/C++/CGo/asm | 28 | 13 |
| Python | 7 | 4 |
| Embedded executables | 13 | 11 |
| **Total** | **122** | **51** |

- **keep** retains an independent owner, usually after shrinking it;
- **merge** moves unique subcases into another bounded owner;
- **move** lands the regression in a more appropriate test layer first;
- **remove** has no independent capability after existing owners are checked;
- **replace** exchanges a broad or dormant case for focused deterministic
  evidence.

## Go: 74 to 23

### ABI, integration, directives, and concurrency

| Current case | What it currently exercises | Decision |
| --- | --- | --- |
| `abimethod` | value/pointer/embedded/generic method ABI | keep and shrink; absorb `aliasrecv` |
| `aliasrecv` | alias pointer receiver method set | merge into `abimethod` |
| `async` | custom future; timeout path is not called and completion is not awaited | remove |
| `atomicfn` | atomic function value call | merge into deterministic `concurrency` |
| `cabi` | interface field clear and result order, despite its name | move to a focused compiler ABI regression |
| `cgo` | compact C calls, aggregate pointer shapes, C2 errno paths | keep |
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

### Reflect: 23 to 10

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `reflectcallfn` | Value.Call closure and multi-result ABI | keep; absorb interface-wrapped result |
| `reflectifacecall` | reflected value returned through interface | merge into `reflectcallfn` |
| `reflectfunc` | Func kind, fields, parameters, and results | keep; absorb type/name metadata |
| `reflectfntype` | named function fields/parameters/results | merge into `reflectfunc` |
| `reflectname-1412` | named integer Name/Kind regression | merge into `reflectfunc` |
| `reflectnamedfn` | named Go/C function values, Set, Call, method | keep; absorb New/NewAt storage |
| `reflectnew` | New/NewAt function and closure storage | merge into `reflectnamedfn` |
| `reflectmakefn` | MakeFunc callback and closure result | keep; absorb conversion/empty aggregate ABI |
| `reflectfnconv` | named MakeFunc/FuncOf conversion | merge into `reflectmakefn` |
| `reflectempty` | zero-size struct/array parameter and result | merge into `reflectmakefn` |
| `reflectmethod` | Method/ByName, promoted/variadic and receiver ABI matrix | keep, drastically reduce Cartesian variants |
| `reflectembed` | promoted embedded method calls | merge into `reflectmethod` |
| `reflectconv` | function conversion; most copied source is dead | keep only the live focused regression |
| `reflectmake` | dynamic Array/Slice/Map/Func/Chan type construction | keep small cache/GC/algorithm representatives |
| `reflectchanof` | ChanOf-to-PointerTo chain | merge into `reflectmake` |
| `reflectstructof` | dynamic StructOf function fields, Set, and Call | keep separate with bounded metadata representatives |
| `reflectpointerto` | dynamic/named/multilevel pointer metadata | keep; absorb package/method-array metadata |
| `reflectpkgpath` | named unsafe pointer package and method metadata | merge into `reflectpointerto` |
| `reflectslice` | dynamic uncommon/method metadata array size | merge into `reflectpointerto` |
| `reflectcopy` | overlapping slice/string/array copy and panic | merge into `reflectvalue` |
| `reflectindirect` | nonpointer, pointer, nil pointer, struct Indirect | merge into `reflectvalue` |
| `reflectsliceat` | unsafe-backed SliceAt | merge into version-gated `reflectvalue` section |
| `reflectvisiblefields` | embedded field index traversal | merge into `reflectvalue` |

The proposed Go owners are 13 non-reflect plus ten reflect, for 23.

## C, C++, CGo, and asm: 28 to 13

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `asmcall` | one raw asm input plus a nondeterministic benchmark | merge one short subcase into `asmfullcall` |
| `asmfullcall` | platform asm inputs, outputs, memory, real execution | keep core and self-check |
| `cabi` | C aggregate register/sret, callback, export | keep core; one representative per ABI family |
| `cabisret` | large aggregate return/allocation regression | move to a small deterministic C ABI runtime owner |
| `cargs` | pkg-config, RPATH negative path, Windows static link, argv | keep specialized integration |
| `catomic` | C atomic load/store/RMW/CAS | move to focused intrinsic runtime owner |
| `cexec` | platform process replacement and C argv | move to process/library integration |
| `cgofull` | broad CGo preamble/source/macro/export/callback/Python mix | remove only after three compiler CGo owners land |
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

The 13 retained families are nine core integrations, two documented smokes,
and the `llama2-c` and `socket` optional families.

## Python: seven to four

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `callpy` | module import and fixed-arity scalar call | keep canonical core case |
| `matrix` | nested list conversion and NumPy call | keep sole third-party integration |
| `max` | variadic call and list/tuple iterator paths | keep small independent core case |
| `pi` | value extraction and C variadic bridge | keep |
| `print` | scalar Python print | remove; subset of `callpy` |
| `statistics` | math call and float extraction | merge into existing call/value owners |
| `tensor` | nested containers and PyTorch extension | remove; duplicates matrix and adds heavy dependency |

## Embedded: 13 to 11

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

Eight current automated regressions remain. The capability-preserving target
adds at most three replacements for the dormant libc/compiler-rt programs,
yielding 11. If an audit proves equivalent same-target coverage, omit those
replacements and use an eight-case final target.

## Current CI coverage facts

| Path/group | Host | ESP32 | ESP32-C3 | Specialized |
| --- | ---: | ---: | ---: | --- |
| generic host candidates | 107 | - | - | repeated for LTO/drop |
| recursively discovered C/Go source dirs | - | 111 | 111 | - |
| actually run after target exclusions | - | 20 | 28 | - |
| `go/failed/stacktrace` | no | ignored | ignored | none |
| `c/socket/{client,server}` | no | ignored | ignored | none |
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
