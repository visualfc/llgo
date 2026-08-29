# Compiler fixture inventory and migration map

Status: audit appendix for
[Compiler fixture suite organization](cl-test-fixtures.md).
Baseline: xgo-dev/main@c1d5da2 (2026-08-29).

## Scope and counting

A current case is a leaf directory selected by a `cl` fixture harness. The
baseline has 264 normally discovered directories plus the explicitly invoked
`_testlto/_globaldce_reflect_method_by_name_ltoplugin_string_abi_unknown`
directory, for 265 total.

The proposed count distinguishes:

- fixture directories, which compile a source package through a harness; and
- logical subtests, such as direct CFG rows in `cl/blocks`.

| Current suite | Current directories | Proposed owner/count |
| --- | ---: | --- |
| `_testdata` | 23 | redistributed into focused suites |
| `_testdefer` | 7 | 0 directories; 6 direct CFG subtests |
| `_testdrop` | 17 | 17 |
| `_testgo` | 80 | redistributed into focused suites |
| `_testlibc` | 9 | redistributed or removed |
| `_testlibgo` | 13 | removed; unique lowering moved |
| `_testlto` | 30 | 24 |
| `_testmeta` | 12 | 12 |
| `_testpy` | 8 | 3 Python owners |
| `_testrt` | 66 | redistributed into focused suites |
| **Total** | **265** | **145 directories + 6 CFG rows** |

Decision terms:

- **keep**: retain as an independent capability, usually with a smaller input;
- **merge**: another named owner covers the same production path;
- **split**: the current fixture contains several independent capabilities;
- **move**: the capability belongs to another test layer;
- **remove**: no independent compiler capability remains;
- **replace**: preserve the capability using a smaller or more direct test.

Each new, moved, or substantially rewritten `LITTEST` also records one target
scope: `common`, `os (...)`, `arch (...)`, or `os+arch (...)`. Scope is
orthogonal to the migration decision above. Untouched legacy cases are not
mechanically relabelled; the scope and FileCheck-prefix closure rules become
mandatory when their proposed owner is implemented.

## `_testdata`: 23

| Current case | Primary evidence | Decision and proposed owner |
| --- | --- | --- |
| `apkg` | floating compare, branch, and return in an imported package | merge into `scalar/control` |
| `cpkg` | `package C` unprefixed exports, C wrapper, compiler-used symbols | merge into `foreign/c/call` |
| `cpkgimp` | imported calls to unprefixed C-package symbols | share the local multipackage C fixture with `cpkg` |
| `debug` | composite DI types, function parameters, lexical scopes | split into three `_testdebug` fixtures |
| `embedunexport` | cross-package unexported embedded methods and wrappers | merge with `embedunexport-1598` into `interface/embed-unexported` |
| `floatint` | target-dependent float-to-signed/unsigned lowering | keep as focused `scalar/float-int` |
| `fncall` | direct call, integer compare, branch, scalar return | merge into `scalar/control` |
| `foo` | eface boxing, aggregate address, value-to-pointer method wrapper | split between `interface/boxing` and `abi/method` |
| `geometry1370` | cross-package struct layout, pointer receiver, interface registration | merge with `interface1370` |
| `importpkg` | imported init ordering and C-linkname varargs | split between `abi/package-init` and `foreign/c/call` |
| `llgointrinsics` | `funcPCABI0` variants, trampoline address, `skip` | split into `intrinsics/function-address` and `intrinsics/control` |
| `llgosyscall` | 3/6-argument, float, pointer, raw, and errno syscall ABI | split into two short syscall intrinsic fixtures |
| `llgotag` | selection of an LLGo-only source file | move to build/package selection coverage |
| `method` | value method, pointer wrapper, nil receiver guard | merge into `abi/method` |
| `print` | broad printer containing arithmetic, slice, bounds, and interface paths | remove after focused owners exist |
| `printf` | C varargs with no additional argument | merge into `foreign/c/call` |
| `printval` | C varargs with an integer argument | merge into `foreign/c/call` |
| `ptrmthd` | pointer receiver forwarding to a C vararg function | split between method ABI and C call owners |
| `uint` | unsigned 32-bit arithmetic through a thin C type alias | use builtin `uint32` in `scalar/integer` |
| `untyped` | untyped constant conversion and initialization | merge into `scalar/constants` |
| `utf8` | string index/slice plus a standard-library rune decoder | replace with a local focused string-range fixture |
| `vargs` | `...any` construction, boxing, assertion, and bounds | merge into `interface/variadic` |
| `varinit` | global load, add, and store | merge into `abi/globals` |

## `_testdefer`: 7

These cases call `cl/blocks.Infos`; they do not generate LLVM.

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `firstloop1` | entry loop with conditional exit and recover block | replace with direct multi-node SCC graph |
| `firstloop2` | minimal natural loop | replace with direct natural-loop graph |
| `gobuild` | 737-line, 206-block topology snapshot | remove and replace with small explicit graphs |
| `loop` | entry, loop, conditional, and recover classification | replace with direct SCC and conditional-exit graphs |
| `multiret` | multiple terminal blocks must not become always | replace with direct multiple-exit graph |
| `print` | complex branch/loop topology without defer | remove and replace with small shared-subgraph graph |
| `singleret` | diamond joining one terminal block | replace with direct single-exit diamond |

The final direct table contains six rows: single-exit diamond, multiple exits,
natural loop, multi-node SCC with conditional exits, self-loop, and
disconnected recover/shared subgraph.

## `_testgo`: 80

### ABI, layout, and evaluation order

| Current case | Primary evidence | Decision and proposed owner |
| --- | --- | --- |
| `abimethod` | value/pointer/anonymous/promoted/generic method ABI, wrappers, nil guards | split into method, promoted-method, and generic-method ABI fixtures |
| `alias` | alias struct fields and methods | merge into `abi/named-alias` |
| `allocinloop` | loop phi and allocation/call placement | merge into `scalar/loop-control` |
| `blankfield` | blank-field layout and RHS side effects | keep as `abi/blank-field` |
| `complitassign` | composite assignment evaluation order | merge with `complitnil` into a focused assignment fixture |
| `complitnil` | nil LHS panic timing while preserving RHS evaluation | merge with `complitassign` |
| `constconv` | constant folding before integer conversion | merge into `scalar/constants` |
| `localitycodegen` | LLGo TLS/GLS, local package block, local context | move to `_testintrinsics/locality` |
| `multiret` | aggregate insert/extract for multiple returns | merge into `abi/results` |
| `postabi` | post-C-ABI sret and target triple behavior | keep as `abi/post-cabi` |
| `returnorder` | multi-result expression evaluation order | keep as `abi/result-order` |

### CGo

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `cgobasic` | repeated wrapper slot loads plus CString/CBytes/GoString helpers | extract one wrapper and conversion coverage into `foreign/cgo/call-conversion` |
| `cgocfiles` | local C source/header and aggregate pointer ABI | merge into `foreign/cgo/aggregate-errno` |
| `cgodefer` | deferred C pointer keepalive and call-after-free-node order | merge into `foreign/cgo/callback-defer` |
| `cgofull` | C2 errno, exports, callback context, function pointer, macros, C files, Python | split across the three CGo owners; remove demo logic |
| `cgomacro` | object-like macro getters and ordinary wrappers | retain one macro path in `call-conversion` |
| `cgopython` | ordinary C wrappers combined with defer/unwind | wrapper is duplicate; retain only any unique defer/C ABI path in `callback-defer` |

### Closures and function values

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `closure` | closure pair and one captured value | merge into `closure/basic` |
| `closure2` | nested closure environment | merge into `closure/basic` or `closure/nested` |
| `closureall` | no-capture/capture/method/C-function/callback variants | split into method and CGo callback owners; remove external wrapper use |
| `closureenv` | zero-sized capture, address capture, typed-nil receiver | keep as focused `closure/environment` |

### Defer and panic code generation

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `defer1` | always/conditional registrations, bit flags, argument node, LIFO | merge into `defer/kinds` |
| `defer2` | conditional registration subset | merge into `defer/kinds` |
| `defer3` | unrecovered panic and `Rethrow` | reduce to `defer/rethrow` |
| `defer4` | recover frame, bind, activation token, consumed panic | reduce to `defer/recover` |
| `defer5` | panic replacement plus target setjmp/dispatch snapshot | move semantics to runtime; low-level ABI remains unit-tested |
| `deferclosure` | deferred closure, method/field closure values, closure-valued args | split into `defer/closure` and `defer/closure-arg` |
| `defercomplex` | loops, branches, evaluation order, named result, formatting | remove after `defer/kinds` and `defer/named-result` exist |
| `deferdispatch` | native blockaddress/indirectbr dispatch | remove duplicate acceptance; keep precise SSA unit owner |
| `deferdispatch-wasm` | wasm selector/switch dispatch | remove duplicate acceptance; keep precise SSA unit owner |
| `deferdispatch-wasm-target` | byte-identical source to wasm fixture | remove |
| `deferiface` | saved itab slot and receiver for deferred invoke | keep as `defer/interface` |
| `deferloop` | per-iteration registration and drain | merge into `defer/kinds` |
| `nesteddeferpanic` | runtime panic replacement/resumption semantics | move to `test/go` |
| `goexit` | Goexit defer lifecycle and recover behavior | move to `test/go` |
| `recoverthenpanic` | recover followed by a new deferred panic | move semantics to `test/go`; compiler paths owned by recover/rethrow |

### Concurrency

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `chan` | make, len/cap, send, receive, comma-ok, close, goroutine | reduce to `concurrency/channel` and `concurrency/goroutine` |
| `goroutine` | closure environment passed through root allocation to `NewProc` | keep as focused `concurrency/goroutine` |
| `select` | send/receive/default select lowering | merge with `selects` into `concurrency/select` |
| `selects` | additional blocking and multi-case select shapes | merge into `concurrency/select` |
| `syncmap` | calls into standard-library `sync.Map` | remove; owned by `test/std` |

### Interfaces, assertions, and equality

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `embedunexport-1598` | cross-package embedded unexported method wrappers | merge with `_testdata/embedunexport` |
| `equal` | scalar/string/array/struct/interface equality and incomparable kinds | split into comparable and interface equality owners |
| `errors` | local error boxing and dispatch | merge into `interface/basic` |
| `ifaceconv` | nil/non-nil interface conversion and assertion | keep as focused `interface/conversion` |
| `ifaceprom` | promoted embedded-interface slots | keep as `interface/promoted` |
| `interface` | imported concrete type to interface, itab, dispatch | merge into `interface/basic` |
| `interface1370` | imported pointer receiver through interface | merge with `geometry1370` |
| `invoke` | interface invocation across receiver shapes | retain representative scalar, pointer, and aggregate shapes |
| `strucintf` | aggregate eface boxing | merge into `interface/aggregate` |
| `struczero` | aggregate assertion comma-ok and zero result | merge into `interface/aggregate` |

### Maps, slices, and bounds

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `indexerr` | array/slice positive and negative bounds helpers | reduce to the focused bounds owner |
| `makemaphint` | sign/zero extension of map size hint | merge into `map/make` |
| `makeslice` | negative len/cap and len-greater-than-cap guards | keep as focused `data/make-slice` |
| `mapfast` | fast helper selection by key representation | keep as `map/helper-selection` |
| `mapindirect` | large indirect generic keys and generic helpers | keep as `map/indirect-key` |

### Reflect bridges

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `reflect` | compiler/runtime reflect call and method bridges | split only the unique call/method paths |
| `reflectconv` | 960-line standard-library-style conversion matrix | replace with a small conversion bridge fixture |
| `reflectfn` | reflected function value call | merge into `reflect/call` |
| `reflectmk` | Method and MethodByName compiler bridges | keep as `reflect/method` |
| `reflectmkfn` | MakeFunc callback bridge | keep as `reflect/makefunc` |

### Generics and recursive types

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `genericembediface` | generic embedded interface ABI | keep as `generics/interface` |
| `genericiter` | generic iterator callback and wrapper | keep as `generics/iterator` |
| `tpindex` | generic slice index/equality/range | merge into `generics/index` |
| `tpinst` | generic receiver method instantiation | merge into `generics/method` |
| `tplocalclosureiface` | local generic closure boxing/assertion | merge into `generics/local` |
| `tplocaltype` | local named-type identity across instantiations | merge into `generics/local` |
| `tpnamed` | nested named generic function values | merge into `generics/function-value` |
| `tprecur` | mutually recursive generic functions | keep as `generics/recursion` |
| `tprecurfn` | recursive generic type/function reference | merge into `generics/recursion` |
| `tptypes` | generic receiver declarations, constraints, variadic method | reduce to `generics/types` |
| `tpycombinator` | recursive generic closure with no new lowering | remove after recursion and closure owners exist |
| `typerecur` | non-generic recursive function type | keep with recursive-type ABI coverage |

### Broad demos, runtime behavior, and build-driver behavior

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `cursor` | large go/ast/iter program combining interfaces, generics, bit operations | remove after focused owners exist |
| `print` | Go print helper selection | reduce to a small builtin-print fixture |
| `reader` | large Reader implementation combining string, slice, interface, closure | remove after focused owners exist |
| `rewrite` | ordinary globals and imported calls without a unique configured rewrite | remove |
| `runextest` | external test-package discovery and execution | move to `internal/build` |
| `runtest` | internal test-package discovery and execution | move to `internal/build` |
| `sigsegv` | recoverable nil-dereference runtime behavior | move to runtime/`test/go` |

## `_testlibc`: 9

| Current case | Primary evidence | Decision and proposed owner |
| --- | --- | --- |
| `allocacstrs` | dynamic pointer array alloca and per-string CStrCopy | move to `intrinsics/cstring` |
| `argv` | `__llgo_argc`/`__llgo_argv` globals and pointer indexing | move to `intrinsics/runtime-globals` |
| `atomic` | load/store/add/sub/cmpxchg through thin wrappers | merge into `intrinsics/atomic` and `atomic-raw` |
| `complex` | C complex aggregate ABI and extraction | move to `foreign/c/aggregate` |
| `defer` | deferred C vararg call and C string capture | move to C/defer ownership and LLGo cstring/deferdata owners |
| `demangle` | call to an external Go wrapper; no C++ source compilation | remove and add a real `foreign/cpp-abi` fixture |
| `once` | pthread wrapper and external initializer | remove as external-library behavior |
| `setjmp` | platform setjmp symbols, buffer sizes, and branching | keep precise target unit coverage; local intrinsic smoke only if needed |
| `sqlite` | external sqlite wrapper and system-library link | remove as integration coverage |

## `_testlibgo`: 13

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `atomic` | standard atomic calls plus LLVM atomic lowering | move unique lowering to `_testintrinsics` |
| `bytes` | ordinary standard-library calls | remove; `test/std` owns it |
| `complex` | ordinary standard-library calls | remove; `test/std` owns it |
| `deferpanic` | deferred panic followed by recover | merge compiler path into defer; runtime semantics in `test/go` |
| `errors` | ordinary standard-library calls | remove; `test/std` owns it |
| `mapzero` | missing map element produces the element zero value | move minimal construct to `map/zero-result` |
| `math` | ordinary standard-library calls | remove; `test/std` owns it |
| `mathbits` | ordinary standard-library calls | remove; `test/std` owns it |
| `nettextproto` | ordinary standard-library calls | remove; `test/std` owns it |
| `os` | ordinary standard-library calls | remove; `test/std` owns it |
| `strings` | ordinary standard-library calls | remove; `test/std` owns it |
| `sync` | ordinary standard-library calls | remove; `test/std` owns it |
| `waitgroup` | ordinary standard-library calls | remove; `test/std` owns it |

## `_testpy`: 8

| Current case | Primary evidence | Decision |
| --- | --- | --- |
| `math` | Python module declaration, import, and symbol storage | move to `foreign/python/module` |
| `callpy` | fixed one-argument and zero-argument calls; result extraction | move to `foreign/python/calls` |
| `gcd` | N-argument Python call and platform-sized long conversion | merge into `foreign/python/calls` |
| `list` | scalar-to-Python conversion and list construction/access | reduce into `foreign/python/values` |
| `matrix` | repeated nested list construction and external numpy call | remove repetition; retain no new path beyond values/calls |
| `max` | variadic call, list/tuple iterator, repeated conversions | split unique call and tuple/list paths into calls/values |
| `pi` | module attribute lookup and float extraction | merge into module/values |
| `pow` | two-argument Python call | merge into `foreign/python/calls` |

The Python root package is supplied synthetically by the harness. Local module
packages keep the required `LLGoPackage = "py.<module>"` classification.

## `_testrt`: 66

### ABI, type descriptors, layout, and conversions

| Current case or group | Primary evidence | Decision |
| --- | --- | --- |
| `abinamed`, `abitype` | runtime type descriptors, PtrToThis/Elem, byte/rune identity | merge into one runtime-type ABI fixture |
| `cast` | integer/float truncation, extension, and conversion opcodes | keep as `scalar/numeric-cast` |
| `complex` | complex construction, division helper, real/imag | keep as focused complex arithmetic |
| `constuptr` | constant int-to-pointer conversion | merge into constants/unsafe owner |
| `eface` | runtime Type/Uncommon/Elem ABI introspection | merge into runtime-type ABI |
| `float2any` | exact float bit patterns during interface boxing | keep as `interface/float-boxing` |
| `gblarray` | global aggregate zero/init and index width | merge into `abi/global-array` |
| `gotypes` | named interface parameter and nil interface call | merge into `interface/basic` |
| `index` | bounds comparison, PanicIndex, index width | merge into focused bounds owner |
| `mask` | shift opcode, width conversion, negative-shift guard | keep as `scalar/shifts` |
| `named` | recursive struct layout mixed with captured closure | split unique layout and closure paths; remove broad snapshot |
| `namedslice` | named slice boxing/assertion | merge into aggregate assertion owner |
| `slice2array` | slice-to-array pointer/value guard | keep as focused conversion |
| `slicelen` | unsafe.Slice zero-length guard folding | merge into unsafe slice owner |
| `struct` | C struct pointer plus value/pointer method wrappers | split between C aggregate and method ABI |
| `structsize` | compile-time aggregate size | check direct return in layout owner |
| `typed` | named scalar and aggregate assertions | merge into assertion owners |
| `unsafe` | pointer arithmetic, unsafe Slice/StringData, bounds | split into unsafe pointer and unsafe slice/string owners |

### LLGo intrinsics and foreign calls

| Current case or group | Primary evidence | Decision |
| --- | --- | --- |
| `alloca`, `allocstr`, `cstr` | stack allocation and C string intrinsics | reorganize into focused cstring/memory owners |
| `asm`, `asmfull` | inline asm without args, input constraints, output registers | remove strict subset `asm`; keep one fixture with short functions |
| `callback` | closure pair passed to a C callback | merge into `foreign/cgo/callback-defer` |
| `cvar` | external aggregate globals | merge into `foreign/c/call` or aggregate owner |
| `fprintf` | external stderr global and C varargs | merge into `foreign/c/call` |
| `funcaddr` | raw address for an `llgo:type C` function | move to `intrinsics/function-address` |
| `hello`, `strlen` | external strlen followed by printf | replace both with one direct C call owner |
| `linkname` | cross-package unexported method/global/function linknames | keep as focused package-linkname fixture |
| `qsort`, `qsortfn` | Go closure and C function pointer callbacks; repeated pairwise conversions | retain representative ABI shapes in callback owner |
| `stacksave` | `llvm.stacksave` | move to `intrinsics/stack` |
| `unreachable` | `llgo.unreachable` to LLVM unreachable | move to `intrinsics/control` |

### Closures and calls

| Current case or group | Primary evidence | Decision |
| --- | --- | --- |
| `any` | eface assertion and boxed update | merge into assertion owner |
| `closure` | several capture shapes | merge into `closure/basic` |
| `closurebound` | bound method wrapper and nil receiver | merge into `closure/method-value` |
| `closureconv` | function, method, and literal conversions | merge into `closure/conversion` |
| `closureiface` | closure assertion and MatchesClosure | merge into `closure/interface` |
| `freevars` | nested free-variable environments | merge into `closure/nested` |
| `funcdecl` | declared function as `{code,nil}` pair and MatchesClosure | merge into closure interface/conversion |
| `intgen` | large generator combining closure and iteration | remove after focused owners exist |
| `litdemo` | direct call, closure, global init, arithmetic | remove after focused owners exist |
| `methodthunk` | method thunk closure and receiver handling | merge into method-value/thunk owner |
| `result` | closure and aggregate function results | merge into result and closure-result owners |
| `vamethod` | variadic value/pointer/interface method ABI | merge into variadic-interface owner |

### Builtins, maps, slices, and strings

| Current case or group | Primary evidence | Decision |
| --- | --- | --- |
| `builtin` | append, copy, conversions, range, and miscellaneous builtins | split into three small slice/string/builtin fixtures |
| `clear` | SliceClear and MapClear | split between data and map owners |
| `concat` | StringCat, loop concat, and bounds | merge into string operations |
| `len` | channel/map len and cap helpers | merge into focused builtin len/cap |
| `makemap` | make, fast keys, iteration, delete, generic fallback | split into map make, helper selection, and operations |
| `map` | minimal fast64 map path | remove as a strict subset |
| `mapclosure` | interface/closure map value and itab | merge into `map/value` |
| `sum` | typed variadic slice, loop, and bounds | merge into variadic/builtin owners |

### Reflect, defer, generics, and aliases

| Current case or group | Primary evidence | Decision |
| --- | --- | --- |
| `ptrtothislazy`, `ptrtothislazynew` | lazy PtrToThis through PointerTo/New/NewAt/Addr | keep the broader case in reduced form |
| `reflectclosureenv` | reflect Call/MakeFunc/method while preserving closure env | reduce to `reflect/closure` |
| `nextblock` | conditional defer cleanup across CFG joins | merge into `defer/kinds` and direct block graphs |
| `panic` | interface boxing, Panic helper, unreachable | merge into defer/panic basic owner |
| `tpabi` | instantiated generic value/pointer method ABI | merge into `generics/method` |
| `tpfunc` | local generic closures and type size | merge into `generics/function-value` |
| `tpmap` | generic map descriptors and helpers | merge into `generics/map` |
| `tpmethod` | nested generic method/function result ABI | merge into `generics/method` |
| `tpunsafe` | generic Alignof/Offsetof/layout | keep as reduced `generics/layout` |
| `typalias` | named/alias pointer crossing C ABI | merge into C aggregate/alias owner |

## `_testdrop`: 17

All cases retain independent reachability ownership.

| Current case | Primary reachability contract |
| --- | --- |
| `c_export_callback` | C-exported callback root |
| `direct_func` | direct function-value root |
| `direct_method` | direct method root |
| `exported_method_crosspkg` | exported cross-package method root |
| `generic_interface_crosspkg` | instantiated generic interface demand |
| `generic_interface_func_crosspkg` | generic-function propagation of interface demand |
| `iface_flow_crosspkg` | interface value flow across packages |
| `interface_demand_fixedpoint` | iterative interface-demand propagation |
| `interface_match` | concrete method-set/interface matching |
| `interface_slot` | demanded itab slot |
| `promoted_method_wrapper` | promoted embedded-method wrapper |
| `reflect_dynamic_iface_crosspkg` | dynamic reflect/interface cross-package root |
| `reflect_field_addr_iface` | reflected field address entering an interface |
| `reflect_method_result` | reflect method result propagation |
| `reflect_named_method` | statically named reflect method root |
| `source64_crosspkg` | cross-package Source64/type source |
| `unexported_method_identity` | package-qualified unexported method identity |

## `_testlto`: 30

| Current case or group | Primary LTO contract | Decision |
| --- | --- | --- |
| `_globaldce_reflect_method_by_name_ltoplugin_string_abi_unknown` | unknown aggregate-string name negative path | keep explicit |
| `abitype_runtime` | runtime ABI type under LTO | keep |
| `anonymous_alias` | anonymous alias method metadata | keep |
| `globaldce_abitype_fakeuse` | ABI fake-use must not retain dead code | keep |
| `globaldce_interface_matrix` | interface/method-set/type metadata matrix | keep |
| `globaldce_interface_slots` | subset of interface matrix | merge into matrix |
| `globaldce_reflect_method` | reflect Method/MethodByName metadata roots | keep |
| `globaldce_reflect_method_by_name_ltoplugin`, `globaldce_reflect_method_by_name_ltoplugin_param` | direct/helper-forwarded finite name | merge into one helper-forwarding owner |
| `globaldce_reflect_method_by_name_ltoplugin_concat`, `globaldce_reflect_method_by_name_ltoplugin_slice` | finite names through string transformation | merge into one transformation owner |
| `globaldce_reflect_method_by_name_ltoplugin_global`, `globaldce_reflect_method_by_name_ltoplugin_global_slice` | finite names from global/aggregate storage | merge into one global-source owner |
| `globaldce_reflect_method_by_name_ltoplugin_loop`, `globaldce_reflect_method_by_name_ltoplugin_range_literal`, `globaldce_reflect_method_by_name_ltoplugin_switch` | finite-name sets through CFG | merge into one small CFG matrix |
| `globaldce_reflect_method_by_name_ltoplugin_string_abi` | aggregate string ABI | keep separate |
| `globaldce_reflect_type_method` | reflect Type.Method root | keep |
| `globaldce_reflect_type_method_by_name` | reflect Type.MethodByName root | keep |
| `globaldce_reflect_type_method_metadata_only` | metadata-only method root | keep |
| `globaldce_reflect_value_method` | reflect Value.Method root | keep |
| `globaldce_static_itab_devirt` | complete static-itab devirtualization | keep |
| `globaldce_static_itab_partial_root` | partial static-itab root | keep |
| `globaldce_typeid_dce` | type-id dead-code elimination | keep |
| `globaldce_unexported_method_identity` | unexported method identity during DCE | keep |
| `ifaceconv_runtime` | interface conversion runtime integration | keep |
| `reflectmk_runtime` | reflect method construction runtime integration | keep |
| `reflectmkfn_runtime` | reflect function construction runtime integration | keep |
| `testpcline` | pcline retention under LTO | keep |
| `typed_runtime` | named/typed runtime integration | keep |

The grouped string-flow rows reduce eleven finite-name/unknown variants to six
owners, including the explicit unknown case. Together with the interface-slot
merge, `_testlto` changes from 30 to 24.

## `_testmeta`: 12

| Current case | Primary metadata contract | Decision |
| --- | --- | --- |
| `ifaceuse_basic` | UseIface root | keep |
| `interface_anyonmous` | anonymous interface identity, InterfaceInfo, MethodInfo | keep; fix name during migration |
| `interface_exported_var` | type edge from exported interface-valued global | keep with local package |
| `interface_generic` | instantiated generic MethodInfo | keep |
| `interface_generic_crosspkg` | cross-package generic instances | keep |
| `interface_imported` | imported interface/method identity | keep |
| `interface_named` | named interface identity | keep |
| `interface_unexported` | package-qualified unexported method identity | keep |
| `methodinfo_imported` | imported concrete method table | keep with local package |
| `reflect_dynamic` | dynamic reflect root | keep |
| `reflect_named` | UseNamedMethod and reflect type-method metadata | keep |
| `typechildren_basic` | nested aggregate TypeChildren edges | keep |

## New owners required by the audit

Three physical fixtures are new capabilities rather than reorganized old
cases. They are already included in the proposed 145-directory total.

| New case | Missing capability |
| --- | --- |
| `scalar/arith-divrem` | zero-divisor guard, safe operands, signed minInt/-1, all four integer div/rem opcodes |
| `scalar/builtin-minmax` | integer, floating, and string source lowering; NaN and signed-zero Go semantics |
| `foreign/cpp-abi` | actual C++ source compilation, linking, and C ABI bridge |

The LLGo intrinsic gaps are folded into existing proposed owners:

| Proposed owner | Added independent assertions |
| --- | --- |
| `intrinsics/cstring` | `allocCStr` and `stringData` |
| `intrinsics/atomic-raw` | xchg/and/nand/or/xor/max/min/umax/umin |
| `foreign/python/values` | `pystr` |
| `foreign/cgo/check-pointer-noop` | no residual `_cgoCheckPointer` call |

## Migration invariant

A current case may be removed only after every positive and negative assertion
that represents an independent compiler path has a named destination owner.
Runtime output, standard-library calls, or broad incidental instructions do
not become compiler owners merely because they appear in an old golden.
