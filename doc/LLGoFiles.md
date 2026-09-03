# LLGoFiles — Attaching C/C++ Source Files to Go Packages

This proposal introduces `LLGoFiles`, a package-level declaration that lets a Go package participate in compilation together with a set of C/C++ source files. Unlike cgo, which embeds C code inline inside `import "C"` comment blocks, `LLGoFiles` keeps the native sources as ordinary files on disk (by convention under a `_wrap` subdirectory) and simply tells the LLGo compiler which of them belong to the package's build.

## Motivation

LLGo compiles Go to LLVM IR specifically to make interoperation with the C/C++ ecosystem cheap and direct. A recurring need in that ecosystem work is wrapping an existing C/C++ library: a small amount of glue code has to sit between the native API and the Go-visible declarations LLGo binds against.

cgo already solves a version of this problem, but its approach — pasting C source into a comment above `import "C"` — has drawbacks that get worse as the wrapper grows:

- **No tooling for the C/C++ side.** Code embedded in a Go comment does not get syntax highlighting, clang-format, clang-tidy, IDE navigation, or a C++ compiler's own diagnostics in the normal way.
- **No file-level separation.** A single package tied to one comment block encourages either a giant blob of C++ in one place, or awkward multi-file/multi-package contortions to split it up.
- **Poor fit for real C++.** Bindings to C++ libraries (as opposed to C) often need proper `.cpp`/`.hpp` files, multiple translation units, and library-specific compiler/linker flags — things cgo's comment-embedding model was never designed around.
- **Diffs and reviews suffer.** Native code changes and Go code changes are interleaved in the same file, making history and review harder to read.
- **Limited to one native language.** cgo's `import "C"` model is inherently tied to C. Since LLGoFiles just hands files off to the compiler by extension, it extends naturally to other native languages LLVM/clang can handle — Objective-C (`.m`/`.mm`) being an immediate case, with room to grow further as LLGo's needs expand.

LLGoFiles addresses this by treating the native sources as first-class files that live next to the package, compiled and linked as part of building it, without changing how the Go side of the package looks or is written.

## Design

### Declaration

A package opts in by declaring an untyped string constant:

```go
package embind

const LLGoFiles = "_wrap/emval.cpp"
```

- **`LLGoFiles`** lists the C/C++ source files to compile as part of this package. A single string names one file; multiple files use a semicolon-separated string or a parallel list.
- Paths are relative to the Go package's directory.

For example, a package that needs two native files compiles both by listing them separated by `;`:

```go
package embind

const LLGoFiles = "_wrap/emval.cpp;_wrap/emval_helpers.cpp"
```

This is a plain constant declaration — no new Go syntax is introduced. The LLGo compiler recognizes the identifier `LLGoFiles` the same way it already recognizes other package-level LLGo directives, by name, at the package level.

Because `LLGoFiles` is an ordinary Go constant, it composes naturally with Go's build-tag mechanism. Platform- or condition-specific native sources are expressed the normal Go way — a filename suffix like `_linux.go`/`_darwin.go` or an explicit `//go:build` constraint — with a different `LLGoFiles` value declared in each variant:

```go
// embind_darwin.go
//go:build darwin

package embind

const LLGoFiles = "_wrap/emval_darwin.mm"
```

```go
// embind_linux.go
//go:build linux

package embind

const LLGoFiles = "_wrap/emval_linux.cpp"
```

No separate mechanism for conditional native-source selection is needed; the same build-constraint evaluation Go already performs on the surrounding file decides which `LLGoFiles` constant is active.

### The `_wrap` convention

There is no hard requirement that native sources live in a directory named `_wrap`, but it is the recommended convention:

```
embind/
├── embind.go        // Go declarations, LLGoFiles const
└── _wrap/
    └── emval.cpp     // native glue code
```

The leading underscore keeps `go build`/`go vet` and other standard Go tooling from treating `_wrap` as a Go source directory, while still keeping the native files physically close to the package that uses them. This mirrors the long-standing Go convention of `_`-prefixed directories being ignored by the toolchain, applied here to native companion code instead of test fixtures or generated output.

### Compilation model

When LLGo builds a package that declares `LLGoFiles`:

1. Each listed file is compiled with the appropriate native compiler, selected by file extension (`clang` for `.c`, `clang++` for `.cpp`, `clang` in Objective-C mode for `.m`/`.mm`, and so on).
2. The resulting object code is linked into the same LLVM module / final binary as the package's Go-derived code.

From the perspective of a consumer of the package, nothing changes: they still `import` it like any other Go package. `LLGoFiles` is purely a build-time instruction to the LLGo compiler.

### Comparison with cgo

| | cgo | LLGoFiles |
|---|---|---|
| Native code location | Inline, in a comment above `import "C"` | Separate files, conventionally under `_wrap/` |
| Tooling for native code | Limited (lives inside a Go comment) | Full — normal `.c`/`.cpp` files |
| Multiple translation units | Awkward | Natural — list several files |
| C++ support | Partial, C-oriented | First-class |
| Go-side syntax | `import "C"` + pseudo-package `C` | Unchanged; declared via `const LLGoFiles = ...` |
| Native language support | C only | Any language clang/LLVM can compile, selected by file extension (C, C++, Objective-C, and beyond) |

LLGoFiles is not intended to replace cgo's `import "C"` mechanism for existing cgo code — LLGo continues to support that for compatibility with the official toolchain's behavior. LLGoFiles is offered as LLGo's own, additional mechanism for cases where genuine native source files, rather than inline snippets, are the natural fit. Because the mechanism is just "compile this file with the toolchain that understands its extension," it also extends more readily than cgo: adding support for a new native language is a matter of teaching the build step a new extension, not designing a new inline-embedding syntax. This matters in practice for platforms like Apple's, where wrapping system frameworks means binding against Objective-C, not just C/C++.

## Example

Wrapping a small C++ helper for an `embind`-style binding:

```
embind/
├── embind.go
└── _wrap/
    └── emval.cpp
```

`embind.go`:

```go
package embind

const LLGoFiles = "_wrap/emval.cpp"

//go:linkname emvalIncRef C.emval_incref
func emvalIncRef(handle uintptr)
```

`_wrap/emval.cpp`:

```cpp
#include "emval.h"

extern "C" void emval_incref(uintptr_t handle) {
    // native glue logic
}
```

Consumers simply `import "path/to/embind"` and use the exported Go API; the `.cpp` file is compiled and linked in automatically.

## Alternatives Considered

- **Keep using cgo's inline model exclusively.** Rejected as the primary mechanism because it does not scale well to real C++ wrappers with multiple files and degrades native-code tooling and review quality.
- **A separate build-system file (e.g. a small YAML/JSON manifest) listing native sources.** Rejected because `LLGoFiles` needs to compose with Go's own build-tag-based conditional compilation — different platforms often need a different set of native sources (e.g. one `LLGoFiles` list in a `_linux.go` file, another in a `_darwin.go` file, or files gated by `//go:build` constraints). A Go constant automatically inherits this for free, since it is declared inside an ordinary Go source file and is included or excluded by the same build-tag rules as everything else in that file. A separate manifest format would need to reinvent Go's build-constraint mechanism (or bolt on an ad hoc equivalent) to express the same per-platform variation, and would then have to be kept in sync with the Go files' own tags by hand.
- **A directory-scanning convention** (compile every `.c`/`.cpp` file found under `_wrap/` automatically, with no explicit `LLGoFiles` declaration). Rejected as the primary mechanism because it is implicit and makes the set of compiled files harder to see at a glance; an explicit constant keeps the package's native footprint self-documenting. This could still be offered later as an opt-in convenience on top of `LLGoFiles`.

## Open Questions

- Exact syntax for multiple files: semicolon-separated single string vs. a Go string slice constant (constants must currently be untyped, so a slice would require a different mechanism, e.g. a `var` recognized by name, or a delimiter convention within the string).
- Whether per-file or package-wide compiler flags are needed for the listed sources, and if so, how they'd be expressed.
- How `LLGoFiles` should interact with `go vet`/`gofmt` and other tooling that is unaware of the `_wrap` convention (informational only today, since the directory is already excluded from the Go build).
- Whether the `_wrap` directory name should be formalized as a requirement, or remain a recommended-but-not-enforced convention.

## Compatibility

This is a purely additive change. Packages that do not declare `LLGoFiles` are unaffected. Packages using cgo's `import "C"` mechanism continue to work as before; `LLGoFiles` is an alternative, not a replacement.
