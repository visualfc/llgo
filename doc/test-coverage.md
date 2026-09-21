# Test coverage

LLGo's native test runner supports Go source coverage:

```sh
llgo test -cover ./...
llgo test -covermode=count -coverprofile=cover.out ./...
llgo test -p=4 -covermode=atomic -coverpkg=./... -coverprofile=cover.out ./...
go tool cover -func=cover.out
go tool cover -html=cover.out
```

`-covermode`, `-coverpkg`, and `-coverprofile` imply `-cover`. The default mode is `set`; `count` records execution counts, and `atomic` supports concurrent counter updates. Coverage flags may appear before or after package arguments. Arguments after `-args` are passed to the test binary unchanged.

Without `-coverpkg`, reports select the packages under test. Package patterns use the selected Go toolchain's package loader, including build tags, modules, workspaces, and target settings. Test source files are not instrumented. Packages without tests follow the selected Go version's behavior. The coverage summary and profile contents for older Go releases can differ from current Go, just as they do with `go test`.

Relative profile paths are resolved from the invocation directory, or from `-outputdir` when specified. `-c` builds an instrumented test executable without creating the requested profile; run that executable with `-test.coverprofile=cover.out` to collect one. `-coverprofile` cannot be combined with `-fuzz`.

`-v` and `-json` include the coverage output in normal test output. JSON uses the selected Go toolchain's `test2json`. `testing.CoverMode`, `testing.Coverage`, and `runtime/coverage` share the standard library's registry and reporting code. In particular, Go's test coverage metadata writer is initialized during `M.Run` teardown; writer APIs called before that point have the same limitations as in Go. A covered test executable that exits from `TestMain` without running tests can emit data to `GOCOVERDIR` through its exit hooks.

## Implementation and invariants

The loader discovers the package graph first. Before the shared parse/type-check pass, independent `go tool cover` invocations run under the build's existing `-p` limit. Their generated sources are installed as stable, package-specific overlays. The subsequent SSA, backend, archive, link, and test work continues through the existing build scheduler. `-debug-trace=trace.json` includes `coverage <package>` worker spans alongside the later stages.

The selected GOROOT supplies both the instrumentation tool and the coverage runtime. Go 1.20's combined source/variable output and Go 1.20–1.22's legacy runtime callbacks are handled separately from the current protocol. Unsupported fixup strategies, counter modes, and package-ID tables fail explicitly.

Each instrumented package registers its metadata and a static table of counter ranges before its user variable initializers. These references keep the counters alive through native linking and ThinLTO/Full LTO; no object-format-specific counter sections are needed. Only package initialization mutates the runtime counter registry. Ordinary binaries do not import coverage reporting packages or invoke coverage tools; their exit hook remains unset.

Whole-package runtime replacements can discard original globals and initialization while retaining original generic functions. For such packages, original-source counters live in a small unpatched synthetic package, explicitly initialized by the test main. Profiles describe the Go sources selected by the loader, after source overlays have been applied. LLGo's additional runtime and injected standard-library implementation files are not coverage targets of `-coverpkg=all`: they are compiler implementation inputs outside that Go package graph. Replaced GOROOT code that does not execute does not gain artificial hits. In particular, an injected implementation must not be reported under a nonexistent GOROOT filename.

Generated overlay names are stable, and their contents participate in existing package fingerprints. Identical covered builds reuse archives; changing counter mode changes the generated sources and metadata. Unaffected packages can still share archives across different `-coverpkg` selections. Original source coordinates are preserved in profiles.

Every test process gets its own coverage directory and profile fragment. For `-coverpkg`, static metadata from selected test roots is ready before tests start, following `cmd/go`'s auxiliary metadata behavior. Other selected dependencies contribute when linked into a test executable. The final profile has one mode header; validated fragments are appended under a lock, including fragments produced by failing tests. A shared output filename does not serialize test processes.

## Scope and validation

This implementation enables native host test executables. Wasm and embedded/emulator targets reject coverage explicitly until their runners can transport coverage files reliably. It does not add fuzz-guidance instrumentation or `llgo build -cover`.

`go test ./internal/build -run '^TestCoverage'` exercises exact Go profile comparisons for all three modes, multi-package selection and no-test packages, JSON events, cache reuse/isolation, LTO retention, writer APIs, early exit, and failing test profiles. The multi-package fixture uses a two-process rendezvous: it fails if a common `-coverprofile` accidentally serializes the tests. Protocol validation and concurrent fragment merging have separate unit tests.
