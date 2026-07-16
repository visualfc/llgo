# PCLN metadata packaging

Status: initial implementation. This design is stacked on the typed Go linker
options introduced by #2113.

## Goals

LLGo supports three independent packaging policies for the runtime function
and line metadata used by `runtime.Caller`, `runtime.Callers`,
`runtime.FuncForPC`, and stack traces or pprof symbolization on the available
unwinding paths:

- `-pclntab=embedded` keeps the metadata in the executable. This is the
  default and preserves existing behavior.
- `-pclntab=external` writes `<executable>.pclntab`. The executable loads that
  optional file on the first safe symbolization request. A missing, stale, or
  corrupt file is a cached soft failure; the program continues with the
  available native fallback.
- `-pclntab=none` does not generate the metadata and does not link the external
  loader or its path/probe code.

The policy is deliberately separate from Go's `-ldflags=-s` and `-w`:

- `-w` controls DWARF debug information.
- `-s` controls the native symbol table and implies `-w` unless `-w` is
  explicitly set; the explicit `-w` value wins regardless of argument order.
- `-pclntab` controls LLGo runtime symbolization metadata.

Consequently all combinations are meaningful. For example,
`-ldflags=-s -pclntab=embedded` keeps Go-compatible runtime symbolization,
while `-ldflags=-s -pclntab=external` keeps it in an optional deployment
artifact.

## Package boundaries

User-facing syntax is parsed in `cmd/internal/flags`. The command layer writes
only typed `internal/build.Config` fields; the build package does not parse
command strings.

`internal/build` validates the selected target, enables compiler metadata,
coordinates link finalization, and owns output paths. `internal/pclnpost`
contains host-side ELF/Mach-O inspection and safe binary rewriting.
`internal/pclnmap` owns the versioned sidecar format. The runtime contains a
small build-tag-selected loader and an independent, dependency-minimal
decoder.

This separation keeps both binary-format policy and command syntax out of the
compiler pipeline.

## Link and strip order

Externalization needs final linked PCs and the native symbol table to remove
LTO inline copies. It therefore cannot be implemented only by suppressing IR
generation. The required final-artifact order is:

1. Generate funcinfo and line records, but keep the external payload out of
   the synthetic main module.
2. Link the final executable with temporary link-site sections and a fixed
   identity slot.
3. Analyze the unmodified executable, join final PCs to the encoded records,
   and stage the sidecar.
4. Write the executable identity and clear link-only site data.
5. Apply any future native symbol-table removal requested by `-s`.
6. Perform final platform signing after all executable mutations.
7. Atomically publish the sidecar, then report the executable and sidecar as
   separate outputs.

#2113 currently implements typed `-s`/`-w` intent and DWARF omission; native
symbol-table removal is a later phase. When that phase is added, it must use
the ordering above rather than adding an independent post-link rewrite before
PCLN analysis. The current external detach step signs a mutated Mach-O itself;
that signing responsibility must move to the end of the unified pipeline when
a later native-strip mutation is added.

`-pclntab=none` takes the generation-time path: compiler metadata and link
sites are not produced. `-pclntab=embedded` keeps the existing post-link
prebuilt-table rewrite.

Darwin embedded builds that emit DWARF keep the historical site-free path so
inline PC anchors do not disturb LLDB lexical scopes. Linux retains sites with
DWARF because most Go symbols are intentionally absent from ELF `.dynsym`, so
`dlsym` alone cannot reconstruct every function entry. External mode needs
the final-PC sites on both platforms before it can construct the sidecar.

## Sidecar identity and addressing

The sidecar header contains a format version, runtime ABI version, target
tuple, pointer size, section descriptors, payload checksum, and the SHA-256
identity of the exact linked executable before detachment. The same identity
is written into a dedicated 32-byte executable section. A sidecar is accepted
only when both values match. It is a pre-mutation pairing token, not the digest
of the final detached, stripped, or signed executable.

All code addresses are stored relative to the lowest file-backed load segment.
At runtime they are relocated with the image base returned by `dladdr`, so the
format works for PIE/ASLR and non-PIE executables without relocations in the
sidecar.

## Loading and failure semantics

Only allocation and I/O-safe public symbolization paths may start a load.
Fatal signal handling never loads or waits; it immediately uses the available
fallback while a load is absent or in progress.

The loader has four states: unattempted, loading, loaded, and unavailable.
One caller performs validation and index construction. Concurrent callers
wait only on safe paths. Both success and failure are terminal and cached, so
the filesystem is never probed repeatedly.

The initial format is intentionally private to LLGo and versioned. It may grow
additional independently removable symbol classes after the PCLN path has
proved stable on ELF and Mach-O.
