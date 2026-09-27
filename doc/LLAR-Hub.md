LLAR Hub: Seamless C/C++ Dependency Management for LLGo
=====

## Design Proposal

## 1. Vision

A Go developer using LLGo to depend on a C/C++ open-source project should never have to leave the Go toolchain's mental model. Fetching a dependency should still mean `go get`; building should still mean `llgo build`/`install`/`test`. No separate, manually-invoked step should be required to obtain the native binaries a dependency needs. Concretely, the target experience is:

```
go get llar.io/zlib
llgo build      # or: llgo install / llgo test
```

That's it — from the developer's point of view, `llar.io/zlib` behaves like any other Go module.

This document specifies the capabilities each component needs to provide to support that experience, and how those capabilities compose. It describes the target design purely at the level of what each piece is responsible for, without distinguishing what already exists from what remains to be built.

## 2. Component Responsibilities

Four components divide the responsibility:

1. **`llgo` — native call interop.** Lets Go code reference a C/C++ function directly, via `//go:linkname` or `//llgo:link`, and, at build/link time, drives resolution of whatever native artifacts that code depends on.
2. **`llar` — the native formula and install mechanism.** Defines a *formula* format, built on the XGo Class Framework, describing how to obtain and lay out a C/C++ project's distribution package. `llar install` fetches that package, but — as detailed in §3 — it serves two distinct audiences differently.
3. **`llcppg` — Go binding generation.** Given a C/C++ project's `include` headers, generates a corresponding Go package. That package is self-contained — it does not depend on the original headers at build time — with one exception: inline functions. Simple inline functions are translated directly into Go; complex ones are wrapped by a generated C shim under a `_wrap` directory, in which case the headers that shim needs travel with the Go package itself, under `_wrap/include`.
4. **The `llarhub` index — project naming and registration.** A registry, described in §4–5, that assigns each supported C/C++ project a canonical short name and coordinates where its formula, its llcppg configuration, and its generated Go package live, while also serving lookups from the C/C++-facing identifiers described in §3.

## 3. Two Audiences of `llar install`

`llar install` is used by two different kinds of users, who supply different identifiers and receive different artifacts:

| | Identifier used | Aware of `proj` short name? | Package fetched |
|---|---|---|---|
| **C/C++ user** | `llar install user/repo` (or `domain/repo`, `domain/user/repo`) | No | Full distribution: `bin` + `lib` + `include` |
| **LLGo user** | `llar install proj` or `llar install llar.io/proj` | Yes | Scoped distribution: `bin` + `lib` only |

A C/C++ user is someone consuming a native project directly, outside of any Go/LLGo context — they think in terms of the project's own source identity (`user/repo`, etc.) and expect the complete distribution, headers included, exactly as `llar` has always provided it. They have no reason to know, or need to know, that the project also has a short name.

An LLGo user never supplies a raw source identifier at all. They arrive at `llar install` indirectly — via the link-time trigger described in §7 — using the project's short name or its `llar.io/proj` form. Since the corresponding `llcppg`-generated Go package is already self-contained, only `bin` + `lib` are fetched; `include` is intentionally omitted from this path.

Both entry points ultimately resolve against the same underlying formula(s) for the project — they differ in identifier, in audience, and in which artifacts are returned, not in the formula itself.

## 4. Naming Model

Each supported project has exactly one canonical short name, `proj`:

- **`llar.io/proj`** is the identity an LLGo user ever writes — in `go get`, in `import`, in `llar install`. Within the Go ecosystem, where a single unique package path per project is the expected convention, this is that project's identity.
- **`github.com/llarhub/proj`** is the *physical hosting location* of the generated Go package behind that identity — where `go get` actually fetches source from. It is an implementation detail an LLGo user is not expected to reference directly.
- The project's original source identifier (`user/repo`, `domain/repo`, or `domain/user/repo`) remains its identity for a C/C++ user, per §3, and is never replaced by the short name from that user's point of view.

## 5. The `.index` Registry: Two Coexisting Lookup Structures

`github.com/llarhub/.index` has to serve both audiences from §3, which need to look a project up by two different keys. It does so with two structures living side by side in the same repository:

### 5.1 Directory layout, keyed by short name

Physically, the repository's top level (aside from `.github`) contains one directory per project, named after its short name:

```
.index/
  zlib/
  libpng/
  openssl/
  ...
```

This matches exactly how an LLGo user's request already arrives — as `proj` — so resolving `llar.io/proj` or `llar install proj` is a direct lookup into `.index/proj/`.

### 5.2 `llarhub.toc`, keyed by source identifier

A C/C++ user, however, arrives with a source identifier, not a short name — and the directory layout above gives them no direct way in. A single file at the repository root, **`llarhub.toc`**, closes that gap with one line per registered project, mapping source identifier to short name:

```
user/repo proj1
domain/user/repo proj2
domain/repo proj3
```

`llar install user/repo` looks up `user/repo` in `llarhub.toc` to find `proj1`, then proceeds into `.index/proj1/` exactly as the short-name path would.

This file is never hand-maintained: **cibot regenerates it automatically** whenever a PR to `.index` is merged (see §6.2), so the two lookup structures never drift out of sync with each other.

## 6. Onboarding a New C/C++ Project

Before a project can be depended on as `llar.io/proj` — or fetched by a C/C++ user under its own identifier — someone has to register it. This section specifies that process.

### 6.1 Submission

A maintainer (of the C/C++ project, or anyone wishing to add support for it) opens a PR against `github.com/llarhub/.index`, adding or updating the project's directory, `proj/` (per §5.1). That directory carries:

1. **Source address** — the project's origin, in one of the three forms `user/repo`, `domain/user/repo`, or `domain/repo`.
2. **Short name** — `proj`, doubling as the directory name itself.
3. **llar formula(s)** — one or more formulas describing how to build/fetch the project's native distribution, using existing llar formula semantics as-is. Different versions of the project may require different formulas.
4. **llcppg configuration** — whatever configuration `llcppg` needs to generate the project's Go package from its headers.
5. **Metadata** (optional but encouraged) — a description, homepage/website link, and any other information useful to someone browsing or evaluating the project.

The PR does not touch `llarhub.toc` — that mapping is derived, not authored (see §5.2).

### 6.2 Automated provisioning after merge

Once a PR is merged, **cibot** watches `.index` for additions and changes. For each affected `proj`, it:

1. Regenerates `llarhub.toc` so the new (or updated) source-identifier-to-short-name mapping is immediately available to C/C++-style lookups.
2. If `proj` is new, creates the corresponding `github.com/llarhub/proj` repository.
3. Builds and publishes an initial set of Go package versions into it, by running `llcppg` against the registered source using the submitted configuration and formula(s).

From that point on, `llar.io/proj` resolves to `github.com/llarhub/proj` for LLGo users, `llar install user/repo` resolves via `llarhub.toc` for C/C++ users, and the project becomes `go get`-able like any other dependency described in §7.

This gives the overall system a clean separation: **registration** (a reviewed, human-in-the-loop PR to `.index`) is decoupled from **provisioning** (an automated, repeatable step performed by cibot), so that adding support for a new library is a one-time, auditable act rather than an ad hoc manual process.

## 7. The Dependency, Split in Two

`llar.io/zlib` is not one dependency — it is two layered together, and each has a natural owner:

| Layer | What it is | Resolved by |
|---|---|---|
| Go binding package | Pure Go source generated by `llcppg`, physically hosted at `github.com/llarhub/zlib` | `go get`, via ordinary Go module resolution |
| Native distribution | Platform-specific `bin` + `lib` (scoped per §3, `include` not fetched here) | `llar install llar.io/zlib` |

The workflow in §8 does not change *how* either layer is resolved — it changes *when* the second layer's resolution is triggered, and by whom.

## 8. Proposed Workflow

### 8.1 `go get llar.io/zlib` — unchanged, Go-native

`go get` resolves `llar.io/zlib` to its physical location, `github.com/llarhub/zlib`, and downloads the Go package from there, exactly as it would any other Go module. No llgo- or llar-specific logic is involved at this stage.

### 8.2 `llgo build`/`install`/`test` — link-time native resolution

When the build reaches the link stage and `llgo` determines that the binary depends on `llar.io/zlib`, it transparently runs, on the developer's behalf:

```
llar install llar.io/zlib
```

This is the LLGo-user path from §3: because it goes through the short-name identifier, the fetch naturally scopes to `bin` + `lib` only, with no separate step needed to discard `include`.

From the developer's perspective this step is invisible: `llgo build` simply works, without any prior manual `llar install`.

## 9. Design Considerations

### 9.1 Triggering signal

`llgo` needs a reliable way to tell, from the Go module graph, that a given import is llar-backed. Two candidate signals:

- **Path convention** — treat any import resolving through `llar.io/...` as llar-backed.
- **In-package marker** — have `llcppg` embed metadata (e.g. the corresponding short name `proj`) inside the generated package, which `llgo` reads at build time.

The marker-based approach is more robust: it ties the trigger to the artifact itself rather than to a naming convention, and does not assume every native dependency is necessarily surfaced through `llar.io`.

### 9.2 Artifact placement and versioning

The automatic `llar install` needs a deterministic, cacheable install location, conceptually similar to the Go module cache, so that:

- the link step can locate `lib` unambiguously;
- the native distribution version fetched stays pinned to the Go module version `go get` resolved, extending `go.sum`-style reproducibility to the native side;
- multiple projects depending on the same `llar.io/zlib` version can share one cached copy.

### 9.3 Failure diagnostics

Because `llar install` is invoked implicitly, its failures (network errors, missing formula, unsupported platform) must surface through `llgo build`/`install`/`test` clearly enough that the developer understands the failure is on the native-dependency side, and which project/version is at fault.

### 9.4 Platform matrix

`bin`/`lib` artifacts are platform- and architecture-specific; the implicit install step must resolve the correct artifact for the host (or cross-compilation target) without manual input in the common case.

## 10. Summary

Each component keeps a narrow, well-defined responsibility:

- **`.index`** serves both `llar install` audiences from a single source of truth — a short-name-keyed directory layout for LLGo users, and a derived `llarhub.toc` mapping for C/C++ users — kept in sync automatically by cibot.
- **A reviewed PR to `.index`** registers a project; **cibot** turns that registration into a `go get`-able Go package and an up-to-date `llarhub.toc`.
- **`llar.io/proj`** gives an LLGo user one canonical, Go-idiomatic identity for the project, independent of both its physical hosting and its native source identifier.
- **`go get`** resolves the Go binding layer exactly as it always has.
- **`llgo`**, at link time, resolves the native distribution layer by delegating to `llar install llar.io/proj`, which — by virtue of going through the short-name path — naturally scopes to just `bin` + `lib`.

The result, from the developer's chair, is indistinguishable from depending on any ordinary Go module:

```
go get llar.io/zlib
llgo build
```
