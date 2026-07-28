# LLGo baseline benchmarks

This suite is the lightweight performance gate for ordinary LLGo changes. It
uses fixed workloads and short calibrated benchmarks on Linux and macOS so it
can run on every `main` push and pull request. Branch-only series can be run
explicitly with `workflow_dispatch`, avoiding duplicate push and pull-request
jobs for the same commit. The two native jobs only measure and upload artifacts;
one trusted Linux job serially publishes both platforms into the same commit,
branch, or pull-request series.

The program workloads reuse:

- `benchmark/binary_size/cprintf`: only `lib/c.Printf`;
- `benchmark/binary_size/println`: only the built-in `println`;
- `benchmark/binary_size/fmtprintf`: `fmt.Printf`.

For each workload, the collector records median warm build time, median process
time, file size, executable/constant bytes, initialized data, and zero-filled
data. The Go benchmark stream adds selected compiler helpers and LLGo-generated
core-language operations: direct/interface calls, defer, goroutine creation,
channels, `getg`, and global/TLS/GLS access.

Before TLS/GLS directives are implemented, their benchmark variables compile as
ordinary package globals. This intentionally establishes the global-access
lower bound before the feature changes their storage. Their accessors use
atomic loads and stores so LLVM cannot replace a read with a constant; the
atomic cost is identical across the global, TLS, and GLS cases.

Results are compared against the latest `main` series during the unprivileged
benchmark workflow. A separate trusted `workflow_run` validates the artifact
against an allowlist and publishes it to the `pages` branch of
the configured data repository. Every LLGo repository defaults to
`<owner>/llgo-benchmark-data`:

```text
llgo/baseline/main
llgo/baseline/branches/<safe branch identifier>
llgo/baseline/pulls/<number>
```

The publisher never executes code from the measured revision and pull request
jobs never receive the benchmark repository token. The data repository owns
the GitHub Pages presentation; LLGo only publishes generated history and the
series index. Pull requests receive one updated summary comment linking to
their long-term trend page.

Local collection:

```sh
GOMAXPROCS=2 go build -o .benchmark/llgo ./cmd/llgo
go run ./benchmark/baseline \
  -llgo .benchmark/llgo \
  -out .benchmark/results
```

Write selected Go benchmark output to `.benchmark/results/go.txt`, then verify
the complete artifact:

```sh
go run ./benchmark/baseline -mode validate -out .benchmark/results
```
