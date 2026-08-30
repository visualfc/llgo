# Runtime stress tests

These tests exercise timer and Unix signal correctness under loads that are too
expensive for routine pull-request testing. The leading-underscore directory
and nested module keep them out of the repository's normal `go test ./...` and
`llgo test ./test/...` patterns. No push, pull-request, scheduled, or daily
workflow runs them automatically.

Build LLGo from the revision being tested, then run the suites explicitly:

```sh
repo=$(git rev-parse --show-toplevel)
(cd "$repo" && go build -o /tmp/llgo-runtime-stress ./cmd/llgo)
export LLGO_ROOT="$repo"
cd "$repo/test/_stress"

go test -race -count=3 -timeout=20m ./runtime/timer
/tmp/llgo-runtime-stress test -count=3 -timeout=20m ./runtime/timer
/tmp/llgo-runtime-stress test -count=3 -timeout=30m ./runtime/signal
```

The signal suite is LLGo-only and targets Unix hosts. The timer suite also runs
with the standard Go compiler so the harness can be checked independently.

`LLGO_STRESS_PROFILE` controls the load while preserving the same assertions:

- `quick`: one eighth of the default counts, for local iteration.
- `default` (or unset): the checked-in baseline.
- `heavy`: twice each default dimension (and potentially more than twice the
  total work), for deliberate soak runs.

For example:

```sh
LLGO_STRESS_PROFILE=heavy /tmp/llgo-runtime-stress test \
  -count=10 -timeout=2h ./runtime/signal
```

The timer suite covers large live heaps, concurrent `Stop`/`Reset` on distinct
and shared timers, callback bursts, and concurrent sleepers. The four signal
tests cover distinct-signal delivery during a lower-number signal storm,
concurrent registration churn, repeated `Notify`/`Stop`/`Reset` barriers, and
timer progress while handlers are busy. The flood cases also exercise
the handler under concurrent delivery pressure.
