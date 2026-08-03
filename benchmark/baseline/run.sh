#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "usage: $0 <source-root> <llgo-output> <result-directory>" >&2
  exit 2
fi

source_root="$1"
llgo_output="$2"
result_directory="$3"
harness_root="$(cd "$(dirname "$0")/../.." && pwd)"

mkdir -p "$(dirname "$llgo_output")" "$result_directory"

(
  cd "$source_root"
  LLGO_ROOT="$source_root" go build -p=1 -o "$llgo_output" ./cmd/llgo
)

(
  cd "$harness_root"
  LLGO_ROOT="$source_root" go run ./benchmark/baseline \
    -root "$source_root" \
    -llgo "$llgo_output" \
    -out "$result_directory"
)

go_results="$result_directory/go.txt"
(
  cd "$source_root"
  GOMAXPROCS=1 LLGO_ROOT="$source_root" go test \
    -run '^$' \
    -bench '^(BenchmarkMergeCompilerFlags|BenchmarkMergeLinkerFlags|BenchmarkLookupPCRandom)$' \
    -benchtime=250ms \
    -count=1 \
    -cpu=1 \
    ./internal/clang ./internal/build/funcinfo
) | tee "$go_results"

(
  cd "$source_root"
  GOMAXPROCS=1 LLGO_ROOT="$source_root" "$llgo_output" test \
    -run '^$' \
    -bench '^(BenchmarkRuntimeGetG|BenchmarkGlobal(Read|Write)|Benchmark(DirectCall|InterfaceCall|Defer|ChannelBuffered|ChannelHandoff))$' \
    -benchtime=250ms \
    -count=1 \
    ./test/llgoext
) | tee -a "$go_results"

# The current native backend creates one pthread per goroutine and intentionally
# has a bounded lifecycle stress limit. Keep creation monitoring deterministic
# instead of letting testing auto-calibrate to millions of host threads.
(
  cd "$source_root"
  GOMAXPROCS=1 LLGO_ROOT="$source_root" "$llgo_output" test \
    -run '^$' \
    -bench '^BenchmarkGoroutine$' \
    -benchtime=100x \
    -count=1 \
    ./test/llgoext
) | tee -a "$go_results"

(
  cd "$harness_root"
  go run ./benchmark/baseline \
    -mode export \
    -out "$result_directory" \
    -benchmark-output "$result_directory/benchmark.txt"
)
