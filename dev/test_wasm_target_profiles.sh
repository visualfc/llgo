#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
llgo_cmd="${LLGO:-llgo}"
node_cmd="${NODE:-node}"
fixture="${repo_root}/internal/build/testdata/wasm-profile"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/llgo-wasm-target-profiles.XXXXXX")"
trap 'rm -rf "${work_dir}"' EXIT

assert_wasm_module() {
	local module="$1"
	local magic
	magic="$(od -An -t x1 -N 4 "${module}" | tr -d '[:space:]')"
	if [[ "${magic}" != "0061736d" ]]; then
		echo "${module}: expected WebAssembly magic, got ${magic:-<empty>}" >&2
		exit 1
	fi
}

run_node() {
	local runner="$1"
	local module="$2"
	if command -v timeout >/dev/null 2>&1; then
		timeout 60s "${node_cmd}" "${repo_root}/targets/${runner}" "${module}"
	else
		"${node_cmd}" "${repo_root}/targets/${runner}" "${module}"
	fi
}

build_emscripten() {
	local target="$1"
	local name="$2"
	local module="${work_dir}/${name}.mjs"
	local runner="emscripten-runner.mjs"
	if [[ "${target}" == "emscripten-memory64" ]]; then
		runner="emscripten-memory64-runner.mjs"
	fi

	"${llgo_cmd}" build -target "${target}" -o "${module}" "${fixture}"
	assert_wasm_module "${work_dir}/${name}.wasm"
	run_node "${runner}" "${module}"
}

build_wasi() {
	local target="$1"
	local name="$2"
	local module="${work_dir}/${name}.wasm"

	"${llgo_cmd}" build -target "${target}" -o "${module}" "${fixture}"
	assert_wasm_module "${module}"
}

build_emscripten emscripten emscripten
build_emscripten emscripten-memory64 emscripten-memory64
build_emscripten wasm legacy-wasm

raw_js="${work_dir}/raw-js.mjs"
GOOS=js GOARCH=wasm "${llgo_cmd}" build -o "${raw_js}" "${fixture}"
assert_wasm_module "${work_dir}/raw-js.wasm"

# R0 verifies that both names link a genuine WASI module while preserving the
# existing host contract. R1 changes process entry, memory ownership, longjmp,
# and scheduling together, then executes the same probes under Wasmtime.
build_wasi wasi wasi
build_wasi wasip1 legacy-wasip1

raw_wasi="${work_dir}/raw-wasip1.wasm"
GOOS=wasip1 GOARCH=wasm "${llgo_cmd}" build -o "${raw_wasi}" "${fixture}"
assert_wasm_module "${raw_wasi}"

echo "WebAssembly target profile checks passed"
