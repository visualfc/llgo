#!/usr/bin/env bash

set -euo pipefail

: "${RUNNER_TEMP:?RUNNER_TEMP is required}"
: "${LLGO_WINDOWS_ABI:?LLGO_WINDOWS_ABI is required}"
: "${LLGO_WINDOWS_ARCH:?LLGO_WINDOWS_ARCH is required}"

if [[ -n "${LLGO_TEST_COMPILER:-}" ]]; then
  llgo=$(cygpath -u "$LLGO_TEST_COMPILER")
else
  llgo=$(command -v llgo.exe || command -v llgo)
fi
temp_root=$(cygpath -u "$RUNNER_TEMP")
source_dir=$(mktemp -d "$temp_root/llgo-windows-host-shell.XXXXXX")
trap 'rm -rf "$source_dir"' EXIT

cat >"$source_dir/go.mod" <<'EOF'
module example.com/llgo-windows-host-shell

go 1.27
EOF
cat >"$source_dir/main.go" <<EOF
package main

func main() {
	println("windows-${LLGO_WINDOWS_ABI}-${LLGO_WINDOWS_ARCH}-host-shell")
}
EOF

case "$LLGO_WINDOWS_ARCH" in
  386) target_arch=i686 ;;
  amd64) target_arch=x86_64 ;;
  arm64) target_arch=aarch64 ;;
  *) echo "unsupported Windows architecture: $LLGO_WINDOWS_ARCH" >&2; exit 1 ;;
esac
case "$LLGO_WINDOWS_ABI" in
  msvc) target="$target_arch-pc-windows-msvc" ;;
  mingw) target="$target_arch-w64-windows-gnu" ;;
  *) echo "unsupported Windows ABI: $LLGO_WINDOWS_ABI" >&2; exit 1 ;;
esac

prepend_runtime_dir() {
  local native_dir=${1:-}
  if [[ -n "$native_dir" ]]; then
    PATH="$(cygpath -u "$native_dir"):$PATH"
  fi
}
case "$LLGO_WINDOWS_ABI" in
  mingw)
    # Cygwin does not reliably preserve MSYS2's native DLL search entries.
    # Reapply the activated MinGW runtime profile using this shell's path
    # syntax; target-architecture DLLs must remain ahead of the host profile.
    [[ -z "${LLGO_MINGW_HOST_ROOT:-}" ]] || prepend_runtime_dir "$LLGO_MINGW_HOST_ROOT/bin"
    prepend_runtime_dir "${LLGO_MINGW_TARGET_RUNTIME_BIN:-}"
    [[ -z "${LLGO_MINGW_TARGET_VCPKG_ROOT:-}" ]] || prepend_runtime_dir "$LLGO_MINGW_TARGET_VCPKG_ROOT/bin"
    ;;
  msvc)
    [[ -z "${LLGO_WINDOWS_VCPKG_ROOT:-}" ]] || prepend_runtime_dir "$LLGO_WINDOWS_VCPKG_ROOT/bin"
    ;;
esac
export PATH

# An amd64 profile can be rediscovered from the shell's native Clang. Other
# architectures deliberately keep the compiler selected by target activation:
# the shell's native Clang may identify the right ABI but does not necessarily
# contain the requested architecture's CRT and libraries.
if [[ "$LLGO_WINDOWS_ARCH" == amd64 ]]; then
  unset CC CXX
fi
executable="$source_dir/host-shell.exe"
set +e
trace=$(cd "$source_dir" && "$llgo" build -x -o "$executable" . 2>&1)
build_status=$?
set -e
if [[ $build_status -ne 0 ]]; then
  echo "host-shell build failed with exit code $build_status:" >&2
  echo "$trace" >&2
  exit "$build_status"
fi
if ! grep -Fq -- "$target" <<<"$trace"; then
  echo "host shell did not retain target $target:" >&2
  echo "$trace" >&2
  exit 1
fi

set +e
output=$($executable 2>&1)
run_status=$?
set -e
if [[ $run_status -ne 0 ]]; then
  echo "$executable failed with exit code $run_status:" >&2
  echo "$output" >&2
  exit "$run_status"
fi
want="windows-${LLGO_WINDOWS_ABI}-${LLGO_WINDOWS_ARCH}-host-shell"
if [[ "$output" != "$want" ]]; then
  echo "$executable printed '$output', want '$want'" >&2
  exit 1
fi

readobj=$(command -v llvm-readobj.exe || command -v llvm-readobj)
set +e
imports=$($readobj --coff-imports "$executable" 2>&1)
readobj_status=$?
set -e
if [[ $readobj_status -ne 0 ]]; then
  echo "$readobj failed with exit code $readobj_status:" >&2
  echo "$imports" >&2
  exit "$readobj_status"
fi
forbidden='(msys-2\.0|cygwin1)\.dll'
if [[ "$LLGO_WINDOWS_ABI" == msvc ]]; then
  forbidden='(msys-2\.0|cygwin1|libwinpthread)\.dll'
fi
if grep -Eqi "$forbidden" <<<"$imports"; then
  echo "host-shell build has an unsupported POSIX-emulation dependency:" >&2
  echo "$imports" >&2
  exit 1
fi
