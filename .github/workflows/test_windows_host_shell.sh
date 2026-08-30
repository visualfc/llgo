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

unset CC CXX
executable="$source_dir/host-shell.exe"
trace=$(cd "$source_dir" && MSYS2_ARG_CONV_EXCL='*' "$llgo" build -x -o "$executable" . 2>&1)
if ! grep -Fq -- "$target" <<<"$trace"; then
  echo "host shell did not retain target $target:" >&2
  echo "$trace" >&2
  exit 1
fi

output=$($executable 2>&1)
want="windows-${LLGO_WINDOWS_ABI}-${LLGO_WINDOWS_ARCH}-host-shell"
if [[ "$output" != "$want" ]]; then
  echo "$executable printed '$output', want '$want'" >&2
  exit 1
fi

readobj=$(command -v llvm-readobj.exe || command -v llvm-readobj)
imports=$($readobj --coff-imports "$executable")
forbidden='(msys-2\.0|cygwin1)\.dll'
if [[ "$LLGO_WINDOWS_ABI" == msvc ]]; then
  forbidden='(msys-2\.0|cygwin1|libwinpthread)\.dll'
fi
if grep -Eqi "$forbidden" <<<"$imports"; then
  echo "host-shell build has an unsupported POSIX-emulation dependency:" >&2
  echo "$imports" >&2
  exit 1
fi
