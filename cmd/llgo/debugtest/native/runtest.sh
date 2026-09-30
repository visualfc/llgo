#!/bin/bash

set -euo pipefail

script_dir=$(cd "$(dirname "$0")" && pwd)

# shellcheck source=../../lldbtest/common.sh
# shellcheck disable=SC1091
source "$script_dir/../../lldbtest/common.sh"

LLGO=${LLGO:-llgo}

test_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/llgo-native-debug.XXXXXX")
trap 'rm -rf "$test_tmp_dir"' EXIT
artifact="$test_tmp_dir/native-debug.out"
optimized_artifact="$test_tmp_dir/native-debug-o2.out"

cd "$script_dir"
"$LLGO" build -O0 -o "$artifact" .
"$LLGO" build -O2 -o "$optimized_artifact" .

lldb_output=$(
    "$LLDB_PATH" --batch "$artifact" \
        -o "command script import \"$script_dir/acceptance.py\"" \
        -o "script acceptance.run_all(\"$artifact\", \"$optimized_artifact\", \"$script_dir\")" \
        2>&1
) || {
    printf '%s\n' "$lldb_output"
    exit 1
}
printf '%s\n' "$lldb_output"

if [[ "$lldb_output" == *"Traceback (most recent call last)"* ]] || \
    [[ "$lldb_output" != *"NATIVE_DEBUG_ACCEPTANCE_OK"* ]]; then
    exit 1
fi
