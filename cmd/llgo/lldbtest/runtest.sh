#!/bin/bash

set -e

script_dir=$(cd "$(dirname "$0")" && pwd)

# Source common functions and variables
# shellcheck source=./common.sh
# shellcheck disable=SC1091
source "$script_dir/common.sh" || exit 1

# Parse command-line arguments
package_path="$DEFAULT_PACKAGE_PATH"
verbose=False
interactive=False
plugin_path=None

while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            verbose=True
            shift
            ;;
        -i|--interactive)
            interactive=True
            shift
            ;;
        -p|--plugin)
            plugin_path="\"$2\""
            shift 2
            ;;
        *)
            package_path="$1"
            shift
            ;;
    esac
done

# Build the project
build_project "$package_path" || exit 1

# Set up private paths for test results and auxiliary fixtures.
test_tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/llgo-lldbtest.XXXXXX")
trap 'rm -rf "$test_tmp_dir"' EXIT
result_file="$test_tmp_dir/exit-code"
result_file_for_lldb="$result_file"
if command -v cygpath >/dev/null 2>&1; then
    result_file_for_lldb=$(cygpath -m "$result_file")
fi

# Prepare LLDB commands
lldb_commands=(
    "command script import ./test.py"
    "script test.run_tests_with_result('./debug.out', ['main.go'], $verbose, $interactive, $plugin_path, '$result_file_for_lldb')"
    "quit"
)

# Prepare LLDB arguments without shell re-parsing.
lldb_args=()
for cmd in "${lldb_commands[@]}"; do
    lldb_args+=("-o" "$cmd")
done

cd "$package_path"
# Run LLDB with the embedded LLGo plugin and the test script.
llgo lldb -lldb "$LLDB_PATH" -- "${lldb_args[@]}"

# Read the exit code from the result file
if [ -f "$result_file" ]; then
    exit_code=$(cat "$result_file")
    rm "$result_file"
else
    echo "Error: Could not find exit code file"
    exit 1
fi

if [ "$exit_code" -ne 0 ]; then
    exit "$exit_code"
fi

llgo lldb -lldb "$LLDB_PATH" -- --batch "./debug.out" \
    -o 'script import os; info = llgo_plugin.inspect_target(lldb.target); (info.schema_version == 1 and info.runtime_layout_version == 1 and info.pointer_size == lldb.target.GetAddressByteSize() and info.byte_order != "unknown") or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo status", result); (result.Succeeded() and "LLGo debugger schema v1 (runtime layout v1)" in result.GetOutput()) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo vars", result); (not result.Succeeded() and "requires a stopped process" in result.GetError()) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo print s", result); (not result.Succeeded() and "requires a stopped process" in result.GetError()) or os._exit(1)'

# The LLGo formatter must not attach itself to an ordinary C target.
non_llgo_dir="$test_tmp_dir/non-llgo"
host_exe_ext=$(go env GOEXE)
mkdir -p "$non_llgo_dir"
printf 'typedef struct { const char *data; unsigned long len; } string; string cstring = {"raw", 3}; int main(void) { return 0; }\n' | \
    "${CC:-cc}" -x c -g -o "$non_llgo_dir/non-llgo$host_exe_ext" -
llgo lldb -lldb "$LLDB_PATH" -- --batch "$non_llgo_dir/non-llgo$host_exe_ext" \
    -o 'script import os; info = llgo_plugin.inspect_target(lldb.target); (not info.marker_versions and not info.supported) or os._exit(1)' \
    -o 'script import os; value = lldb.target.FindFirstGlobalVariable("cstring"); (value.IsValid() and value.GetSummary() is None and value.GetNumChildren() == 2) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo status", result); (result.Succeeded() and "Not an LLGo target" in result.GetOutput()) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("p 1+1", result); (result.Succeeded() and "2" in result.GetOutput()) or os._exit(1)'

# An unknown marker must disable only LLGo-specific presentation.
printf 'typedef struct { const char *data; unsigned long len; } string; string cstring = {"raw", 3}; __attribute__((used)) int __llgo_debugger_marker_v2 = 2; int main(void) { return 0; }\n' | \
    "${CC:-cc}" -x c -g -o "$non_llgo_dir/unsupported-llgo$host_exe_ext" -
llgo lldb -lldb "$LLDB_PATH" -- --batch "$non_llgo_dir/unsupported-llgo$host_exe_ext" \
    -o 'script import os; info = llgo_plugin.inspect_target(lldb.target); (info.marker_versions == (2,) and not info.supported) or os._exit(1)' \
    -o 'script import os; value = lldb.target.FindFirstGlobalVariable("cstring"); (value.IsValid() and value.GetSummary() is None and value.GetNumChildren() == 2) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo status", result); (result.Succeeded() and "Unsupported LLGo debugger marker version(s): v2" in result.GetOutput()) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo vars", result); (not result.Succeeded() and "Unsupported LLGo debugger marker version(s): v2" in result.GetError()) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("p 1+1", result); (result.Succeeded() and "2" in result.GetOutput()) or os._exit(1)'

# Multiple marker versions are ambiguous even when one version is supported.
printf 'typedef struct { const char *data; unsigned long len; } string; string cstring = {"raw", 3}; __attribute__((used)) int __llgo_debugger_marker_v1 = 1; __attribute__((used)) int __llgo_debugger_marker_v2 = 2; int main(void) { return 0; }\n' | \
    "${CC:-cc}" -x c -g -o "$non_llgo_dir/ambiguous-llgo$host_exe_ext" -
llgo lldb -lldb "$LLDB_PATH" -- --batch "$non_llgo_dir/ambiguous-llgo$host_exe_ext" \
    -o 'script import os; info = llgo_plugin.inspect_target(lldb.target); (info.marker_versions == (1, 2) and not info.supported) or os._exit(1)' \
    -o 'script import os; value = lldb.target.FindFirstGlobalVariable("cstring"); (value.IsValid() and value.GetSummary() is None and value.GetNumChildren() == 2) or os._exit(1)' \
    -o 'script import os; result = lldb.SBCommandReturnObject(); lldb.debugger.GetCommandInterpreter().HandleCommand("llgo status", result); (result.Succeeded() and "Unsupported LLGo debugger marker version(s): v1, v2" in result.GetOutput()) or os._exit(1)'
