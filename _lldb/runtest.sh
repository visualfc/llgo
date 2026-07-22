#!/bin/bash

set -e

script_dir=$(cd "$(dirname "$0")" && pwd)

# Source common functions and variables
# shellcheck source=./_lldb/common.sh
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

# Set up the result file path
result_file="/tmp/lldb_exit_code"

# Prepare LLDB commands
lldb_commands=(
    "command script import ../llgo_plugin.py"
    "command script import ../test.py"
    "script test.run_tests_with_result('./debug.out', ['main.go'], $verbose, $interactive, $plugin_path, '$result_file')"
    "quit"
)

# Run LLDB with prepared commands
lldb_command_string=""
for cmd in "${lldb_commands[@]}"; do
    lldb_command_string+=" -o \"$cmd\""
done

cd "$package_path"
# Run LLDB with the test script
eval "$LLDB_PATH $lldb_command_string"

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

# The LLGo formatter must not attach itself to an ordinary C target.
non_llgo_dir=$(mktemp -d)
trap 'rm -rf "$non_llgo_dir"' EXIT
printf 'int main(void) { return 0; }\n' | \
    "${CC:-cc}" -x c -g -o "$non_llgo_dir/non-llgo" -
"$LLDB_PATH" --batch "$non_llgo_dir/non-llgo" \
    -o "command script import \"$script_dir/llgo_plugin.py\"" \
    -o 'script assert not llgo_plugin.is_llgo_compiler(lldb.target)'
