#!/usr/bin/env bash

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export LLGO_INSTALLER_LIBRARY_ONLY=1
# shellcheck disable=SC1091
source "$root/install.sh"
export LLGO_GO_VERSION="$LLGO_GO_VERSION_DEFAULT"

fail() {
    printf 'install.sh test: %s\n' "$*" >&2
    exit 1
}

assert_equal() {
    [[ "$1" == "$2" ]] || fail "got '$1', want '$2'"
}

assert_equal "$(normalize_os Darwin)" darwin
assert_equal "$(normalize_os Linux)" linux
assert_equal "$(normalize_arch x86_64)" amd64
assert_equal "$(normalize_arch AMD64)" amd64
assert_equal "$(normalize_arch aarch64)" arm64
assert_equal "$(normalize_arch i686)" 386
assert_equal "$(normalize_version v1.2.3-rc.1)" 1.2.3-rc.1
if normalize_version '../bad' >/dev/null 2>&1; then
    fail 'unsafe version was accepted'
fi
version_at_least go1.27.0 1.27.0 || fail 'equal Go version was rejected'
version_at_least go1.28.0 1.27.0 || fail 'newer Go version was rejected'
if version_at_least go1.26.9 1.27.0; then
    fail 'older Go version was accepted'
fi

temporary="$(mktemp -d "${TMPDIR:-/tmp}/llgo-installer-test.XXXXXXXX")"
trap 'rm -rf "$temporary"' EXIT
payload="$temporary/payload"
mkdir -p "$payload/bin" "$payload/runtime"
printf '#!/usr/bin/env bash\nprintf "llgo version test\\n"\n' >"$payload/bin/llgo"
chmod +x "$payload/bin/llgo"
printf 'module github.com/xgo-dev/llgo/runtime\n' >"$payload/runtime/go.mod"
archive="$temporary/llgo.tar.gz"
tar -czf "$archive" -C "$payload" .

install_root="$temporary/root"
assert_equal "$(install_release "$install_root" linux amd64 v1.2.3 "$archive")" 1.2.3
assert_equal "$(readlink "$install_root/current")" versions/1.2.3/linux-amd64
assert_equal "$(readlink "$install_root/bin/llgo")" ../current/bin/llgo
"$install_root/bin/llgo" version >/dev/null
export LLGO_UPDATE_PATH=0
update_shell_path "$install_root"

profile_home="$temporary/home"
mkdir -p "$profile_home"
(
    export HOME="$profile_home"
    export SHELL=/bin/bash
    export LLGO_UPDATE_PATH=1
    update_shell_path "$install_root"
    update_shell_path "$install_root"
    update_shell_path "$temporary/other-root"
) >/dev/null
[[ "$(grep -c '# LLGo installer' "$profile_home/.bashrc")" == 1 ]] ||
    fail 'shell profile retained a stale installation root'
grep -Fq "$temporary/other-root/bin" "$profile_home/.bashrc" ||
    fail 'shell profile was not updated to the new installation root'

assert_equal "$(install_release "$install_root" linux amd64 1.2.4 "$archive")" 1.2.4
assert_equal "$(readlink "$install_root/current")" versions/1.2.4/linux-amd64
[[ -x "$install_root/versions/1.2.3/linux-amd64/bin/llgo" ]] ||
    fail 'installing a new version removed the old version'

dependency_calls=0
install_linux_dependencies() {
    dependency_calls=$((dependency_calls + 1))
}
ensure_go() {
    dependency_calls=$((dependency_calls + 1))
}
system_go_is_compatible() {
    return 0
}
dependency_root="$temporary/dependency-root"
install_dependencies "$dependency_root" linux amd64
install_dependencies "$dependency_root" linux amd64
assert_equal "$dependency_calls" 2

printf 'install.sh tests passed\n'
