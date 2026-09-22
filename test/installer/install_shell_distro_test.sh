#!/usr/bin/env bash

set -euo pipefail

repository="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [[ "$#" == 0 ]]; then
    set -- ubuntu:24.04 debian:12 fedora:latest archlinux:latest alpine:latest opensuse/tumbleweed:latest
fi

# Only shell startup is tested here. Keep compiler builds, native dependency
# installation, and real release execution in their existing platform jobs.
status=0
for image in "$@"; do
    case "$image" in
        ubuntu:* | debian:*)
            setup='apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends bash zsh fish tar gzip'
            ;;
        fedora:*)
            setup='dnf install -y --setopt=install_weak_deps=False bash zsh fish tar gzip'
            ;;
        archlinux:*)
            setup='pacman -Syu --needed --noconfirm bash zsh fish tar gzip'
            ;;
        alpine:*)
            setup='apk add --no-cache bash zsh fish tar gzip'
            ;;
        opensuse/*:*)
            setup='zypper --non-interactive refresh && zypper --non-interactive install --no-recommends bash zsh fish tar gzip'
            ;;
        *) printf 'Unsupported shell-test image: %s\n' "$image" >&2; exit 1 ;;
    esac
    printf '::group::Installer shell startup: %s\n' "$image"
    if docker run --rm -v "$repository:/src:ro" -w /src "$image" \
        sh -ec "$setup; exec bash test/installer/install_shell_test.sh bash zsh fish sh"; then
        printf 'PASS %s\n' "$image"
    else
        printf 'FAIL %s\n' "$image" >&2
        status=1
    fi
    printf '::endgroup::\n'
done
exit "$status"
