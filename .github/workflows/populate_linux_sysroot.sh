#!/bin/bash

set -e

# Pin the architecture-specific images from the same Debian 12 index
# (sha256:6ebd97fa83deb272194a2cf015b3d26a4d538e9ad3a7a79d544c8af5b0a01443).
# Refresh the index and child digests with `docker buildx imagetools inspect debian:12`.
# Docker's classic image store cannot retain two platforms under one index
# digest. The GCC 12 paths in .goreleaser.yaml must match these sysroots.
LINUX_AMD64_IMAGE=debian:12@sha256:2f65600e1252c5649d2213e1d1ea4d74253d26514dc6530102a875e429245929
LINUX_ARM64_IMAGE=debian:12@sha256:5eac3978974cfa26a880057766c683e55c5763355d30a8beecbd263e0e1621d9

TMPDIR="$(mktemp -d)"
export TMPDIR
trap 'rm -rf "${TMPDIR}"' EXIT

LINUX_AMD64_PREFIX=.sysroot/linux/amd64
LINUX_ARM64_PREFIX=.sysroot/linux/arm64
mkdir -p "${LINUX_AMD64_PREFIX}" "${LINUX_ARM64_PREFIX}"

POPULATE_LINUX_SYSROOT_SCRIPT="$(mktemp)"
cat > "${POPULATE_LINUX_SYSROOT_SCRIPT}" << EOF
#!/bin/bash

set -e

export DEBIAN_FRONTEND=noninteractive

apt-get -o Acquire::Retries=3 update
apt-get -o Acquire::Retries=3 install -y build-essential zlib1g-dev rsync
dpkg-query -W libc6 libc6-dev gcc g++ libstdc++6
test -f /usr/include/c++/12/string
GCC_TRIPLE="\$(gcc -dumpmachine)"
test -d "/usr/include/\${GCC_TRIPLE}/c++/12"
test -d "/usr/lib/gcc/\${GCC_TRIPLE}/12"

error() {
	echo -e "\$1" >&2
	exit 1
}

exclude_list=()
include_list=()

exclude_list+=(--exclude "/bin")
exclude_list+=(--exclude "/boot")
exclude_list+=(--exclude "/boot*")
exclude_list+=(--exclude "/dev")
exclude_list+=(--exclude "/etc")
exclude_list+=(--exclude "/home")
exclude_list+=(--exclude "/lib/dhcpd")
exclude_list+=(--exclude "/lib/firmware")
exclude_list+=(--exclude "/lib/hdparm")
exclude_list+=(--exclude "/lib/ifupdown")
exclude_list+=(--exclude "/lib/modules")
exclude_list+=(--exclude "/lib/modprobe.d")
exclude_list+=(--exclude "/lib/modules-load.d")
exclude_list+=(--exclude "/lib/resolvconf")
exclude_list+=(--exclude "/lib/startpar")
exclude_list+=(--exclude "/lib/systemd")
exclude_list+=(--exclude "/lib/terminfo")
exclude_list+=(--exclude "/lib/udev")
exclude_list+=(--exclude "/lib/xtables")
exclude_list+=(--exclude "/lib/ssl/private")
exclude_list+=(--exclude "/lost+found")
exclude_list+=(--exclude "/media")
exclude_list+=(--exclude "/mnt")
exclude_list+=(--exclude "/proc")
exclude_list+=(--exclude "/root")
exclude_list+=(--exclude "/run")
exclude_list+=(--exclude "/sbin")
exclude_list+=(--exclude "/srv")
exclude_list+=(--exclude "/sys")
exclude_list+=(--exclude "/sysroot")
exclude_list+=(--exclude "/tmp")
exclude_list+=(--exclude "/usr/bin")
exclude_list+=(--exclude "/usr/games")
exclude_list+=(--exclude "/usr/sbin")
exclude_list+=(--exclude "/usr/share")
exclude_list+=(--exclude "/usr/src")
exclude_list+=(--exclude "/usr/local/bin")
exclude_list+=(--exclude "/usr/local/etc")
exclude_list+=(--exclude "/usr/local/games")
exclude_list+=(--exclude "/usr/local/man")
exclude_list+=(--exclude "/usr/local/sbin")
exclude_list+=(--exclude "/usr/local/share")
exclude_list+=(--exclude "/usr/local/src")
exclude_list+=(--exclude "/usr/lib/ssl/private")
exclude_list+=(--exclude "/var")
exclude_list+=(--exclude "/snap")
exclude_list+=(--exclude "*python*")

include_list+=(--include "*.a")
include_list+=(--include "*.o")
include_list+=(--include "*.so")
include_list+=(--include "*.so.*")
include_list+=(--include "*.h")
include_list+=(--include "*.hh")
include_list+=(--include "*.hpp")
include_list+=(--include "*.hxx")
include_list+=(--include "*.pc")
include_list+=(--include "*.def")
include_list+=(--include "*.inc")
include_list+=(--include "/lib")
include_list+=(--include "/lib32")
include_list+=(--include "/lib64")
include_list+=(--include "/libx32")
# libstdc++ has extensionless headers such as string, optional, and type_traits.
include_list+=(--include "/usr/include/c++/***")
include_list+=(--include "/usr/include/*-linux-gnu/c++/***")
include_list+=(--include "*/")

do-sync() {
	from=\$1
	to=\$2

	args=()
	args+=(-a)
	args+=(-z)
	args+=(-m)
	args+=(-d)
	args+=(-h)
	args+=(--keep-dirlinks)
	args+=(--delete)
	args+=(--prune-empty-dirs)
	args+=(--sparse)
	args+=(--links)
	args+=(--copy-unsafe-links)
	args+=("\${exclude_list[@]}")
	args+=("\${include_list[@]}")
	args+=(--exclude "*")
	args+=("\${from}")
	args+=("\${to}")

	echo "\${args[@]}"
	rsync "\${args[@]}"

	return \$?
}

do-sync / /sysroot/
EOF
chmod +x "${POPULATE_LINUX_SYSROOT_SCRIPT}"

populate_linux_sysroot() {
	local ARCH="$1"
	local PREFIX="$2"
	local IMAGE="$3"
	docker run \
		--rm \
		--platform "linux/${ARCH}" \
		-v "$(pwd)/${PREFIX}":/sysroot \
		-v "${POPULATE_LINUX_SYSROOT_SCRIPT}":/populate_linux_sysroot.sh:ro \
		"${IMAGE}" \
		/populate_linux_sysroot.sh
}
# Populate serially to bound the memory and disk pressure of extraction.
populate_linux_sysroot amd64 "${LINUX_AMD64_PREFIX}" "${LINUX_AMD64_IMAGE}"
populate_linux_sysroot arm64 "${LINUX_ARM64_PREFIX}" "${LINUX_ARM64_IMAGE}"
