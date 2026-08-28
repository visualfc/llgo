#!/bin/bash
set -euo pipefail

# Installation directory (from argument or default)
INSTALL_DIR="${1:-.cache/qemu}"

# Detect platform
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
EXE_SUFFIX=""

# Map architecture names
case "$ARCH" in
  x86_64|amd64)
    ARCH="x86_64"
    ;;
  aarch64|arm64)
    ARCH="aarch64"
    ;;
  *)
    echo "Unsupported architecture: $ARCH"
    exit 1
    ;;
esac

# Map OS names
case "$OS" in
  darwin)
    PLATFORM="${ARCH}-apple-darwin"
    ;;
  linux)
    PLATFORM="${ARCH}-linux-gnu"
    ;;
  mingw*|msys*|cygwin*)
    # Espressif publishes the Windows emulator as an x86-64 MinGW host
    # binary. This is independent of the ESP firmware target architecture.
    PLATFORM="x86_64-w64-mingw32"
    EXE_SUFFIX=".exe"
    ;;
  *)
    echo "Unsupported OS: $OS"
    exit 1
    ;;
esac

RELEASE_TAG="esp-develop-9.2.2-20250817"
VERSION="esp_develop_9.2.2_20250817"
PACKAGES=(
  "qemu-riscv32-softmmu-${VERSION}-${PLATFORM}.tar.xz"
  "qemu-xtensa-softmmu-${VERSION}-${PLATFORM}.tar.xz"
)

echo "Detected platform: $PLATFORM"
echo "Installing to: ${INSTALL_DIR}"

# Download and extract
rm -rf "$INSTALL_DIR"
mkdir -p "$INSTALL_DIR"

for filename in "${PACKAGES[@]}"; do
  url="https://github.com/espressif/qemu/releases/download/${RELEASE_TAG}/${filename}"
  echo "Downloading: $url"
  curl -fsSL "$url" | tar -xJ -C "$INSTALL_DIR" --strip-components=1
done

if [[ "$PLATFORM" == "x86_64-w64-mingw32" ]]; then
  # Espressif's Windows archives omit libiconv-2.dll, their only non-system
  # runtime dependency. Install the matching pinned MinGW runtime DLL next to
  # QEMU; this does not make LLGo or its Windows toolchain depend on MSYS2.
  LIBICONV_PACKAGE="mingw-w64-x86_64-libiconv-1.19-1-any.pkg.tar.zst"
  LIBICONV_SHA256="21e334d0911f25de75d3e18e0697648bcecfa9658256d600cad0827d719c2f35"
  LIBICONV_URL="https://repo.msys2.org/mingw/mingw64/${LIBICONV_PACKAGE}"
  libiconv_archive="${INSTALL_DIR}/${LIBICONV_PACKAGE}"
  echo "Downloading: $LIBICONV_URL"
  curl -fsSL -o "$libiconv_archive" "$LIBICONV_URL"
  echo "${LIBICONV_SHA256}  ${libiconv_archive}" | sha256sum -c -
  # Git Bash's GNU tar delegates zstd decompression to an optional executable;
  # Windows' built-in bsdtar has native zstd support on every supported runner.
  windows_tar="$(cygpath -u "${SYSTEMROOT}\\System32\\tar.exe")"
  "$windows_tar" -xf "$libiconv_archive" -C "${INSTALL_DIR}/bin" \
    --strip-components=2 mingw64/bin/libiconv-2.dll
  rm -f "$libiconv_archive"
fi

# Verify installation
for exe in qemu-system-riscv32 qemu-system-xtensa; do
  if [ ! -x "${INSTALL_DIR}/bin/${exe}${EXE_SUFFIX}" ]; then
    echo "Error: ${exe} not found after extraction"
    exit 1
  fi
done

if [[ "$PLATFORM" == "x86_64-w64-mingw32" && ! -f "${INSTALL_DIR}/bin/libiconv-2.dll" ]]; then
  echo "Error: libiconv-2.dll not found after extraction"
  exit 1
fi

echo "ESP QEMU (riscv32 + xtensa) installed successfully to: ${INSTALL_DIR}"
