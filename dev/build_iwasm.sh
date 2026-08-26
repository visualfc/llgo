#!/bin/bash
# Script to build iwasm with correct options for llgo WASM testing
# This ensures local testing uses the same iwasm configuration as CI

set -e

# Determine cache directory based on platform
if [ "$(uname -s)" = "Darwin" ]; then
    LLGO_CACHE_DIR="${HOME}/Library/Caches/llgo"
else
    LLGO_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/llgo"
fi

IWASM_BIN_DIR="${LLGO_CACHE_DIR}/bin"
WAMR_VERSION="WAMR-2.4.4"

echo "Building iwasm for llgo WASM testing..."
echo "Target directory: ${IWASM_BIN_DIR}"

# Create bin directory if it doesn't exist
mkdir -p "${IWASM_BIN_DIR}"

# Create temp directory for building
TEMP_DIR=$(mktemp -d)
cd "${TEMP_DIR}"

echo "Cloning wasm-micro-runtime ${WAMR_VERSION}..."
git clone --branch ${WAMR_VERSION} --depth 1 https://github.com/bytecodealliance/wasm-micro-runtime.git

# WAMR's Windows platform sources expect MSVC preprocessing. This compiler
# choice is only for the host-side iwasm test helper.
IWASM_NAME="iwasm"
CMAKE_GENERATOR_ARGS=()
case "$(uname -s)" in
    Darwin)
        PLATFORM="darwin"
        ;;
    Linux)
        PLATFORM="linux"
        ;;
    MINGW*|MSYS*|CYGWIN*)
        PLATFORM="windows"
        IWASM_NAME="iwasm.exe"
        CMAKE_GENERATOR_ARGS=(
            -G "NMake Makefiles"
            -D CMAKE_C_COMPILER=cl
            -D CMAKE_CXX_COMPILER=cl
        )
        ;;
    *)
        echo "Unsupported platform: $(uname -s)"
        exit 1
        ;;
esac

echo "Building for platform: ${PLATFORM}"

mkdir -p wasm-micro-runtime/product-mini/platforms/${PLATFORM}/build
cd wasm-micro-runtime/product-mini/platforms/${PLATFORM}/build

# The test helper executes Wasm bytecode only, so AOT is unnecessary; LLGo's
# generated modules require reference-types support.
cmake "${CMAKE_GENERATOR_ARGS[@]}" \
    -D WAMR_BUILD_EXCE_HANDLING=1 \
    -D WAMR_BUILD_AOT=0 \
    -D WAMR_BUILD_FAST_INTERP=0 \
    -D WAMR_BUILD_REF_TYPES=1 \
    -D WAMR_BUILD_SHARED_MEMORY=1 \
    -D WAMR_BUILD_LIB_WASI_THREADS=1 \
    -D WAMR_BUILD_LIB_PTHREAD=1 \
    -D CMAKE_BUILD_TYPE=Debug \
    -D WAMR_BUILD_DEBUG_INTERP=1 \
    ..

echo "Compiling iwasm..."
cmake --build . --parallel "$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"

# Copy iwasm to cache directory
echo "Installing iwasm to ${IWASM_BIN_DIR}..."
cp "${IWASM_NAME}" "${IWASM_BIN_DIR}/"

# Cleanup
cd /
rm -rf "${TEMP_DIR}"

echo ""
echo "✓ iwasm successfully built and installed to ${IWASM_BIN_DIR}/${IWASM_NAME}"
echo ""
echo "To use this iwasm, add to your PATH:"
echo "  export PATH=\"${IWASM_BIN_DIR}:\$PATH\""
echo ""
echo "Or run directly:"
echo "  ${IWASM_BIN_DIR}/${IWASM_NAME} --version"
