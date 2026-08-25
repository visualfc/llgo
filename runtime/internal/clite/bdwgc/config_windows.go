//go:build windows

package bdwgc

// MSVC-targeted clang resolves -lgc as gc.lib, while MSYS2 packages the
// ABI-compatible COFF import archive under the GNU spelling.
const LLGoPackage = "link: -Wl,$(pkg-config --variable=libdir bdw-gc)/libgc.dll.a"
