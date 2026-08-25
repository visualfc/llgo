//go:build !windows

package bdwgc

const LLGoPackage = "link: $(pkg-config --libs bdw-gc); -lgc"
