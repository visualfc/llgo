package c

import (
	"go/build"
	"testing"
)

func TestWasmCTypeFileSelection(t *testing.T) {
	for _, test := range []struct {
		name string
		goos string
		tags []string
		want string
	}{
		{name: "raw js wasm32", goos: "js", want: "ctypes_wasm.go"},
		{name: "explicit Emscripten wasm32", goos: "js", tags: []string{"llgo.wasm.emscripten"}, want: "ctypes_wasm.go"},
		{name: "explicit Emscripten Memory64", goos: "js", tags: []string{"llgo.wasm.emscripten", "llgo.wasm.emscripten.memory64"}, want: "ctypes_wasm_memory64.go"},
		{name: "WASI wasm32", goos: "wasip1", want: "ctypes_wasm.go"},
	} {
		t.Run(test.name, func(t *testing.T) {
			ctx := build.Default
			ctx.GOOS = test.goos
			ctx.GOARCH = "wasm"
			ctx.BuildTags = test.tags
			pkg, err := ctx.ImportDir(".", 0)
			if err != nil {
				t.Fatal(err)
			}
			found := false
			for _, name := range pkg.GoFiles {
				if name == test.want {
					found = true
					break
				}
			}
			if !found {
				t.Fatalf("GoFiles = %v, want %s", pkg.GoFiles, test.want)
			}
		})
	}
}
