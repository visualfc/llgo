//go:build !llgo

package cl

import (
	"strings"
	"testing"

	llssa "github.com/xgo-dev/llgo/ssa"
)

func TestFloatToUint32ConversionMode(t *testing.T) {
	const src = `package floatconvert
func F32(x float32) uint32 { return uint32(x) }
func F64(x float64) uint32 { return uint32(x) }
`
	for _, arch := range []string{"amd64", "arm64", "386"} {
		for _, saturating := range []bool{false, true} {
			mode := "legacy"
			if saturating {
				mode = "saturating"
			}
			t.Run(arch+"/"+mode, func(t *testing.T) {
				ssaPkg, _, files := buildGoSSAPkg(t, src)
				prog := newLLSSAProgForTarget(t, &llssa.Target{
					GOOS: "linux", GOARCH: arch,
					SaturatingFloatToUint32: saturating,
				})
				defer prog.Dispose()
				pkg, err := NewPackage(prog, ssaPkg, files)
				if err != nil {
					t.Fatal(err)
				}
				for _, name := range []string{"F32", "F64"} {
					ir := mustNamedFunction(t, pkg.Module(), "floatconvert."+name).String()
					want := "fptosi "
					if saturating {
						want = "@llvm.fptoui.sat.i32." + strings.ToLower(name) + "("
					} else if arch == "arm64" {
						want = "@llvm.fptosi.sat.i64." + strings.ToLower(name) + "("
					}
					if !strings.Contains(ir, want) {
						t.Fatalf("%s conversion IR missing %q in %s mode:\n%s", name, want, mode, ir)
					}
					if !saturating && !strings.Contains(ir, "trunc i64") {
						t.Fatalf("%s legacy uint32 conversion must truncate i64:\n%s", name, ir)
					}
				}
			})
		}
	}
}
