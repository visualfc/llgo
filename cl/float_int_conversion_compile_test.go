//go:build !llgo

package cl

import (
	"go/types"
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
				if arch == "386" {
					// The host runtime import omits this 386-only declaration.
					rt := types.NewPackage(llssa.PkgRuntime, "runtime")
					params := types.NewTuple(types.NewParam(0, rt, "x", types.Typ[types.Float64]))
					results := types.NewTuple(types.NewParam(0, rt, "", types.Typ[types.Uint64]))
					rt.Scope().Insert(types.NewFunc(0, rt, "Float64ToUint64", types.NewSignatureType(nil, nil, nil, params, results, false)))
					prog.SetRuntime(rt)
				}
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
					} else if arch == "386" {
						want = "Float64ToUint64"
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
