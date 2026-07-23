//go:build !llgo
// +build !llgo

package build

import (
	"strings"
	"testing"

	"github.com/xgo-dev/llvm"

	"github.com/goplus/llgo/internal/packages"
	llssa "github.com/goplus/llgo/ssa"
)

func init() {
	llssa.Initialize(llssa.InitAll)
}

func TestGenMainModuleExecutable(t *testing.T) {
	llvm.InitializeAllTargets()
	t.Setenv(llgoStdioNobuf, "")
	ctx := &context{
		prog: llssa.NewProgram(nil),
		buildConf: &Config{
			BuildMode: BuildModeExe,
			Goos:      "linux",
			Goarch:    "amd64",
		},
	}
	pkg := &packages.Package{PkgPath: "example.com/foo", ExportFile: "foo.a"}
	mod := genMainModule(ctx, llssa.PkgRuntime, pkg,
		&genConfig{rtInit: true, pyInit: true})
	if mod.ExportFile != "foo.a-main" {
		t.Fatalf("unexpected export file: %s", mod.ExportFile)
	}
	ir := mod.LPkg.String()
	checks := []string{
		"define i32 @main(",
		"call void @Py_Initialize()",
		"call void @Py_Finalize()",
		"call void @\"example.com/foo.init\"()",
		"define weak void @_start()",
	}
	for _, want := range checks {
		if !strings.Contains(ir, want) {
			t.Fatalf("main module IR missing %q:\n%s", want, ir)
		}
	}
	assertInOrder(t, ir,
		"call void @Py_Initialize()",
		"call void @\"example.com/foo.init\"()",
		"call void @\"example.com/foo.main\"()",
		"call void @Py_Finalize()",
	)
}

func TestGenMainModuleLibrary(t *testing.T) {
	llvm.InitializeAllTargets()
	t.Setenv(llgoStdioNobuf, "")
	ctx := &context{
		prog: llssa.NewProgram(nil),
		buildConf: &Config{
			BuildMode: BuildModeCArchive,
			Goos:      "linux",
			Goarch:    "amd64",
		},
	}
	pkg := &packages.Package{PkgPath: "example.com/foo", ExportFile: "foo.a"}
	mod := genMainModule(ctx, llssa.PkgRuntime, pkg, &genConfig{})
	ir := mod.LPkg.String()
	if strings.Contains(ir, "define i32 @main") {
		t.Fatalf("library mode should not emit main function:\n%s", ir)
	}
	if !strings.Contains(ir, "@__llgo_argc = global i32 0") {
		t.Fatalf("library mode missing argc global:\n%s", ir)
	}
	if strings.Contains(ir, "@llvm.global_ctors") {
		t.Fatalf("library mode without the runtime should not emit a constructor:\n%s", ir)
	}
}

func TestGenMainModuleLibraryInitializesRuntime(t *testing.T) {
	llvm.InitializeAllTargets()
	t.Setenv(llgoStdioNobuf, "")
	for _, mode := range []BuildMode{BuildModeCArchive, BuildModeCShared} {
		t.Run(string(mode), func(t *testing.T) {
			ctx := &context{
				prog: llssa.NewProgram(nil),
				buildConf: &Config{
					BuildMode: mode,
					Goos:      "linux",
					Goarch:    "amd64",
				},
			}
			pkg := &packages.Package{PkgPath: "example.com/foo", ExportFile: "foo.a"}
			mod := genMainModule(ctx, llssa.PkgRuntime, pkg, &genConfig{rtInit: true})
			ir := mod.LPkg.String()
			checks := []string{
				"@llvm.global_ctors = appending global",
				"define internal void @__llgo_runtime_ctor()",
				"call void @\"github.com/goplus/llgo/runtime/internal/runtime.init\"()",
			}
			for _, want := range checks {
				if !strings.Contains(ir, want) {
					t.Fatalf("library module IR missing %q:\n%s", want, ir)
				}
			}
			if strings.Contains(ir, "define i32 @main") {
				t.Fatalf("library mode should not emit main function:\n%s", ir)
			}
		})
	}
}

func assertInOrder(t *testing.T, s string, wants ...string) {
	t.Helper()
	offset := 0
	for _, want := range wants {
		i := strings.Index(s[offset:], want)
		if i < 0 {
			t.Fatalf("main module IR missing ordered entry %q after byte %d:\n%s", want, offset, s)
		}
		offset += i + len(want)
	}
}

func TestGenMainModuleLinknames(t *testing.T) {
	llvm.InitializeAllTargets()
	t.Setenv(llgoStdioNobuf, "")
	ctx := &context{
		prog: llssa.NewProgram(nil),
		buildConf: &Config{
			BuildMode: BuildModeCShared,
			Goos:      "linux",
			Goarch:    "amd64",
		},
	}
	mainPkg := ctx.prog.NewPackage("main", "example.com/foo/pkg")
	fn := mainPkg.NewFunc("example.com/foo/pkg.demo", llssa.NoArgsNoRet, llssa.InC)
	fn.MakeBody(1).Return()
	fn = mainPkg.NewFunc("example.com/foo/pkg.main", llssa.NoArgsNoRet, llssa.InC)
	fn.MakeBody(1).Return()

	ctx.prog.SetLinkname("main_demo", "main.demo")
	ctx.prog.SetLinkname("main_main", "main.main")

	pkg := &packages.Package{PkgPath: "example.com/foo/pkg", Name: "main"}
	mod := genMainModule(ctx, llssa.PkgRuntime, pkg, &genConfig{
		mainPkg: mainPkg,
	})
	ir := mod.LPkg.String()
	if !strings.Contains(ir, "main.main") {
		t.Fatalf("go:linkname missing main.main:\n%s", ir)
	}
	if !strings.Contains(ir, "main.demo") {
		t.Fatalf("go:linkname missing main.demo:\n%s", ir)
	}
	if strings.Contains(ir, "@llvm.global_ctors") {
		t.Fatalf("library mode without the runtime should not emit a constructor:\n%s", ir)
	}
}
