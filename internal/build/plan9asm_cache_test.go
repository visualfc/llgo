//go:build !llgo

package build

import (
	"go/token"
	"go/types"
	"path/filepath"
	"runtime"
	"sync"
	"testing"
	"weak"

	"github.com/xgo-dev/llgo/internal/packages"
)

func TestPlan9AsmSignatureCacheDoesNotRetainBuildContext(t *testing.T) {
	ref := func() weak.Pointer[context] {
		ctx := &context{plan9asmReady: true, plan9asmMode: plan9asmEnvNone}
		if _, err := plan9asmSigsForPkg(ctx, "example.com/disabled"); err != nil {
			t.Fatal(err)
		}
		return weak.Make(ctx)
	}()
	runtime.GC()
	if ref.Value() != nil {
		t.Fatal("Plan 9 signature cache retains a completed build context")
	}
}

func TestPlan9AsmSignatureCacheEmptyResults(t *testing.T) {
	if got, err := plan9asmSigsForPkg(nil, "p"); err != nil || got != nil {
		t.Fatalf("nil context: %v, %v", got, err)
	}
	ctx := &context{plan9asmReady: true, plan9asmMode: plan9asmEnvAll, buildConf: &Config{Goos: "linux", Goarch: "amd64"}}
	for _, path := range []string{"", "runtime", "example.com/missing"} {
		for range 2 {
			if got, err := plan9asmSigsForPkg(ctx, path); err != nil || len(got) != 0 {
				t.Fatalf("%s: %v, %v", path, got, err)
			}
		}
	}
}

func TestPlan9AsmSignatureCacheRetriesErrorsAndReusesSuccess(t *testing.T) {
	const path = "example.com/asm"
	dir := t.TempDir()
	asm := filepath.Join(dir, "foo_amd64.s")
	typesPkg := types.NewPackage(path, "asm")
	typesPkg.Scope().Insert(types.NewFunc(token.NoPos, typesPkg, "Foo", types.NewSignatureType(nil, nil, nil, nil, nil, false)))
	pkg := &packages.Package{ID: path, PkgPath: path, Types: typesPkg, Fset: token.NewFileSet(), Dir: dir, OtherFiles: []string{asm}}
	ctx := &context{
		plan9asmReady: true, plan9asmMode: plan9asmEnvAll,
		buildConf: &Config{Goos: "linux", Goarch: "amd64", Overlay: make(map[string][]byte)},
		pkgs:      map[*packages.Package]Package{pkg: {Package: pkg}},
	}
	if _, err := plan9asmSigsForPkg(ctx, path); err == nil {
		t.Fatal("missing assembly source unexpectedly succeeded")
	}
	ctx.buildConf.Overlay[asm] = []byte("TEXT ·Foo(SB),NOSPLIT,$0-0\nRET\n")
	got, err := plan9asmSigsForPkg(ctx, path)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := got[path+".Foo"]; !ok {
		t.Fatalf("missing Foo signature: %v", got)
	}
	// A successful lookup is cached; a failed one must not have been.
	delete(ctx.buildConf.Overlay, asm)
	if cached, err := plan9asmSigsForPkg(ctx, path); err != nil || len(cached) != 1 {
		t.Fatalf("cached translated signatures: %v, %v", cached, err)
	}
}

func TestPlan9AsmSignatureCacheIsPerContext(t *testing.T) {
	first := &context{plan9asmReady: true, plan9asmMode: plan9asmEnvNone}
	second := &context{plan9asmReady: true, plan9asmMode: plan9asmEnvNone}
	load := func(ctx *context) map[string]struct{} {
		t.Helper()
		sigs, err := plan9asmSigsForPkg(ctx, "example.com/p")
		if err != nil {
			t.Fatal(err)
		}
		return sigs
	}
	sigs := load(first)
	sigs["cached"] = struct{}{}
	if _, ok := load(first)["cached"]; !ok {
		t.Fatal("same context did not reuse its cached signatures")
	}
	if _, ok := load(second)["cached"]; ok {
		t.Fatal("signature cache leaked between build contexts")
	}
}

func TestPlan9AsmSignatureCacheConcurrentLookup(t *testing.T) {
	ctx := &context{plan9asmReady: true, plan9asmMode: plan9asmEnvNone}
	var wg sync.WaitGroup
	for range 16 {
		wg.Go(func() {
			for range 8 {
				sigs, err := plan9asmSigsForPkg(ctx, "example.com/p")
				if err != nil || len(sigs) != 0 {
					t.Errorf("concurrent signature lookup: %v, %v", sigs, err)
				}
			}
		})
	}
	wg.Wait()
}
