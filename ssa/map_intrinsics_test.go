package ssa

import (
	"go/types"
	"strings"
	"testing"
)

func TestAMD64MapIntrinsics(t *testing.T) {
	prog := NewProgram(&Target{GOOS: "linux", GOARCH: "amd64", BuildTags: "llgo,swissmap"})
	defer prog.Dispose()
	pkg := prog.NewPackage("maps", "maps")

	uint64Type := types.Typ[types.Uint64]
	uintptrType := types.Typ[types.Uintptr]
	params := types.NewTuple(
		types.NewParam(0, nil, "group", uint64Type),
		types.NewParam(0, nil, "hash", uintptrType),
	)
	results := types.NewTuple(types.NewParam(0, nil, "", uint64Type))
	matchSig := types.NewSignatureType(nil, nil, nil, params, results, false)
	match := pkg.NewFunc(mapsRuntimePackage+"ctrlGroupMatchH2", matchSig, InGo)
	matchCaller := pkg.NewFunc("match", matchSig, InGo)
	matchBuilder := matchCaller.MakeBody(1)
	matchBuilder.Return(matchBuilder.Call(match.Expr, matchCaller.Param(0), matchCaller.Param(1)))

	firstParams := types.NewTuple(types.NewParam(0, nil, "bits", uint64Type))
	firstResults := types.NewTuple(types.NewParam(0, nil, "", uintptrType))
	firstSig := types.NewSignatureType(nil, nil, nil, firstParams, firstResults, false)
	first := pkg.NewFunc(mapsRuntimePackage+"bitsetFirst", firstSig, InGo)
	firstCaller := pkg.NewFunc("first", firstSig, InGo)
	firstBuilder := firstCaller.MakeBody(1)
	firstBuilder.Return(firstBuilder.Call(first.Expr, firstCaller.Param(0)))
	ordinary := pkg.NewFunc(mapsRuntimePackage+"ordinary", NoArgsNoRet, InGo)
	ordinaryCaller := pkg.NewFunc("ordinary", NoArgsNoRet, InGo)
	ordinaryBuilder := ordinaryCaller.MakeBody(1)
	ordinaryBuilder.Call(ordinary.Expr)
	ordinaryBuilder.Return()

	ir := pkg.String()
	for _, want := range []string{
		"icmp eq <8 x i8>",
		"bitcast <8 x i1>",
		"@llvm.cttz.i64",
	} {
		if !strings.Contains(ir, want) {
			t.Fatalf("missing %q in amd64 map intrinsic IR:\n%s", want, ir)
		}
	}
	for _, unwanted := range []string{
		"call i64 @\"" + mapsRuntimePackage + "ctrlGroupMatchH2\"",
		"call i64 @\"" + mapsRuntimePackage + "bitsetFirst\"",
	} {
		if strings.Contains(ir, unwanted) {
			t.Fatalf("map intrinsic was left as a call %q:\n%s", unwanted, ir)
		}
	}
	if !strings.Contains(ir, "call void @\""+mapsRuntimePackage+"ordinary\"()") {
		t.Fatalf("ordinary maps runtime call was incorrectly replaced:\n%s", ir)
	}
}
