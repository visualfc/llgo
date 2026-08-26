//go:build !llgo

/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cl

import (
	"go/token"
	"go/types"
	"strings"
	"testing"

	"golang.org/x/tools/go/ssa"
)

func TestArrayCompareReusesImmutableLocalStorage(t *testing.T) {
	const source = `
package foo

func immutable() bool {
	x := [32]byte{1}
	y := [32]byte{2}
	return x == y
}

func snapshot() bool {
	x := [32]byte{1}
	y := [32]byte{2}
	old := x
	x[0] = 3
	return old == y
}

func small() bool {
	x := [4]byte{1}
	y := [4]byte{2}
	return x == y
}

func parameters(x, y [32]byte) bool {
	return x == y
}

func scalar(p *int) int {
	return *p
}

func crossBlock(change bool) bool {
	x := [32]byte{1}
	y := [32]byte{2}
	if change {
		x[0] = 3
	}
	return x == y
}
`
	ssaPkg, _, _ := buildGoSSAPkg(t, source)
	findCompare := func(name string) *ssa.BinOp {
		t.Helper()
		for _, block := range ssaPkg.Func(name).Blocks {
			for _, instr := range block.Instrs {
				if bin, ok := instr.(*ssa.BinOp); ok && bin.Op == token.EQL {
					if _, ok := bin.X.Type().Underlying().(*types.Array); ok {
						return bin
					}
				}
			}
		}
		t.Fatalf("array comparison not found in %s", name)
		return nil
	}
	immutable := findCompare("immutable")
	if _, ok := immutableLocalArrayLoadAddr(immutable.X); !ok {
		t.Fatal("immutable left array was not recognized")
	}
	if _, ok := immutableLocalArrayLoadAddr(immutable.Y); !ok {
		t.Fatal("immutable right array was not recognized")
	}
	snapshot := findCompare("snapshot")
	if _, ok := immutableLocalArrayLoadAddr(snapshot.X); ok {
		t.Fatal("array changed after its load was treated as immutable")
	}
	if _, ok := immutableLocalArrayLoadAddr(snapshot.Y); !ok {
		t.Fatal("unchanged snapshot operand was not recognized")
	}
	parameters := findCompare("parameters")
	if _, ok := immutableLocalArrayLoadAddr(parameters.X); ok {
		t.Fatal("array parameter was treated as reusable local storage")
	}
	var scalarLoad *ssa.UnOp
	for _, block := range ssaPkg.Func("scalar").Blocks {
		for _, instr := range block.Instrs {
			if load, ok := instr.(*ssa.UnOp); ok && load.Op == token.MUL {
				scalarLoad = load
			}
		}
	}
	if scalarLoad == nil {
		t.Fatal("scalar load not found")
	}
	if _, ok := immutableLocalArrayLoadAddr(scalarLoad); ok {
		t.Fatal("scalar load was treated as reusable array storage")
	}
	immutableLoad := immutable.X.(*ssa.UnOp)
	refs := immutableLoad.X.Referrers()
	func() {
		original := *refs
		defer func() { *refs = original }()
		*refs = append(*refs, &ssa.BinOp{})
		if _, ok := immutableLocalArrayLoadAddr(immutable.X); ok {
			t.Fatal("array with an unknown address use was treated as immutable")
		}
	}()
	crossBlock := findCompare("crossBlock")
	if _, ok := immutableLocalArrayLoadAddr(crossBlock.X); ok {
		t.Fatal("array written from another block was treated as immutable")
	}
	if _, ok := immutableLocalArrayLoadAddr(crossBlock.Y); !ok {
		t.Fatal("unchanged cross-block operand was not recognized")
	}

	_, mod := mustCompileLLPkgFromSrc(t, source)
	immutableIR := mustNamedFunction(t, mod, "foo.immutable").String()
	if got := strings.Count(immutableIR, "alloca [32 x i8]"); got != 2 {
		t.Fatalf("immutable comparison has %d array allocations, want only its two source values:\n%s", got, immutableIR)
	}
	if !strings.Contains(immutableIR, ".memequal") || strings.Contains(immutableIR, "store [32 x i8]") || strings.Contains(immutableIR, "stacksave") {
		t.Fatalf("immutable comparison copied an aggregate value:\n%s", immutableIR)
	}
	snapshotIR := mustNamedFunction(t, mod, "foo.snapshot").String()
	if !strings.Contains(snapshotIR, ".memequal") || !strings.Contains(snapshotIR, "store [32 x i8]") || !strings.Contains(snapshotIR, "stacksave") {
		t.Fatalf("mutable source did not preserve the loaded array snapshot:\n%s", snapshotIR)
	}
	smallIR := mustNamedFunction(t, mod, "foo.small").String()
	if strings.Contains(smallIR, ".memequal") || strings.Contains(smallIR, "stacksave") {
		t.Fatalf("small array comparison used the runtime path:\n%s", smallIR)
	}
}
