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

package ssa

import (
	"go/token"
	"go/types"
	"strings"
	"testing"
)

func TestBuildLocalPackageAccessor(t *testing.T) {
	for _, test := range []struct {
		name         string
		nativeAnchor bool
		declaration  string
	}{
		{"native TLS anchor", true, "external thread_local global i64"},
		{"ordinary anchor", false, "external global i64"},
	} {
		t.Run(test.name, func(t *testing.T) {
			prog := NewProgram(nil)
			prog.SetRuntime(localContextTestRuntime())
			anchorName := PkgRuntime + "." + runtimeCurrentLocalContext
			if test.nativeAnchor {
				prog.SetLocalityInfo(anchorName, LocalityInfo{Locality: ThreadLocal})
				prog.SetLocalStorage(anchorName, LocalStorageNativeTLS)
			}

			pkg := prog.NewPackage("accessor", "example.com/accessor")
			key := pkg.NewVar("example.com/accessor.key", types.NewPointer(types.Typ[types.Uint8]), InGo)
			key.InitNil()
			field := types.NewField(token.NoPos, nil, "pointer", types.NewPointer(types.Typ[types.Int]), false)
			block := types.NewStruct([]*types.Var{field}, nil)
			blockType := prog.Type(block, InGo)
			result := types.NewPointer(block)
			results := types.NewTuple(types.NewVar(token.NoPos, nil, "", result))
			accessor := pkg.NewFunc("example.com/accessor.block", types.NewSignatureType(nil, nil, nil, nil, results, false), InGo)
			accessor.BuildLocalPackageAccessor(
				key.Expr,
				prog.IntVal(prog.SizeOf(blockType), prog.Uintptr()),
				prog.IntVal(prog.AlignOf(blockType), prog.Uintptr()),
			)

			ir := pkg.String()
			anchor := `@"` + anchorName + `" = ` + test.declaration
			if !strings.Contains(ir, anchor) {
				t.Fatalf("context anchor declaration not found: %s\n%s", anchor, ir)
			}
			if got := strings.Count(ir, "icmp "); got != 3 {
				t.Fatalf("accessor comparisons = %d, want context/head/key checks:\n%s", got, ir)
			}
			if !strings.Contains(ir, `call ptr @"`+PkgRuntime+`.LocalPackage"`) {
				t.Fatalf("accessor has no LocalPackage slow path:\n%s", ir)
			}
		})
	}
}

func localContextTestRuntime() *types.Package {
	pkg := types.NewPackage(PkgRuntime, "runtime")
	unsafePointer := types.Typ[types.UnsafePointer]

	contextName := types.NewTypeName(token.NoPos, pkg, runtimeLocalContext, nil)
	contextFields := []*types.Var{types.NewField(token.NoPos, pkg, "blocks", unsafePointer, false)}
	types.NewNamed(contextName, types.NewStruct(contextFields, nil), nil)
	pkg.Scope().Insert(contextName)

	blockName := types.NewTypeName(token.NoPos, pkg, runtimeLocalBlock, nil)
	blockFields := []*types.Var{
		types.NewField(token.NoPos, pkg, "next", unsafePointer, false),
		types.NewField(token.NoPos, pkg, "key", unsafePointer, false),
	}
	types.NewNamed(blockName, types.NewStruct(blockFields, nil), nil)
	pkg.Scope().Insert(blockName)
	pkg.Scope().Insert(types.NewVar(token.NoPos, pkg, runtimeCurrentLocalContext, types.Typ[types.Uintptr]))

	params := types.NewTuple(
		types.NewVar(token.NoPos, pkg, "key", unsafePointer),
		types.NewVar(token.NoPos, pkg, "size", types.Typ[types.Uintptr]),
		types.NewVar(token.NoPos, pkg, "align", types.Typ[types.Uintptr]),
	)
	results := types.NewTuple(types.NewVar(token.NoPos, pkg, "", unsafePointer))
	pkg.Scope().Insert(types.NewFunc(token.NoPos, pkg, "LocalPackage", types.NewSignatureType(nil, nil, nil, params, results, false)))
	return pkg
}
