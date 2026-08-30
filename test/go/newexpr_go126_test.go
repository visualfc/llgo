//go:build go1.26
// +build go1.26

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

package gotest

import "testing"

var (
	globalNewExpr = new(0)
	globalAlias   = globalNewExpr
)

func TestNewExpressionInitializesAllocatedValue(t *testing.T) {
	{
		p := new(123)
		if *p != 123 {
			t.Fatalf("new(untyped constant) = %d, want 123", *p)
		}
	}
	{
		x := 42
		p := new(x)
		if *p != x {
			t.Fatalf("new(non-constant) = %d, want %d", *p, x)
		}
	}
	{
		x := [2]int{123, 456}
		p := new(x)
		if *p != x {
			t.Fatalf("new(composite value) = %v, want %v", *p, x)
		}
	}
	{
		var i int
		p := new(i > 0)
		if *p {
			t.Fatal("new(untyped bool expression) = true, want false")
		}
	}
	if globalAlias != globalNewExpr || *globalNewExpr != 0 {
		t.Fatalf("global new expression = (%p, %p, %d), want identical pointers and zero value", globalAlias, globalNewExpr, *globalNewExpr)
	}
}
