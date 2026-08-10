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

package cl_test

import (
	"regexp"
	"strings"
	"testing"

	"github.com/goplus/llgo/cl/cltest"
)

func TestRecoverUsesActivationToken(t *testing.T) {
	const src = `package foo

func recursive(depth int) any {
	if depth > 0 {
		return recursive(depth - 1)
	}
	return recover()
}
`
	ir := cltest.CompileIREx(t, src, "foo.go", false, nil)

	bind := regexp.MustCompile(`BindRecoverFrame"\(ptr @foo\.recursive, ptr (%[0-9]+)\)`).FindStringSubmatch(ir)
	if bind == nil {
		t.Fatalf("recover function does not bind an activation token:\n%s", ir)
	}
	if want := `Recover"(ptr ` + bind[1] + `)`; !strings.Contains(ir, want) {
		t.Fatalf("recover does not use bound activation token %s:\n%s", bind[1], ir)
	}
	if !strings.Contains(ir, `noinline`) || !strings.Contains(ir, `"disable-tail-calls"="true"`) {
		t.Fatalf("recover function must preserve its activation frame:\n%s", ir)
	}
}
