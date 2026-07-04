//go:build llgo

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

package llgoext

import (
	"reflect"
	"testing"
	_ "unsafe"
)

//go:linkname runtimeStorePanicPCsForTest github.com/goplus/llgo/runtime/internal/runtime.StorePanicPCs
func runtimeStorePanicPCsForTest([]uintptr)

//go:linkname runtimeStoreFaultPCsForTest github.com/goplus/llgo/runtime/internal/runtime.StoreFaultPCs
func runtimeStoreFaultPCsForTest([]uintptr)

//go:linkname runtimePanicPCsForTest github.com/goplus/llgo/runtime/internal/runtime.PanicPCs
func runtimePanicPCsForTest() []uintptr

//go:linkname runtimePanicPCsAreFaultForTest github.com/goplus/llgo/runtime/internal/runtime.PanicPCsAreFault
func runtimePanicPCsAreFaultForTest() bool

//go:linkname runtimeMarkPanicRecoverFPsForTest github.com/goplus/llgo/runtime/internal/runtime.MarkPanicRecoverFPs
func runtimeMarkPanicRecoverFPsForTest(uintptr, uintptr)

//go:linkname runtimePanicRecoverFPsForTest github.com/goplus/llgo/runtime/internal/runtime.PanicRecoverFPs
func runtimePanicRecoverFPsForTest() (uintptr, uintptr)

type panicPCState struct {
	pcs      []uintptr
	fault    bool
	recover1 uintptr
	recover2 uintptr
}

func TestRuntimePanicPCStateIsolation(t *testing.T) {
	runtimeStorePanicPCsForTest([]uintptr{11, 12})
	runtimeMarkPanicRecoverFPsForTest(13, 14)

	start := make(chan struct{})
	results := make(chan panicPCState, 2)
	for i := uintptr(0); i < 2; i++ {
		go func(base uintptr) {
			<-start
			runtimeStoreFaultPCsForTest([]uintptr{base, base + 1})
			runtimeMarkPanicRecoverFPsForTest(base+2, base+3)
			recover1, recover2 := runtimePanicRecoverFPsForTest()
			results <- panicPCState{
				pcs:      append([]uintptr(nil), runtimePanicPCsForTest()...),
				fault:    runtimePanicPCsAreFaultForTest(),
				recover1: recover1,
				recover2: recover2,
			}
		}(21 + i*10)
	}
	close(start)

	seen := make(map[uintptr]panicPCState)
	for i := 0; i < 2; i++ {
		state := <-results
		seen[state.pcs[0]] = state
	}
	for _, base := range []uintptr{21, 31} {
		state, ok := seen[base]
		if !ok {
			t.Fatalf("missing goroutine state for base %d: %#v", base, seen)
		}
		if want := []uintptr{base, base + 1}; !reflect.DeepEqual(state.pcs, want) {
			t.Fatalf("pcs for base %d = %v, want %v", base, state.pcs, want)
		}
		if !state.fault || state.recover1 != base+2 || state.recover2 != base+3 {
			t.Fatalf("state for base %d = %#v", base, state)
		}
	}

	if got := runtimePanicPCsForTest(); !reflect.DeepEqual(got, []uintptr{11, 12}) {
		t.Fatalf("main goroutine pcs = %v, want [11 12]", got)
	}
	if runtimePanicPCsAreFaultForTest() {
		t.Fatal("main goroutine snapshot unexpectedly marked as fault")
	}
	if recover1, recover2 := runtimePanicRecoverFPsForTest(); recover1 != 13 || recover2 != 14 {
		t.Fatalf("main goroutine recover marks = (%d, %d), want (13, 14)", recover1, recover2)
	}
}
