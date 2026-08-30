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

import (
	"os"
	"runtime"
	"runtime/debug"
	"strconv"
	"strings"
	"testing"
)

type runtimeStatementWrapper struct {
	values []int
}

func (w runtimeStatementWrapper) get(index int) int {
	return w.values[index]
}

func TestRuntimeStatementLineInfo(t *testing.T) {
	checkRuntimeCallerStatement(t)
	checkRuntimeCallersFramesStatement(t, runtimeStatementMarkerLine(t, "// CALLERS_STMT_MARK"))
	checkRuntimeInterfaceIndirectCaller(t)
	checkRuntimeClosureIndirectCaller(t)
	checkAdjacentRuntimeStack(t)
	checkRecoveredDebugStackBounds(t, runtimeStatementMarkerLine(t, "// BOUNDS_MARK"))
	checkRecoveredStaticPanicLine(t)
	if runtime.GOOS == "windows" {
		checkRecoveredStorePanicLine(t)
	}
	checkRecoveredIndirectPanicLine(t)
}

//go:noinline
func checkRuntimeCallerStatement(t *testing.T) {
	_, _, previous, _ := runtime.Caller(0)
	_, file, line, ok := runtime.Caller(0)
	if !ok || !strings.HasSuffix(file, "runtime_statement_line_test.go") || line != previous+1 {
		t.Fatalf("caller statement = %s:%d, want next line after %d", file, line, previous)
	}
}

//go:noinline
func checkRuntimeCallersFramesStatement(t *testing.T, want int) {
	var pcs [16]uintptr
	n := runtime.Callers(0, pcs[:]) // CALLERS_STMT_MARK
	frames := runtime.CallersFrames(pcs[:n])
	for {
		frame, more := frames.Next()
		if strings.HasSuffix(frame.Function, ".checkRuntimeCallersFramesStatement") {
			if !strings.HasSuffix(frame.File, "runtime_statement_line_test.go") || frame.Line != want {
				t.Fatalf("callers frame = %s:%d, want line %d", frame.File, frame.Line, want)
			}
			fn := runtime.FuncForPC(frame.PC - 1)
			if fn == nil || !strings.HasSuffix(fn.Name(), ".checkRuntimeCallersFramesStatement") {
				t.Fatalf("FuncForPC(pc-1) = %s", runtimeFunctionName(fn))
			}
			file, line := fn.FileLine(frame.PC - 1)
			if !strings.HasSuffix(file, "runtime_statement_line_test.go") || line == 0 {
				t.Fatalf("Func.FileLine(pc-1) = %s:%d", file, line)
			}
			return
		}
		if !more {
			break
		}
	}
	t.Fatal("CallersFrames is missing the current function")
}

func runtimeStatementMarkerLine(t *testing.T, marker string) int {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("current source file is unavailable")
	}
	source, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	for index, line := range strings.Split(string(source), "\n") {
		if strings.HasSuffix(strings.TrimSpace(line), marker) {
			return index + 1
		}
	}
	t.Fatalf("source marker %q is missing", marker)
	return 0
}

type runtimeStatementIndirectCaller interface {
	call(*testing.T, int)
}

type runtimeStatementIndirectCallerImpl struct{}

//go:noinline
func checkRuntimeInterfaceIndirectCaller(t *testing.T) {
	var caller runtimeStatementIndirectCaller = runtimeStatementIndirectCallerImpl{}
	_, _, previous, _ := runtime.Caller(0)
	caller.call(t, previous+1)
}

//go:noinline
func (runtimeStatementIndirectCallerImpl) call(t *testing.T, want int) {
	runtimeStatementInterfaceMiddle(t, want)
}

//go:noinline
func runtimeStatementInterfaceMiddle(t *testing.T, want int) {
	checkRuntimeIndirectCallerLine(t, "interface", 3, want)
}

//go:noinline
func checkRuntimeClosureIndirectCaller(t *testing.T) {
	want := 0
	fn := runtimeStatementClosureLayer(runtimeStatementClosureLayer(func() {
		checkRuntimeIndirectCallerLine(t, "closure", 4, want)
	}))
	_, _, previous, _ := runtime.Caller(0)
	want = previous + 2
	fn()
}

//go:noinline
func runtimeStatementClosureLayer(next func()) func() {
	return func() {
		next()
	}
}

//go:noinline
func checkRuntimeIndirectCallerLine(t *testing.T, kind string, skip, want int) {
	_, file, line, ok := runtime.Caller(skip)
	if !ok || !strings.HasSuffix(file, "runtime_statement_line_test.go") || line != want {
		t.Fatalf("%s indirect caller = %s:%d, want line %d", kind, file, line, want)
	}
}

//go:noinline
func checkAdjacentRuntimeStack(t *testing.T) {
	var first, second [4096]byte
	_, _, previous, _ := runtime.Caller(0)
	n1 := runtime.Stack(first[:], false)
	n2 := runtime.Stack(second[:], false)
	line1 := runtimeStackLineFor(string(first[:n1]), "checkAdjacentRuntimeStack")
	line2 := runtimeStackLineFor(string(second[:n2]), "checkAdjacentRuntimeStack")
	if line1 != previous+1 || line2 != previous+2 {
		t.Fatalf("adjacent stack lines = %d,%d, want %d,%d", line1, line2, previous+1, previous+2)
	}
}

//go:noinline
func checkRecoveredDebugStackBounds(t *testing.T, want int) {
	var stack string
	func() {
		defer func() {
			if recover() == nil {
				t.Fatal("missing bounds panic")
			}
			stack = string(debug.Stack())
		}()
		wrapper := runtimeStatementWrapper{values: []int{0, 1, 2}}
		_ = wrapper.get(3) // BOUNDS_MARK
	}()
	if got := runtimeStackLineFor(stack, "checkRecoveredDebugStackBounds.func1"); got != want {
		t.Fatalf("recovered bounds line = %d, want %d\n%s", got, want, stack)
	}
}

func checkRecoveredStaticPanicLine(t *testing.T) {
	checkRecoveredPanicLine(t, "runtimeStatementStaticNilPanic", runtimeStatementMarkerLine(t, "// STATIC_NIL_PANIC_MARK"), runtimeStatementStaticNilPanic)
}

func runtimeStatementStaticNilPanic() {
	var pointer *int
	_ = *pointer // STATIC_NIL_PANIC_MARK
}

func checkRecoveredStorePanicLine(t *testing.T) {
	checkRecoveredPanicLine(t, "runtimeStatementStoreNilPanic", runtimeStatementMarkerLine(t, "// STORE_NIL_PANIC_MARK"), runtimeStatementStoreNilPanic)
}

func runtimeStatementStoreNilPanic() {
	var pointer *int
	*pointer = 1 // STORE_NIL_PANIC_MARK
}

func checkRecoveredIndirectPanicLine(t *testing.T) {
	checkRecoveredPanicLine(t, "runtimeStatementIndirectBoundsPanic", runtimeStatementMarkerLine(t, "// INDIRECT_BOUNDS_PANIC_MARK"), runtimeStatementIndirectBoundsPanic)
}

func runtimeStatementIndirectBoundsPanic() {
	values := []int{0}
	_ = values[1] // INDIRECT_BOUNDS_PANIC_MARK
}

func checkRecoveredPanicLine(t *testing.T, function string, want int, panicFunc func()) {
	t.Helper()
	var stack string
	func() {
		defer func() {
			if recover() == nil {
				t.Fatalf("missing panic for %s", function)
			}
			stack = string(debug.Stack())
		}()
		panicFunc()
	}()
	if got := runtimeStackLineFor(stack, function); got != want {
		t.Fatalf("recovered panic line for %s = %d, want %d\n%s", function, got, want, stack)
	}
}

func TestRuntimeDeferredPanicLine(t *testing.T) {
	want := runtimeStatementMarkerLine(t, "// DEFERRED_PANIC_MARK")
	var stack string
	func() {
		defer func() {
			recover()
			stack = string(debug.Stack())
		}()
		defer runtimeStatementDeferredPanic()
		panic("start unwinding")
	}()
	if got := runtimeStackLineFor(stack, "runtimeStatementDeferredPanic"); got != want {
		t.Fatalf("deferred panic line = %d, want %d\n%s", got, want, stack)
	}
}

func runtimeStatementDeferredPanic() {
	var pointer *int
	_ = *pointer // DEFERRED_PANIC_MARK
}

func runtimeStackLineFor(stack, function string) int {
	lines := strings.Split(stack, "\n")
	for i := 0; i+1 < len(lines); i++ {
		if !strings.Contains(strings.TrimSpace(lines[i]), function+"(") {
			continue
		}
		location := strings.TrimSpace(lines[i+1])
		colon := strings.LastIndexByte(location, ':')
		if colon < 0 {
			return 0
		}
		rest := location[colon+1:]
		if end := strings.IndexByte(rest, ' '); end >= 0 {
			rest = rest[:end]
		}
		line, _ := strconv.Atoi(rest)
		return line
	}
	return 0
}
