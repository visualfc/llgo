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
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"testing"
)

const callerPanicChild = "LLGO_TEST_CALLER_PANIC"

var (
	callerInitFile string
	callerInitLine int
)

func init() {
	_, callerInitFile, callerInitLine, _ = runtime.Caller(0)
	if os.Getenv(callerPanicChild) == "1" {
		callerPanicCaller() // PANIC_INIT_MARK
	}
}

//go:noinline
func callerPanicBoom() {
	panic("acceptance-boom") // PANIC_MARK
}

//go:noinline
func callerPanicCaller() {
	callerPanicBoom() // PANIC_CALLER_MARK
}

func TestCallerPanicTraceback(t *testing.T) {
	cmd := exec.Command(os.Args[0], "-test.run=^$")
	cmd.Env = append(os.Environ(), callerPanicChild+"=1")
	output, err := cmd.CombinedOutput()
	if err == nil {
		t.Fatalf("panic child unexpectedly succeeded:\n%s", output)
	}

	_, sourceFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("current source file is unavailable")
	}
	source, err := os.ReadFile(sourceFile)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"panic: acceptance-boom",
		"goroutine 1 [running]:",
		"callerPanicBoom",
		"caller_runtime_test.go:" + strconv.Itoa(markerLine(string(source), "PANIC_MARK")),
		"callerPanicCaller",
		"caller_runtime_test.go:" + strconv.Itoa(markerLine(string(source), "PANIC_CALLER_MARK")),
	} {
		if !strings.Contains(string(output), want) {
			t.Fatalf("panic traceback is missing %q:\n%s", want, output)
		}
	}
}

type callerReceiver struct{}

var callerValueSink, callerPointerSink, callerClosureSink, callerGenericSink int

//go:noinline
func (callerReceiver) value() uintptr {
	callerValueSink++
	pc, _, _, _ := runtime.Caller(0)
	return pc
}

//go:noinline
func (*callerReceiver) pointer() uintptr {
	callerPointerSink++
	pc, _, _, _ := runtime.Caller(0)
	return pc
}

//go:noinline
func callerGeneric[T any](v T) uintptr {
	callerGenericSink++
	pc, _, _, _ := runtime.Caller(0)
	return pc
}

type callerStackError struct {
	msg string
	pcs [8]uintptr
	n   int
}

func (e *callerStackError) Error() string { return e.msg }

//go:noinline
func newCallerStackError(msg string) *callerStackError {
	err := &callerStackError{msg: msg}
	err.n = runtime.Callers(1, err.pcs[:])
	return err
}

func TestCallerIntrospection(t *testing.T) {
	if !strings.HasSuffix(callerInitFile, "caller_runtime_test.go") || callerInitLine == 0 {
		t.Fatalf("init caller = %s:%d", callerInitFile, callerInitLine)
	}

	var wg sync.WaitGroup
	wg.Add(1)
	var goroutineFile string
	var goroutineLine int
	go func() {
		defer wg.Done()
		_, goroutineFile, goroutineLine, _ = runtime.Caller(0)
	}()
	wg.Wait()
	if !strings.HasSuffix(goroutineFile, "caller_runtime_test.go") || goroutineLine == 0 {
		t.Fatalf("goroutine caller = %s:%d", goroutineFile, goroutineLine)
	}

	var deferredFile string
	var deferredLine int
	func() {
		defer func() {
			_, deferredFile, deferredLine, _ = runtime.Caller(0)
		}()
	}()
	if !strings.HasSuffix(deferredFile, "caller_runtime_test.go") || deferredLine == 0 {
		t.Fatalf("deferred caller = %s:%d", deferredFile, deferredLine)
	}

	var receiver callerReceiver
	checkCallerFunctionSuffix(t, receiver.value(), ".callerReceiver.value")
	checkCallerFunctionSuffix(t, (&receiver).pointer(), ".(*callerReceiver).pointer")
	closure := func() uintptr {
		callerClosureSink++
		pc, _, _, _ := runtime.Caller(0)
		return pc
	}
	closureName := runtime.FuncForPC(closure()).Name()
	if !strings.Contains(closureName, "TestCallerIntrospection.func") && !strings.Contains(closureName, "TestCallerIntrospection$") {
		t.Fatalf("closure name = %q", closureName)
	}
	genericName := runtime.FuncForPC(callerGeneric(0)).Name()
	if !strings.Contains(genericName, ".callerGeneric") {
		t.Fatalf("generic function name = %q", genericName)
	}

	_, _, callLine, _ := runtime.Caller(0)
	err := newCallerStackError("wrapped")
	frames := runtime.CallersFrames(err.pcs[:err.n])
	for {
		frame, more := frames.Next()
		if strings.HasSuffix(frame.Function, ".TestCallerIntrospection") {
			if frame.Line != callLine+1 {
				t.Fatalf("stack capture line = %d, want %d", frame.Line, callLine+1)
			}
			return
		}
		if !more {
			break
		}
	}
	t.Fatal("TestCallerIntrospection frame is missing")
}

func checkCallerFunctionSuffix(t *testing.T, pc uintptr, suffix string) {
	t.Helper()
	fn := runtime.FuncForPC(pc)
	if fn == nil || !strings.HasSuffix(fn.Name(), suffix) {
		name := "<nil>"
		if fn != nil {
			name = fn.Name()
		}
		t.Fatalf("function for pc = %q, want suffix %q", name, suffix)
	}
}
