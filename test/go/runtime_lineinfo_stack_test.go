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
	"reflect"
	"runtime"
	"runtime/debug"
	"strconv"
	"strings"
	"sync"
	"testing"
)

func TestRuntimeLineInfoAndStack(t *testing.T) {
	checkRuntimeCallerLine(t)
	checkRuntimeCallerSkip(t)
	_, _, callLine, _ := runtime.Caller(0)
	checkRuntimeCallersFrames(t, callLine+1)
	checkRuntimeFuncForPC(t)
	checkRuntimeFuncForPCFunctionValue(t)
	checkRuntimeStack(t)
	checkRuntimePanicStack(t)
}

//go:noinline
func checkRuntimeCallerLine(t *testing.T) {
	_, _, previous, _ := runtime.Caller(0)
	_, file, line, ok := runtime.Caller(0)
	if !ok || !strings.HasSuffix(file, "runtime_lineinfo_stack_test.go") || line != previous+1 {
		t.Fatalf("runtime.Caller(0) = %s:%d, want next line after %d", file, line, previous)
	}
}

//go:noinline
func checkRuntimeCallerSkip(t *testing.T) {
	_, _, previous, _ := runtime.Caller(0)
	checkRuntimeCallerSkipHelper(t, previous+1)
}

//go:noinline
func checkRuntimeCallerSkipHelper(t *testing.T, want int) {
	_, file, line, ok := runtime.Caller(1)
	if !ok || !strings.HasSuffix(file, "runtime_lineinfo_stack_test.go") || line != want {
		t.Fatalf("runtime.Caller(1) = %s:%d, want line %d", file, line, want)
	}
}

//go:noinline
func checkRuntimeCallersFrames(t *testing.T, wantCallerLine int) {
	var pcs [16]uintptr
	_, _, previous, _ := runtime.Caller(0)
	n := runtime.Callers(0, pcs[:])
	wantSelfLine := previous + 1
	frames := runtime.CallersFrames(pcs[:n])
	seenSelf := false
	seenCaller := false
	for {
		frame, more := frames.Next()
		switch {
		case strings.HasSuffix(frame.Function, ".checkRuntimeCallersFrames"):
			if !strings.HasSuffix(frame.File, "runtime_lineinfo_stack_test.go") || frame.Line != wantSelfLine {
				t.Fatalf("self frame = %s:%d, want line %d", frame.File, frame.Line, wantSelfLine)
			}
			seenSelf = true
		case strings.HasSuffix(frame.Function, ".TestRuntimeLineInfoAndStack"):
			if !strings.HasSuffix(frame.File, "runtime_lineinfo_stack_test.go") || frame.Line != wantCallerLine {
				t.Fatalf("caller frame = %s:%d, want line %d", frame.File, frame.Line, wantCallerLine)
			}
			seenCaller = true
		}
		if seenSelf && seenCaller {
			return
		}
		if !more {
			break
		}
	}
	t.Fatalf("CallersFrames missing frames: self=%v caller=%v", seenSelf, seenCaller)
}

//go:noinline
func checkRuntimeFuncForPC(t *testing.T) {
	pc, file, line, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller did not return a pc")
	}
	fn := runtime.FuncForPC(pc)
	if fn == nil || !strings.HasSuffix(fn.Name(), ".checkRuntimeFuncForPC") {
		t.Fatalf("FuncForPC name = %v", runtimeFunctionName(fn))
	}
	if fn.Entry() == 0 {
		t.Fatal("FuncForPC entry is zero")
	}
	gotFile, gotLine := fn.FileLine(pc)
	if gotFile != file || gotLine != line {
		t.Fatalf("Func.FileLine = %s:%d, want %s:%d", gotFile, gotLine, file, line)
	}
}

//go:noinline
func runtimeLineInfoEntryTarget() int {
	return 7
}

func checkRuntimeFuncForPCFunctionValue(t *testing.T) {
	if runtimeLineInfoEntryTarget() != 7 {
		t.Fatal("bad function target")
	}
	pc := reflect.ValueOf(runtimeLineInfoEntryTarget).Pointer()
	fn := runtime.FuncForPC(pc)
	if fn == nil || !strings.HasSuffix(fn.Name(), ".runtimeLineInfoEntryTarget") {
		t.Fatalf("function-value FuncForPC name = %v", runtimeFunctionName(fn))
	}
	if fn.Entry() == 0 {
		t.Fatal("function-value entry is zero")
	}
	file, line := fn.FileLine(pc)
	if !strings.HasSuffix(file, "runtime_lineinfo_stack_test.go") || line == 0 {
		t.Fatalf("function-value FileLine = %s:%d", file, line)
	}
}

//go:noinline
func checkRuntimeStack(t *testing.T) {
	var buf [4096]byte
	n := runtime.Stack(buf[:], false)
	stack := string(buf[:n])
	if !strings.Contains(stack, "checkRuntimeStack") || !strings.Contains(stack, "runtime_lineinfo_stack_test.go:") {
		t.Fatalf("runtime.Stack is missing the current frame:\n%s", stack)
	}
}

//go:noinline
func checkRuntimePanicStack(t *testing.T) {
	var stack string
	func() {
		defer func() {
			if recover() == nil {
				t.Fatal("missing bounds panic")
			}
			stack = string(debug.Stack())
		}()
		values := []int{1, 2, 3}
		_ = values[3]
	}()
	if !strings.Contains(stack, "checkRuntimePanicStack") || !strings.Contains(stack, "runtime_lineinfo_stack_test.go:") {
		t.Fatalf("debug.Stack is missing the panic frame:\n%s", stack)
	}
}

func runtimeFunctionName(fn *runtime.Func) string {
	if fn == nil {
		return "<nil>"
	}
	return fn.Name()
}

type concurrentRuntimeTarget struct {
	fn     func()
	suffix string
}

var concurrentRuntimeTargets = []concurrentRuntimeTarget{
	{concurrentRuntimeTarget0, ".concurrentRuntimeTarget0"},
	{concurrentRuntimeTarget1, ".concurrentRuntimeTarget1"},
	{concurrentRuntimeTarget2, ".concurrentRuntimeTarget2"},
	{concurrentRuntimeTarget3, ".concurrentRuntimeTarget3"},
}

//go:noinline
func concurrentRuntimeTarget0() {}

//go:noinline
func concurrentRuntimeTarget1() {}

//go:noinline
func concurrentRuntimeTarget2() {}

//go:noinline
func concurrentRuntimeTarget3() {}

func TestRuntimeFuncInfoConcurrentFirstUse(t *testing.T) {
	const workers = 32
	const rounds = 1000
	start := make(chan struct{})
	errc := make(chan string, workers)
	var wg sync.WaitGroup
	for i := 0; i < workers; i++ {
		target := concurrentRuntimeTargets[i%len(concurrentRuntimeTargets)]
		pc := reflect.ValueOf(target.fn).Pointer()
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start
			for j := 0; j < rounds; j++ {
				fn := runtime.FuncForPC(pc)
				if fn == nil || !strings.HasSuffix(fn.Name(), target.suffix) {
					errc <- "bad target function: " + runtimeFunctionName(fn)
					return
				}
				if err := checkConcurrentRuntimeInfo(); err != "" {
					errc <- err
					return
				}
			}
		}()
	}
	close(start)
	wg.Wait()
	close(errc)
	for err := range errc {
		t.Error(err)
	}
}

//go:noinline
func checkConcurrentRuntimeInfo() string {
	pc, file, line, ok := runtime.Caller(0)
	if !ok || !strings.HasSuffix(file, "runtime_lineinfo_stack_test.go") || line == 0 {
		return "bad caller: " + file + ":" + strconv.Itoa(line)
	}
	fn := runtime.FuncForPC(pc)
	if fn == nil || !strings.HasSuffix(fn.Name(), ".checkConcurrentRuntimeInfo") {
		return "bad function: " + runtimeFunctionName(fn)
	}
	gotFile, gotLine := fn.FileLine(pc)
	if gotFile != file || gotLine != line {
		return "bad FileLine: " + gotFile + ":" + strconv.Itoa(gotLine)
	}
	var pcs [8]uintptr
	n := runtime.Callers(0, pcs[:])
	frames := runtime.CallersFrames(pcs[:n])
	for {
		frame, more := frames.Next()
		if strings.HasSuffix(frame.Function, ".checkConcurrentRuntimeInfo") {
			if !strings.HasSuffix(frame.File, "runtime_lineinfo_stack_test.go") || frame.Line == 0 {
				return "bad frame: " + frame.File + ":" + strconv.Itoa(frame.Line)
			}
			return ""
		}
		if !more {
			return "missing frame"
		}
	}
}

func markerLine(source, marker string) int {
	line := 1
	for _, part := range strings.SplitAfter(source, "\n") {
		if strings.Contains(part, marker) {
			return line
		}
		line++
	}
	panic("missing marker " + marker)
}
