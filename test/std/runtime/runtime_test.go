package runtime_test

import (
	"runtime"
	"strings"
	"testing"
)

func TestRuntimeInformationAndCallers(t *testing.T) {
	if runtime.GOOS == "" || runtime.GOARCH == "" || runtime.Compiler == "" {
		t.Fatal("runtime target information is empty")
	}
	if runtime.NumCPU() < 1 || runtime.GOMAXPROCS(0) < 1 {
		t.Fatal("runtime CPU information is invalid")
	}
	if !strings.HasPrefix(runtime.Version(), "go") {
		t.Fatalf("Version = %q", runtime.Version())
	}
	pcs := make([]uintptr, 16)
	n := runtime.Callers(0, pcs)
	if n == 0 {
		t.Fatal("Callers returned no frames")
	}
	frames := runtime.CallersFrames(pcs[:n])
	foundTest := false
	for {
		frame, more := frames.Next()
		if strings.Contains(frame.Function, "TestRuntimeInformationAndCallers") {
			if frame.File == "" || frame.Line == 0 {
				t.Fatalf("test frame has incomplete source information: %#v", frame)
			}
			foundTest = true
			break
		}
		if !more {
			break
		}
	}
	if !foundTest {
		t.Fatal("CallersFrames did not report TestRuntimeInformationAndCallers")
	}
}
