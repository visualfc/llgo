//go:build llgo

package llgoext

import (
	"os"
	"runtime"
	"strings"
	"testing"
	_ "unsafe"
)

//go:linkname runtimeInfoRenamedPC github.com/xgo-dev/llgo/test/llgoext.runtimeInfoRenamedPCSymbol
//go:noinline
func runtimeInfoRenamedPC() uintptr {
	pc, _, _, ok := runtime.Caller(0)
	if !ok {
		panic("missing renamed pc")
	}
	return pc
}

func TestRuntimeFuncInfoKeepsSourceName(t *testing.T) {
	fn := runtime.FuncForPC(runtimeInfoRenamedPC())
	if fn == nil || !strings.HasSuffix(fn.Name(), ".runtimeInfoRenamedPC") {
		name := "<nil>"
		if fn != nil {
			name = fn.Name()
		}
		t.Fatalf("renamed function = %q, want source name suffix .runtimeInfoRenamedPC", name)
	}
}

func TestRuntimeFuncInfoFramePCStatementLine(t *testing.T) {
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("current source file is unavailable")
	}
	source, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	want := 0
	for index, line := range strings.Split(string(source), "\n") {
		if strings.HasSuffix(strings.TrimSpace(line), "// CALLERS_PC_MARK") {
			want = index + 1
			break
		}
	}
	if want == 0 {
		t.Fatal("CALLERS_PC_MARK is missing")
	}
	checkRuntimeFuncInfoFramePCStatementLine(t, want)
}

//go:noinline
func checkRuntimeFuncInfoFramePCStatementLine(t *testing.T, want int) {
	var pcs [8]uintptr
	n := runtime.Callers(0, pcs[:]) // CALLERS_PC_MARK
	frames := runtime.CallersFrames(pcs[:n])
	for {
		frame, more := frames.Next()
		if strings.HasSuffix(frame.Function, ".checkRuntimeFuncInfoFramePCStatementLine") {
			fn := runtime.FuncForPC(frame.PC - 1)
			if fn == nil {
				t.Fatal("FuncForPC(pc-1) returned nil")
			}
			file, line := fn.FileLine(frame.PC - 1)
			if !strings.HasSuffix(file, "runtime_funcinfo_test.go") || line != want {
				t.Fatalf("Func.FileLine(pc-1) = %s:%d, want line %d", file, line, want)
			}
			return
		}
		if !more {
			break
		}
	}
	t.Fatal("CallersFrames is missing the current function")
}
