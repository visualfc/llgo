//go:build windows

package runtime

func pcSymbolIsWrapper(sym pcSymbol) bool {
	return uint32(sym.startLine)&runtimeFuncInfoLineWrapper != 0
}

// elideWrapperCalling mirrors Go's runtime.elideWrapperCalling. A generated
// wrapper normally has no logical stack frame, but must remain visible when
// its own receiver/conversion check called a panic helper instead of the
// wrapped method.
func elideWrapperCalling(callee string) bool {
	switch callee {
	case "runtime.gopanic", "runtime.sigpanic", "runtime.panicwrap",
		"github.com/xgo-dev/llgo/runtime/internal/runtime.PanicWrapNilPointer":
		return false
	}
	return true
}
