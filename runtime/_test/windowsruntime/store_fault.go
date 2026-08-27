package main

import "runtime"

func checkStoreNilFaultLine() {
	recovered := false
	func() {
		defer func() {
			if recover() == nil {
				panic("Windows nil store was not recoverable")
			}
			var pcs [32]uintptr
			n := runtime.Callers(0, pcs[:])
			frames := runtime.CallersFrames(pcs[:n])
			for {
				frame, more := frames.Next()
				if hasSuffix(frame.Function, ".windowsStoreNilFault") {
					if !hasSuffix(frame.File, "windows_store_fault.go") || frame.Line != 167 {
						panic("Windows nil store reported the wrong source line")
					}
					recovered = true
					return
				}
				if !more {
					break
				}
			}
			panic("Windows nil store traceback lost the faulting frame")
		}()
		windowsStoreNilFault(nil)
	}()
	if !recovered {
		panic("Windows nil store did not complete recovery")
	}
}

//go:noinline
func windowsStoreNilFault(p *byte) {
//line windows_store_fault.go:167
	*p = 1
}
