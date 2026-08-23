package main

import "runtime"

func nilDeferredCallReplacesPanic() {
	var fn func()
	defer fn()
	panic("original panic")
}

func checkNilFunctionFaultOrigin() {
	var got any
	foundOrigin := false
	foundCaller := false
	func() {
		defer func() {
			got = recover()
			var pcs [32]uintptr
			n := runtime.Callers(0, pcs[:])
			frames := runtime.CallersFrames(pcs[:n])
			for {
				frame, more := frames.Next()
				if hasSuffix(frame.Function, ".nilDeferredCallReplacesPanic") {
					foundOrigin = true
				}
				if hasSuffix(frame.Function, ".checkNilFunctionFaultOrigin") {
					foundCaller = true
				}
				if !more {
					break
				}
			}
		}()
		nilDeferredCallReplacesPanic()
	}()
	if got == nil || got == "original panic" {
		panic("nil deferred call did not replace the original panic")
	}
	if !foundOrigin || !foundCaller {
		panic("nil deferred call lost its Go caller traceback")
	}
}
