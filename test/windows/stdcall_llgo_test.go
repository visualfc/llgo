//go:build llgo && windows

package windowstesting

import (
	"testing"
	"unsafe"
)

type initOnce struct {
	value unsafe.Pointer
}

//llgo:type stdcall
type initOnceCallback func(*initOnce, unsafe.Pointer, *unsafe.Pointer) int32

//go:linkname initOnceExecuteOnce stdcall.InitOnceExecuteOnce
func initOnceExecuteOnce(*initOnce, initOnceCallback, unsafe.Pointer, *unsafe.Pointer) int32

//go:linkname sleep stdcall.Sleep
func sleep(milliseconds uint32)

var initOnceCallbackCalls int

func recordInitOnce(_ *initOnce, parameter unsafe.Pointer, context *unsafe.Pointer) int32 {
	initOnceCallbackCalls++
	*context = parameter
	return 1
}

func TestStdcallDirectCallAndCallback(t *testing.T) {
	// On windows/386, both calls exercise callee stack cleanup. Repetition makes
	// an incorrect cdecl declaration fail before the callback assertions.
	for range 32 {
		sleep(0)
	}

	initOnceCallbackCalls = 0
	var once initOnce
	token := new(byte)
	var context unsafe.Pointer
	if ok := initOnceExecuteOnce(&once, recordInitOnce, unsafe.Pointer(token), &context); ok == 0 {
		t.Fatal("InitOnceExecuteOnce failed")
	}
	if initOnceCallbackCalls != 1 || context != unsafe.Pointer(token) {
		t.Fatalf("first call: callback calls = %d, context = %p", initOnceCallbackCalls, context)
	}

	context = nil
	if ok := initOnceExecuteOnce(&once, recordInitOnce, nil, &context); ok == 0 {
		t.Fatal("second InitOnceExecuteOnce failed")
	}
	if initOnceCallbackCalls != 1 || context != unsafe.Pointer(token) {
		t.Fatalf("second call reran callback or lost context: calls = %d, context = %p", initOnceCallbackCalls, context)
	}
}
