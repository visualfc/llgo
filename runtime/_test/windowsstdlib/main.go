package main

import (
	"math"
	"runtime"
	"sync"
	"syscall"
	"time"
	"unsafe"

	c "github.com/goplus/llgo/runtime/internal/clite"
	"github.com/goplus/llgo/runtime/internal/clite/libuv"
	_ "github.com/goplus/llgo/runtime/internal/runtime"
)

const LLGoFiles = "_wrap/syscall.c"

//go:linkname cSum12 C.llgo_windows_sum12
func cSum12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 uintptr) uintptr

//go:linkname cAddDouble C.llgo_windows_add_double
func cAddDouble(a, b float64) float64

func testWindowsSyscalls() {
	sum12 := uintptr(c.Func(cSum12))
	wantSum := uintptr(78)
	r1, _, errNo := syscall.SyscallN(sum12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
	if r1 != wantSum || errNo != 0 {
		panic("SyscallN did not pass 12 C ABI arguments")
	}
	var maxArgs [42]uintptr
	for i := 0; i < 12; i++ {
		maxArgs[i] = uintptr(i + 1)
	}
	r1, _, errNo = syscall.SyscallN(sum12, maxArgs[:]...)
	if r1 != wantSum || errNo != 0 {
		panic("SyscallN did not preserve its 42-argument stack frame")
	}
	r1, _, errNo = syscall.Syscall12(sum12, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
	if r1 != wantSum || errNo != 0 {
		panic("Syscall12 did not pass 12 C ABI arguments")
	}

	kernel32 := syscall.NewLazyDLL("kernel32.dll")
	pid, _, _ := kernel32.NewProc("GetCurrentProcessId").Call()
	if pid == 0 {
		panic("zero-argument SyscallN returned an invalid process id")
	}
	if err := kernel32.NewProc("llgo_missing_windows_procedure").Find(); err == nil {
		panic("GetProcAddress failure lost its error")
	}
	if _, err := syscall.LoadDLL("llgo_missing_windows_library.dll"); err == nil {
		panic("LoadLibraryExW failure lost its error")
	}
	setLastError := kernel32.NewProc("SetLastError")
	const wantErr = syscall.Errno(0x4d2)
	_, _, err := setLastError.Call(uintptr(wantErr))
	if err != wantErr {
		panic("SyscallN did not preserve GetLastError")
	}
	if runtime.GOARCH == "amd64" {
		addDouble := uintptr(c.Func(cAddDouble))
		_, bits, _ := syscall.SyscallN(
			addDouble,
			uintptr(math.Float64bits(1.25)),
			uintptr(math.Float64bits(2.5)),
		)
		if got := math.Float64frombits(uint64(bits)); got != 3.75 {
			panic("SyscallN did not preserve amd64 floating-point registers")
		}
	}

	var wsa syscall.WSAData
	if err := syscall.WSAStartup(0x202, &wsa); err != nil {
		panic("WSAStartup failed")
	}
	defer syscall.WSACleanup()
	var received uint32
	var flags uint32
	var fromLen int32
	if err := syscall.WSARecvFrom(
		syscall.InvalidHandle, nil, 0, &received, &flags,
		nil, &fromLen, nil, nil,
	); err == nil {
		panic("nine-argument WSARecvFrom unexpectedly succeeded")
	}

	const envKey = "LLGO_WINDOWS_STDLIB_SMOKE"
	if err := syscall.Setenv(envKey, "ok"); err != nil {
		panic("Setenv failed")
	}
	if value, ok := syscall.Getenv(envKey); !ok || value != "ok" {
		panic("Getenv did not observe Setenv")
	}
	if err := syscall.Unsetenv(envKey); err != nil {
		panic("Unsetenv failed")
	}
}

func testLibuvHandleSizes() {
	tests := [...]struct {
		typeof libuv.HandleType
		got    uintptr
	}{
		{libuv.ASYNC, unsafe.Sizeof(libuv.Async{})},
		{libuv.TIMER, unsafe.Sizeof(libuv.Timer{})},
		{libuv.SIGNAL, unsafe.Sizeof(libuv.Signal{})},
	}
	for _, test := range tests {
		if want := libuv.HandleSize(test.typeof); test.got != want {
			panic("libuv handle storage size does not match the installed DLL")
		}
	}
}

func main() {
	var once sync.Once
	done := make(chan struct{}, 4)
	value := 0
	for i := 0; i < 4; i++ {
		go func() {
			once.Do(func() { value = 42 })
			done <- struct{}{}
		}()
	}
	for i := 0; i < 4; i++ {
		<-done
	}
	if value != 42 {
		panic("sync.Once ran incorrectly")
	}
	testWindowsSyscalls()
	testLibuvHandleSizes()

	start := time.Now()
	time.Sleep(time.Millisecond)
	if time.Since(start) < 0 {
		panic("monotonic clock moved backwards")
	}
	println("windows stdlib smoke: ok")
}
