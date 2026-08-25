package main

import (
	"runtime"
	"syscall"
	"time"
	"unsafe"

	nativesync "github.com/xgo-dev/llgo/runtime/internal/sync"
	// The smoke package lives below the LLGo runtime root, whose packages are
	// excluded from the ordinary need-runtime signal. Import the core runtime
	// explicitly so its global state is initialized before the low-level test.
	_ "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

const LLGoFiles = "_wrap/runtime.c"

//go:linkname currentThreadID C.llgo_windows_current_thread_id
func currentThreadID() uint32

//go:linkname traceClockNow runtime.traceClockNow
func traceClockNow() uint64

//go:linkname traceClockUnitsPerSecond runtime/trace.runtime_traceClockUnitsPerSecond
func traceClockUnitsPerSecond() uint64

//go:linkname cMaxprocs C.llgo_maxprocs
func cMaxprocs() int32

func checkRecover() {
	defer func() {
		if value := recover(); value != "windows panic smoke" {
			panic("wrong recovered value")
		}
	}()
	panic("windows panic smoke")
}

func checkThreadSemantics() {
	if runtime.GOMAXPROCS(0) < 1 {
		panic("Windows GOMAXPROCS returned an invalid processor count")
	}
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()
	threadID := currentThreadID()
	for i := 0; i < 64; i++ {
		runtime.Gosched()
		if currentThreadID() != threadID {
			panic("Windows LockOSThread allowed thread migration")
		}
	}
}

func checkProcessAffinityCPUCount() {
	kernel32 := syscall.NewLazyDLL("kernel32.dll")
	process, _, _ := kernel32.NewProc("GetCurrentProcess").Call()
	var processMask, systemMask uintptr
	result, _, _ := kernel32.NewProc("GetProcessAffinityMask").Call(
		process,
		uintptr(unsafe.Pointer(&processMask)),
		uintptr(unsafe.Pointer(&systemMask)),
	)
	if result == 0 || processMask == 0 {
		panic("GetProcessAffinityMask failed")
	}
	singleCPU := processMask & -processMask
	result, _, _ = kernel32.NewProc("SetProcessAffinityMask").Call(process, singleCPU)
	if result == 0 {
		panic("SetProcessAffinityMask(single CPU) failed")
	}
	got := cMaxprocs()
	result, _, _ = kernel32.NewProc("SetProcessAffinityMask").Call(process, processMask)
	if result == 0 {
		panic("restoring the process affinity mask failed")
	}
	if got != 1 {
		panic("Windows CPU count ignored the process affinity mask")
	}
}

func checkTraceClock() {
	if traceClockUnitsPerSecond() != 1_000_000_000/64 {
		panic("Windows trace clock reports the wrong frequency")
	}
	start := traceClockNow()
	for attempt := 0; attempt < 1_000_000; attempt++ {
		if traceClockNow() > start {
			return
		}
	}
	panic("Windows trace clock did not advance")
}

func checkWallClock() {
	now := time.Now()
	if now.Unix() < 1_500_000_000 {
		panic("Windows wall clock returned an invalid timestamp")
	}
}

func main() {
	values := make(chan int)
	go func() {
		values <- 42
	}()
	if value := <-values; value != 42 {
		panic("wrong channel value")
	}

	var once nativesync.Once
	done := make(chan struct{}, 4)
	onceValue := 0
	for i := 0; i < 4; i++ {
		go func() {
			if result := once.Do(func() { onceValue = 7 }); result != 0 {
				panic("native once failed")
			}
			done <- struct{}{}
		}()
	}
	for i := 0; i < 4; i++ {
		<-done
	}
	if onceValue != 7 {
		panic("native once ran incorrectly")
	}

	checkRecover()
	checkThreadSemantics()
	checkProcessAffinityCPUCount()
	checkTraceClock()
	checkWallClock()
	checkGC()
	// Returning from main must terminate the process even while another
	// goroutine is blocked. This is observable on Windows because goroutines
	// are currently backed by host threads.
	lingeringStarted := make(chan struct{})
	go func() {
		close(lingeringStarted)
		select {}
	}()
	<-lingeringStarted
	println("windows runtime smoke: ok")
}
