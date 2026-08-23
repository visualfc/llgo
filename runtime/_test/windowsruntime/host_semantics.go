package main

import (
	"syscall"
	"unicode/utf16"
	"unsafe"
)

//go:linkname cMaxprocs C.llgo_maxprocs
func cMaxprocs() int32

func checkCoreMapRandStreams() {
	const (
		workers = 8
		entries = 64
	)
	start := make(chan struct{})
	orders := make(chan [entries]int, workers)
	for i := 0; i < workers; i++ {
		go func() {
			<-start
			values := make(map[int]struct{}, entries)
			for value := 0; value < entries; value++ {
				values[value] = struct{}{}
			}
			var order [entries]int
			index := 0
			for value := range values {
				order[index] = value
				index++
			}
			orders <- order
		}()
	}
	close(start)

	first := <-orders
	allEqual := true
	for i := 1; i < workers; i++ {
		if <-orders != first {
			allEqual = false
		}
	}
	if allEqual {
		panic("Windows goroutines share identical core map random streams")
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

func checkUnicodeConsolePrint() {
	kernel32 := syscall.NewLazyDLL("kernel32.dll")
	getConsoleOutputCP := kernel32.NewProc("GetConsoleOutputCP")
	allocConsole := kernel32.NewProc("AllocConsole")
	freeConsole := kernel32.NewProc("FreeConsole")
	if codePage, _, _ := getConsoleOutputCP.Call(); codePage == 0 {
		if result, _, _ := allocConsole.Call(); result == 0 {
			panic("AllocConsole failed")
		}
		defer freeConsole.Call()
	}
	setConsoleOutputCP := kernel32.NewProc("SetConsoleOutputCP")
	originalCodePage, _, _ := getConsoleOutputCP.Call()
	if result, _, _ := setConsoleOutputCP.Call(437); result == 0 {
		panic("SetConsoleOutputCP failed")
	}
	defer setConsoleOutputCP.Call(originalCodePage)

	const (
		genericRead           = uintptr(0x80000000)
		genericWrite          = uintptr(0x40000000)
		fileShareRead         = uintptr(1)
		fileShareWrite        = uintptr(2)
		consoleTextmodeBuffer = uintptr(1)
		stdErrorHandle        = ^uintptr(11)
	)
	createBuffer := kernel32.NewProc("CreateConsoleScreenBuffer")
	handle, _, _ := createBuffer.Call(
		genericRead|genericWrite,
		fileShareRead|fileShareWrite,
		0,
		consoleTextmodeBuffer,
		0,
	)
	if handle == 0 || handle == ^uintptr(0) {
		panic("CreateConsoleScreenBuffer failed")
	}
	defer syscall.CloseHandle(syscall.Handle(handle))

	getStdHandle := kernel32.NewProc("GetStdHandle")
	setStdHandle := kernel32.NewProc("SetStdHandle")
	oldStderr, _, _ := getStdHandle.Call(stdErrorHandle)
	if result, _, _ := setStdHandle.Call(stdErrorHandle, handle); result == 0 {
		panic("SetStdHandle(stderr) failed")
	}
	defer setStdHandle.Call(stdErrorHandle, oldStderr)

	// Force a legacy code page so writing UTF-8 bytes through WriteFile would
	// be observably wrong. The runtime must use WriteConsoleW instead.
	const text = "llgo-控制台-é"
	println(text)
	var got [64]uint16
	var read uint32
	readConsole := kernel32.NewProc("ReadConsoleOutputCharacterW")
	result, _, _ := readConsole.Call(
		handle,
		uintptr(unsafe.Pointer(&got[0])),
		uintptr(len(got)),
		0,
		uintptr(unsafe.Pointer(&read)),
	)
	if result == 0 {
		panic("ReadConsoleOutputCharacterW failed")
	}
	want := utf16.Encode([]rune(text))
	if int(read) < len(want) {
		panic("Windows console print was truncated")
	}
	for i := range want {
		if got[i] != want[i] {
			panic("Windows console print did not preserve Unicode")
		}
	}
}
