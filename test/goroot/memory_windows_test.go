//go:build windows

package goroot

import (
	"fmt"
	"testing"
	"unsafe"

	"golang.org/x/sys/windows"
)

var globalMemoryStatusEx = windows.NewLazySystemDLL("kernel32.dll").NewProc("GlobalMemoryStatusEx")

type windowsMemoryStatus struct {
	length            uint32
	memoryLoad        uint32
	totalPhysical     uint64
	availablePhysical uint64
	totalPageFile     uint64
	availablePageFile uint64
	totalVirtual      uint64
	availableVirtual  uint64
	availableExtended uint64
}

func systemMemoryMonitoringSupported() bool { return true }

func readSystemMemoryState() (systemMemoryState, error) {
	status := windowsMemoryStatus{length: uint32(unsafe.Sizeof(windowsMemoryStatus{}))}
	ok, _, callErr := globalMemoryStatusEx.Call(uintptr(unsafe.Pointer(&status)))
	if ok == 0 {
		return systemMemoryState{}, fmt.Errorf("GlobalMemoryStatusEx: %w", callErr)
	}
	return windowsSystemMemoryState(status)
}

func windowsSystemMemoryState(status windowsMemoryStatus) (systemMemoryState, error) {
	if status.totalPhysical == 0 || status.availablePhysical > status.totalPhysical {
		return systemMemoryState{}, fmt.Errorf(
			"invalid Windows physical memory totals: total=%d available=%d",
			status.totalPhysical,
			status.availablePhysical,
		)
	}
	state := systemMemoryState{
		freePercent: int(status.availablePhysical * 100 / status.totalPhysical),
		swapPresent: status.totalPageFile > status.totalPhysical,
	}
	if status.availablePageFile > status.availablePhysical {
		state.swapFree = status.availablePageFile - status.availablePhysical
	}
	return state, nil
}

func TestWindowsSystemMemoryState(t *testing.T) {
	state, err := readSystemMemoryState()
	if err != nil {
		t.Fatal(err)
	}
	if state.freePercent < 0 || state.freePercent > 100 {
		t.Fatalf("free memory percentage = %d, want 0..100", state.freePercent)
	}
}

func TestWindowsSystemMemoryStateRejectsInvalidTotals(t *testing.T) {
	_, err := windowsSystemMemoryState(windowsMemoryStatus{
		totalPhysical:     1024,
		availablePhysical: 2048,
	})
	if err == nil {
		t.Fatal("invalid physical memory totals were accepted")
	}
}
