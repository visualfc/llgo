//go:build !darwin && !linux && !windows

package goroot

func systemMemoryMonitoringSupported() bool { return false }

func readSystemMemoryState() (systemMemoryState, error) {
	return systemMemoryState{}, nil
}
