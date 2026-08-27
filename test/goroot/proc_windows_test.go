//go:build windows

package goroot

import (
	"fmt"
	"os/exec"
	"reflect"
	"sort"
	"syscall"
	"testing"
	"unsafe"

	"golang.org/x/sys/windows"
)

type windowsProcessInfo struct {
	parentPID uint32
	rss       uint64
}

func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{CreationFlags: windows.CREATE_NEW_PROCESS_GROUP}
}

func killProcessTree(cmd *exec.Cmd) {
	if cmd.Process == nil {
		return
	}
	rootPID := uint32(cmd.Process.Pid)
	if processes, err := snapshotWindowsProcesses(); err == nil {
		processTree := windowsProcessTree(rootPID, processes)
		for i := len(processTree) - 1; i > 0; i-- {
			terminateWindowsProcess(processTree[i])
		}
	}
	_ = cmd.Process.Kill()
}

func resourceMonitoringSupported() bool { return true }

func processGroupRSS(processGroupID int) (uint64, error) {
	processes, err := snapshotWindowsProcesses()
	if err != nil {
		return 0, err
	}
	rootPID := uint32(processGroupID)
	if _, ok := processes[rootPID]; !ok {
		return 0, fmt.Errorf("process %d is no longer present", processGroupID)
	}
	var total uint64
	for _, pid := range windowsProcessTree(rootPID, processes) {
		total += processes[pid].rss
	}
	return total, nil
}

func snapshotWindowsProcesses() (map[uint32]windowsProcessInfo, error) {
	bufferSize := uint32(1 << 20)
	for {
		buffer := make([]byte, bufferSize)
		var required uint32
		err := windows.NtQuerySystemInformation(
			windows.SystemProcessInformation,
			unsafe.Pointer(&buffer[0]),
			uint32(len(buffer)),
			&required,
		)
		if err == windows.STATUS_INFO_LENGTH_MISMATCH {
			if required > bufferSize {
				bufferSize = required + 64<<10
			} else {
				bufferSize *= 2
			}
			continue
		}
		if err != nil {
			return nil, err
		}
		return parseWindowsProcessSnapshot(buffer)
	}
}

func parseWindowsProcessSnapshot(buffer []byte) (map[uint32]windowsProcessInfo, error) {
	processes := make(map[uint32]windowsProcessInfo)
	entrySize := uint32(unsafe.Sizeof(windows.SYSTEM_PROCESS_INFORMATION{}))
	for offset := uint32(0); ; {
		if offset > uint32(len(buffer)) || uint32(len(buffer))-offset < entrySize {
			return nil, fmt.Errorf("truncated Windows process snapshot at offset %d", offset)
		}
		entry := (*windows.SYSTEM_PROCESS_INFORMATION)(unsafe.Pointer(&buffer[offset]))
		pid := uint32(entry.UniqueProcessID)
		processes[pid] = windowsProcessInfo{
			parentPID: uint32(entry.InheritedFromUniqueProcessID),
			rss:       uint64(entry.WorkingSetSize),
		}
		if entry.NextEntryOffset == 0 {
			return processes, nil
		}
		if entry.NextEntryOffset < entrySize || entry.NextEntryOffset > uint32(len(buffer))-offset {
			return nil, fmt.Errorf("invalid Windows process snapshot offset %d at %d", entry.NextEntryOffset, offset)
		}
		offset += entry.NextEntryOffset
	}
}

func windowsProcessTree(rootPID uint32, processes map[uint32]windowsProcessInfo) []uint32 {
	children := make(map[uint32][]uint32)
	for pid, process := range processes {
		if pid != rootPID {
			children[process.parentPID] = append(children[process.parentPID], pid)
		}
	}
	for parentPID := range children {
		sort.Slice(children[parentPID], func(i, j int) bool {
			return children[parentPID][i] < children[parentPID][j]
		})
	}
	tree := []uint32{rootPID}
	seen := map[uint32]bool{rootPID: true}
	for i := 0; i < len(tree); i++ {
		for _, childPID := range children[tree[i]] {
			if !seen[childPID] {
				seen[childPID] = true
				tree = append(tree, childPID)
			}
		}
	}
	return tree
}

func terminateWindowsProcess(pid uint32) {
	handle, err := windows.OpenProcess(windows.PROCESS_TERMINATE, false, pid)
	if err != nil {
		return
	}
	defer windows.CloseHandle(handle)
	_ = windows.TerminateProcess(handle, 1)
}

func TestWindowsProcessTree(t *testing.T) {
	processes := map[uint32]windowsProcessInfo{
		10: {parentPID: 1},
		11: {parentPID: 10},
		12: {parentPID: 10},
		13: {parentPID: 11},
		14: {parentPID: 99},
		15: {parentPID: 15},
	}
	got := windowsProcessTree(10, processes)
	want := []uint32{10, 11, 12, 13}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("windowsProcessTree() = %v, want %v", got, want)
	}
}
