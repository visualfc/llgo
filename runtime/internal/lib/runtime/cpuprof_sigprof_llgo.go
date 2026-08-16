//go:build !baremetal && !wasm && (darwin || linux) && (amd64 || arm64)

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	latomic "sync/atomic"
)

const maxCPUProfileStack = 64

// SIGPROF is 27 on the native Darwin/Linux architectures supported here.
const cpuProfileSignal = 27

//go:linkname c_cpuProfileStart C.llgo_cpu_profile_start
func c_cpuProfileStart(hz int32) int32

//go:linkname c_cpuProfileStop C.llgo_cpu_profile_stop
func c_cpuProfileStop()

//go:linkname c_cpuProfileRead C.llgo_cpu_profile_read
func c_cpuProfileRead(p unsafe.Pointer, n int32) int32

//go:linkname c_cpuProfileTakeLost C.llgo_cpu_profile_take_lost
func c_cpuProfileTakeLost() uint64

//go:linkname c_cpuProfileEmpty C.llgo_cpu_profile_empty
func c_cpuProfileEmpty() int32

//go:linkname c_cpuProfileSignalUpdateBegin C.llgo_cpu_profile_signal_update_begin
func c_cpuProfileSignalUpdateBegin()

//go:linkname c_cpuProfileSignalUpdateEnd C.llgo_cpu_profile_signal_update_end
func c_cpuProfileSignalUpdateEnd() int32

var (
	cpuProfileRate          int32
	cpuProfileOpen          uint32
	cpuProfilePeriodPending uint32
	cpuProfilePeriodRecord  = [3]uint64{3, 0, 100}
	cpuProfilePeriodTags    [1]unsafe.Pointer
)

func cpuProfileSignalUpdateBegin(sig uint32) bool {
	if sig != cpuProfileSignal {
		return false
	}
	c_cpuProfileSignalUpdateBegin()
	return true
}

func cpuProfileSignalUpdateEnd(locked bool) {
	if !locked {
		return
	}
	if c_cpuProfileSignalUpdateEnd() != 0 {
		print("runtime: failed to restore CPU profiling SIGPROF handler.\n")
	}
}

// SetCPUProfileRate starts or stops process CPU-time sampling. The signal
// handler and its ring buffer live in profile.c so the sampling path neither
// allocates Go memory nor enters the Go runtime.
func SetCPUProfileRate(hz int) {
	if hz < 0 {
		hz = 0
	}
	if hz > 1000000 {
		hz = 1000000
	}
	if hz == 0 {
		if latomic.LoadInt32(&cpuProfileRate) != 0 {
			c_cpuProfileStop()
			latomic.StoreInt32(&cpuProfileRate, 0)
		}
		return
	}
	if !latomic.CompareAndSwapUint32(&cpuProfileOpen, 0, 1) {
		print("runtime: cannot set cpu profile rate until previous profile has finished.\n")
		return
	}

	switch c_cpuProfileStart(int32(hz)) {
	case 1:
		cpuProfilePeriodRecord[2] = uint64(hz)
		latomic.StoreUint32(&cpuProfilePeriodPending, 1)
		latomic.StoreInt32(&cpuProfileRate, int32(hz))
	case 0:
		// Samples from a stopped profile have not been drained yet.
		latomic.StoreUint32(&cpuProfileOpen, 0)
		print("runtime: cannot set cpu profile rate until previous profile has finished.\n")
	default:
		// Preserve runtime/pprof's stream contract even if the OS rejected
		// SIGPROF setup: the caller gets a valid empty profile, not a
		// malformed stream or a blocked writer.
		cpuProfilePeriodRecord[2] = uint64(hz)
		latomic.StoreUint32(&cpuProfilePeriodPending, 1)
	}
}

//go:linkname runtime_pprof_readProfile runtime/pprof.readProfile
func runtime_pprof_readProfile() (data []uint64, tags []unsafe.Pointer, eof bool) {
	if latomic.CompareAndSwapUint32(&cpuProfilePeriodPending, 1, 0) {
		return cpuProfilePeriodRecord[:], cpuProfilePeriodTags[:], false
	}

	var pcs [maxCPUProfileStack]uintptr
	for {
		lost := c_cpuProfileTakeLost()
		n := int(c_cpuProfileRead(unsafe.Pointer(&pcs[0]), maxCPUProfileStack))
		if lost != 0 || n != 0 {
			// runtime/pprof waits 100 ms between reads on Darwin. Drain a
			// chunk, rather than one sample, so a 100 Hz producer cannot
			// outrun the writer.
			data = make([]uint64, 0, 1024)
			tags = make([]unsafe.Pointer, 0, 16)
			if lost != 0 {
				data = append(data, 4, 0, 0, lost)
				tags = append(tags, nil)
			}
			for records := 0; n != 0 && records < 256; records++ {
				data = append(data, uint64(3+n), 0, 1)
				for i := 0; i < n; i++ {
					data = append(data, uint64(pcs[i]))
				}
				tags = append(tags, nil)
				if records == 255 {
					break
				}
				n = int(c_cpuProfileRead(unsafe.Pointer(&pcs[0]), maxCPUProfileStack))
			}
			return data, tags, false
		}
		if latomic.LoadInt32(&cpuProfileRate) == 0 && c_cpuProfileEmpty() != 0 {
			latomic.StoreUint32(&cpuProfileOpen, 0)
			return nil, nil, true
		}

		// Linux's profile writer expects readProfile to block. Keep the wait
		// out of the signal path and poll at the 100 Hz profiler's period;
		// Darwin already sleeps in pprof.
		c.Usleep(10000)
	}
}
