//go:build windows

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	clitetime "github.com/xgo-dev/llgo/runtime/internal/clite/time"
	"github.com/xgo-dev/llgo/runtime/internal/runtime/math"
)

const windowsRandIncrement = uint64(0xa0761d6478bd642f)

var windowsRandProcessSeed = newWindowsRandProcessSeed()

//go:linkname c_windowsRandom C.llgo_windows_random
func c_windowsRandom(data unsafe.Pointer, size uintptr) c.Int

func windowsRandom(data unsafe.Pointer, size uintptr) bool {
	return c_windowsRandom(data, size) != 0
}

func newWindowsRandProcessSeed() uint64 {
	var seed uint64
	if windowsRandom(unsafe.Pointer(&seed), unsafe.Sizeof(seed)) {
		return seed
	}
	// Match Go's availability-first startup behavior: OS entropy is the
	// primary source, but a process must still start if it is unavailable.
	return uint64(clitetime.Time(nil))
}

// fastrand returns random data from the current M, following the ownership
// model used by Go's runtime.rand. LLGo cannot use UCRT rand here: srand seeds
// only the calling Windows thread, while LLGo currently maps each goroutine to
// a separate native thread. New threads would therefore all start with UCRT's
// default seed and emit identical temporary-file names, map seeds, and other
// supposedly randomized runtime values. See Microsoft's srand documentation:
// https://learn.microsoft.com/cpp/c-runtime-library/reference/srand.
//
// The process seed comes from Windows' system-preferred CSPRNG. The M id then
// separates the streams created during one process. Mixing it with the seed,
// rather than merely offsetting one linear sequence, prevents adjacent M ids
// from producing the same sequence shifted by one call.
func fastrand() uint32 {
	mp := getg().m
	state := mp.os.randomState
	if state == 0 {
		state = mixWindowsRand(windowsRandProcessSeed ^ uint64(mp.id))
		if state == 0 {
			state = windowsRandIncrement
		}
	}
	state += windowsRandIncrement
	mp.os.randomState = state
	return uint32(mixWindowsRand(state))
}

// Fastrand exposes the core random source to the public runtime compatibility
// package. Compiler-lowered maps and channels call fastrand directly.
func Fastrand() uint32 {
	return fastrand()
}

func mixWindowsRand(state uint64) uint64 {
	hi, lo := math.Mul64(state, state^0xe7037ed1a0b428db)
	return hi ^ lo
}
