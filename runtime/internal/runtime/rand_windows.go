//go:build windows

package runtime

import (
	clitetime "github.com/xgo-dev/llgo/runtime/internal/clite/time"
	"github.com/xgo-dev/llgo/runtime/internal/runtime/math"
)

const windowsRandIncrement = uint64(0xa0761d6478bd642f)

var windowsRandProcessSeed = uint64(clitetime.Time(nil))

// Fastrand returns random data from the current M, following the ownership
// model used by Go's runtime.rand. LLGo cannot use UCRT rand here: srand seeds
// only the calling Windows thread, while LLGo currently maps each goroutine to
// a separate native thread. New threads would therefore all start with UCRT's
// default seed and emit identical temporary-file names, map seeds, and other
// supposedly randomized runtime values. See Microsoft's srand documentation:
// https://learn.microsoft.com/cpp/c-runtime-library/reference/srand.
//
// The M id separates the streams created during one process. Mixing it with a
// process seed, rather than merely offsetting one linear sequence, prevents
// adjacent M ids from producing the same sequence shifted by one call.
func Fastrand() uint32 {
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

func mixWindowsRand(state uint64) uint64 {
	hi, lo := math.Mul64(state, state^0xe7037ed1a0b428db)
	return hi ^ lo
}
