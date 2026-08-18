//go:build windows

package runtime

import (
	latomic "sync/atomic"
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	psync "github.com/xgo-dev/llgo/runtime/internal/clite/sync"
)

// The signal numbers accepted by os/signal are bounded by its numSig constant.
// Keeping the state in fixed bitsets mirrors Go's runtime signal queue and
// avoids allocation from a Windows console-control callback thread.
const windowsSignalCount = 65

type windowsSignalState struct {
	active  bool
	ignored bool
}

var (
	windowsSignalInitState uint32
	windowsSignalMu        psync.Mutex
	windowsSignalCond      psync.Cond
	windowsSignalStates    [windowsSignalCount]windowsSignalState
	windowsSignalPending   [(windowsSignalCount + 31) / 32]uint32
)

//go:linkname c_windowsSignalInit C.llgo_windows_signal_init
func c_windowsSignalInit() c.Int

func ensureSignalInit() {
	const (
		windowsSignalUninitialized uint32 = iota
		windowsSignalInitialized
		windowsSignalInitializing
	)
	for {
		switch state := latomic.LoadUint32(&windowsSignalInitState); state {
		case windowsSignalInitialized:
			return
		case windowsSignalUninitialized:
			if latomic.CompareAndSwapUint32(&windowsSignalInitState, windowsSignalUninitialized, windowsSignalInitializing) {
				windowsSignalMu.Init(nil)
				windowsSignalCond.Init(nil)
				if c_windowsSignalInit() == 0 {
					throw("SetConsoleCtrlHandler failed")
				}
				latomic.StoreUint32(&windowsSignalInitState, windowsSignalInitialized)
				return
			}
		}
		c.Usleep(1)
	}
}

//export llgo_runtime_windowsSignalCallback
func llgo_runtime_windowsSignalCallback(signum c.Uint) c.Int {
	sig := uint32(signum)
	if sig >= windowsSignalCount {
		return 0
	}
	windowsSignalMu.Lock()
	state := windowsSignalStates[sig]
	if !state.active || state.ignored {
		windowsSignalMu.Unlock()
		return 0
	}
	windowsSignalPending[sig/32] |= uint32(1) << (sig & 31)
	windowsSignalCond.Signal()
	windowsSignalMu.Unlock()
	return 1
}

func signal_enable(sig uint32) {
	ensureSignalInit()
	if sig >= windowsSignalCount {
		return
	}
	windowsSignalMu.Lock()
	windowsSignalStates[sig] = windowsSignalState{active: true}
	windowsSignalMu.Unlock()
}

func signal_disable(sig uint32) {
	ensureSignalInit()
	if sig >= windowsSignalCount {
		return
	}
	windowsSignalMu.Lock()
	state := &windowsSignalStates[sig]
	state.active = false
	windowsSignalMu.Unlock()
}

func signal_ignore(sig uint32) {
	ensureSignalInit()
	if sig >= windowsSignalCount {
		return
	}
	windowsSignalMu.Lock()
	windowsSignalStates[sig] = windowsSignalState{ignored: true}
	windowsSignalMu.Unlock()
}

func signal_ignored(sig uint32) bool {
	ensureSignalInit()
	if sig >= windowsSignalCount {
		return false
	}
	windowsSignalMu.Lock()
	ignored := windowsSignalStates[sig].ignored
	windowsSignalMu.Unlock()
	return ignored
}

func signal_recv() uint32 {
	ensureSignalInit()
	windowsSignalMu.Lock()
	for {
		for sig := uint32(0); sig < windowsSignalCount; sig++ {
			word := &windowsSignalPending[sig/32]
			bit := uint32(1) << (sig & 31)
			if *word&bit != 0 {
				*word &^= bit
				windowsSignalMu.Unlock()
				return sig
			}
		}
		windowsSignalCond.Wait(&windowsSignalMu)
	}
}

func signalWaitUntilIdle() {
	ensureSignalInit()
	for {
		windowsSignalMu.Lock()
		pending := false
		for _, word := range windowsSignalPending {
			pending = pending || word != 0
		}
		windowsSignalMu.Unlock()
		if !pending {
			return
		}
		c.Usleep(1)
	}
}
