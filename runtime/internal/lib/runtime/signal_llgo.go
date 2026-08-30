//go:build !baremetal && !wasm && !windows

package runtime

import (
	latomic "sync/atomic"
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	psync "github.com/xgo-dev/llgo/runtime/internal/sync"
)

// Unix signal handlers write signal numbers to a nonblocking self-pipe. A
// dedicated runtime goroutine drains that pipe and enters Go code in a normal
// execution context; the async-signal-safe C handler never calls into Go.

type sigState struct {
	active  bool
	ignored bool
}

var (
	sigInitState uint32
	sigReadFD    c.Int

	sigMu     psync.Mutex
	sigCond   psync.Cond
	sigWaitMu psync.Mutex
	sigQueue  []uint32
	sigStates map[uint32]*sigState

	// sigReceiving is true only while the os/signal receive goroutine is
	// blocked here waiting for another signal. Reaching that state after a
	// pipe barrier proves that it has processed every earlier queue entry.
	sigReceiving  bool
	sigBarrierAck uint64
)

//go:linkname c_signalInit C.llgo_signal_init
func c_signalInit() c.Int

//go:linkname c_signalEnable C.llgo_signal_enable
func c_signalEnable(sig c.Int) c.Int

//go:linkname c_signalDisable C.llgo_signal_disable
func c_signalDisable(sig c.Int) c.Int

//go:linkname c_signalIgnore C.llgo_signal_ignore
func c_signalIgnore(sig c.Int) c.Int

//go:linkname c_signalBarrier C.llgo_signal_barrier
func c_signalBarrier() c.Int

//go:linkname c_signalRecv C.llgo_signal_recv
func c_signalRecv(fd c.Int, sig *c.Int) c.Int

func ensureSignalInit() {
	const (
		sigInitUninit uint32 = iota
		sigInitDone
		sigInitBusy
	)
	for {
		switch state := latomic.LoadUint32(&sigInitState); state {
		case sigInitDone:
			return
		case sigInitUninit:
			if latomic.CompareAndSwapUint32(&sigInitState, sigInitUninit, sigInitBusy) {
				sigMu.Init(nil)
				sigCond.Init(nil)
				sigWaitMu.Init(nil)
				sigStates = make(map[uint32]*sigState)
				sigReadFD = c_signalInit()
				if sigReadFD < 0 {
					panic("runtime: failed to initialize signal pipe")
				}
				go signalReadLoop()
				latomic.StoreUint32(&sigInitState, sigInitDone)
				return
			}
		}
		c.Usleep(1)
	}
}

func signalReadLoop() {
	for {
		var value c.Int
		if c_signalRecv(sigReadFD, &value) != 0 {
			panic("runtime: failed to read signal pipe")
		}
		if value == 0 {
			sigMu.Lock()
			sigBarrierAck++
			sigCond.Broadcast()
			sigMu.Unlock()
			continue
		}
		signalCallback(uint32(value))
	}
}

func signalCallback(sig uint32) {
	sigMu.Lock()
	// Every positive pipe entry was written by a handler that was active at
	// the instant the signal arrived. Preserve it even if Reset or Stop has
	// since changed sigStates; os/signal keeps stopping channels registered
	// until signalWaitUntilIdle observes the barrier below.
	sigQueue = append(sigQueue, sig)
	sigCond.Broadcast()
	sigMu.Unlock()
}

func ensureSignalState(sig uint32) *sigState {
	st := sigStates[sig]
	if st == nil {
		st = &sigState{}
		sigStates[sig] = st
	}
	return st
}

func setSignalHandler(sig uint32, enabled bool) c.Int {
	locked := cpuProfileSignalLock(sig)
	var code c.Int
	if enabled {
		code = c_signalEnable(c.Int(sig))
	} else {
		code = c_signalDisable(c.Int(sig))
	}
	cpuProfileSignalUnlock(locked)
	return code
}

func setSignalIgnored(sig uint32) c.Int {
	locked := cpuProfileSignalLock(sig)
	code := c_signalIgnore(c.Int(sig))
	cpuProfileSignalUnlock(locked)
	return code
}

// signal_enable enables Go signal delivery for sig.
func signal_enable(sig uint32) {
	ensureSignalInit()
	sigMu.Lock()
	st := ensureSignalState(sig)
	if (!st.active || st.ignored) && setSignalHandler(sig, true) != 0 {
		sigMu.Unlock()
		panic("runtime: failed to install signal handler")
	}
	st.active = true
	st.ignored = false
	sigMu.Unlock()
}

// signal_disable disables Go signal delivery for sig.
func signal_disable(sig uint32) {
	ensureSignalInit()
	sigMu.Lock()
	st := sigStates[sig]
	if st == nil || !st.active {
		sigMu.Unlock()
		return
	}
	st.active = false
	st.ignored = false
	if setSignalHandler(sig, false) != 0 {
		sigMu.Unlock()
		panic("runtime: failed to restore signal handler")
	}
	sigMu.Unlock()
}

// signal_ignore causes sig to be ignored (do not deliver to Go).
func signal_ignore(sig uint32) {
	ensureSignalInit()
	sigMu.Lock()
	st := ensureSignalState(sig)
	if (!st.active || !st.ignored) && setSignalIgnored(sig) != 0 {
		sigMu.Unlock()
		panic("runtime: failed to install ignored signal handler")
	}
	st.active = true
	st.ignored = true
	sigMu.Unlock()
}

// signal_ignored reports whether sig is being ignored.
func signal_ignored(sig uint32) bool {
	ensureSignalInit()
	sigMu.Lock()
	st := sigStates[sig]
	ignored := st != nil && st.ignored
	sigMu.Unlock()
	return ignored
}

// signal_recv receives the next queued signal.
func signal_recv() uint32 {
	ensureSignalInit()
	sigMu.Lock()
	for len(sigQueue) == 0 {
		sigReceiving = true
		sigCond.Broadcast()
		sigCond.Wait(&sigMu)
	}
	sigReceiving = false
	sig := sigQueue[0]
	sigQueue = sigQueue[1:]
	sigMu.Unlock()
	return sig
}

func signalWaitUntilIdle() {
	ensureSignalInit()

	// os/signal documents this as single-caller runtime machinery, but keep
	// the LLGo implementation serialized so concurrent Stop calls cannot
	// mistake another caller's pipe marker for their own.
	sigWaitMu.Lock()
	defer sigWaitMu.Unlock()

	sigMu.Lock()
	wantAck := sigBarrierAck + 1
	sigMu.Unlock()
	if c_signalBarrier() != 0 {
		panic("runtime: failed to synchronize signal pipe")
	}

	sigMu.Lock()
	for sigBarrierAck < wantAck || len(sigQueue) != 0 || !sigReceiving {
		sigCond.Wait(&sigMu)
	}
	sigMu.Unlock()
}
