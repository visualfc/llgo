#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#if defined(__EMSCRIPTEN__)
#include <emscripten.h>
#elif defined(__wasi__)
#include <poll.h>
#else
#error "unsupported WebAssembly host"
#endif

#define LLGO_NANOSECONDS_PER_MILLISECOND UINT64_C(1000000)

#if defined(__EMSCRIPTEN__)
EM_JS(void, llgo_wasm_host_wait_async, (double milliseconds), {
	Asyncify.handleSleep(function(resolve) {
		const state = Module['llgoWasmHostWait'] || (Module['llgoWasmHostWait'] = {});
		let finished = false;
		const finish = function() {
			if (finished) {
				return;
			}
			finished = true;
			if (state.timer !== undefined) {
				clearTimeout(state.timer);
				delete state.timer;
			}
			if (state.wake === finish) {
				delete state.wake;
			}
			resolve();
		};
		state.wake = finish;
		state.timer = setTimeout(finish, milliseconds);
	});
});

EM_JS(void, llgo_wasm_host_wake, (void), {
	const state = Module['llgoWasmHostWait'];
	if (state === undefined || state.wake === undefined) {
		return;
	}
	const wake = state.wake;
	delete state.wake;
	setTimeout(wake, 0);
});
#else
void llgo_wasm_host_wake(void) {}
#endif

void llgo_wasm_host_wait(uint64_t nanoseconds) {
	uint64_t milliseconds = nanoseconds / LLGO_NANOSECONDS_PER_MILLISECOND;
	if (nanoseconds % LLGO_NANOSECONDS_PER_MILLISECOND != 0) {
		milliseconds++;
	}
#if defined(__EMSCRIPTEN__)
	if (milliseconds > UINT32_MAX) {
		milliseconds = UINT32_MAX;
	}
	/*
	 * Yield through Asyncify until either the deadline expires or a host
	 * callback publishes runnable work. The wake is deferred to a microtask so
	 * the callback's thin wasm bridge returns before the suspended Go context is
	 * resumed.
	 */
	llgo_wasm_host_wait_async((double)milliseconds);
#else
	if (milliseconds > INT_MAX) {
		milliseconds = INT_MAX;
	}
	(void)poll(NULL, 0, (int)milliseconds);
#endif
}
