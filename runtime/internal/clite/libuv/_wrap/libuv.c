#if defined(_WIN32)
/* MinGW's SDK headers assume a MinGW target and cannot be included while
 * LLGo is deliberately compiling for the MSVC ABI. These are opaque libuv
 * types and the complete public function surface used by this shim. */
typedef unsigned long long uint64_t;
typedef struct uv_loop_s uv_loop_t;
typedef struct uv_async_s uv_async_t;
typedef struct uv_timer_s uv_timer_t;
typedef struct uv_signal_s uv_signal_t;
typedef struct uv_tcp_s uv_tcp_t;
typedef void (*uv_async_cb)(uv_async_t *handle);
typedef void (*uv_timer_cb)(uv_timer_t *handle);
typedef void (*uv_signal_cb)(uv_signal_t *handle, int signum);

int uv_async_init(uv_loop_t *loop, uv_async_t *async, uv_async_cb cb);
int uv_timer_start(uv_timer_t *timer, uv_timer_cb cb, uint64_t timeout,
                   uint64_t repeat);
int uv_signal_start(uv_signal_t *handle, uv_signal_cb cb, int signum);
int uv_signal_start_oneshot(uv_signal_t *handle, uv_signal_cb cb, int signum);
#else
#include <stdint.h>
#include <uv.h>
#endif

extern void llgo_runtime_timerEvent(uv_async_t* handle);
extern void llgo_runtime_timerCallback(uv_timer_t* handle);
extern void llgo_runtime_signalCallback(uv_signal_t* handle, int signum);

static void llgo_uv_async_noop(uv_async_t* handle) {
  (void)handle;
}

int llgo_uv_async_init_noop(uv_loop_t* loop, uv_async_t* async) {
  return uv_async_init(loop, async, llgo_uv_async_noop);
}

int llgo_uv_async_init_runtime(uv_loop_t* loop, uv_async_t* async) {
  return uv_async_init(loop, async, llgo_runtime_timerEvent);
}

int llgo_uv_timer_start_runtime(uv_timer_t* timer, uint64_t timeout, uint64_t repeat) {
  return uv_timer_start(timer, llgo_runtime_timerCallback, timeout, repeat);
}

int llgo_uv_signal_start_runtime(uv_signal_t* handle, int signum) {
  return uv_signal_start(handle, llgo_runtime_signalCallback, signum);
}

int llgo_uv_signal_start_oneshot_runtime(uv_signal_t* handle, int signum) {
  return uv_signal_start_oneshot(handle, llgo_runtime_signalCallback, signum);
}

int uv_tcp_get_io_watcher_fd (uv_tcp_t* handle) {
#if defined(_WIN32)
  (void)handle;
  return -1;
#else
  return handle->io_watcher.fd;
#endif
}
