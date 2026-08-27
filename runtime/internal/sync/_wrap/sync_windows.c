/* Native Windows synchronization for the hosted LLGo runtime.
 *
 * Keep this file independent of the Windows SDK headers. The declarations
 * below are the complete Kernel32 surface used here; clang still applies the
 * target's MSVC ABI and emits ordinary Kernel32 imports.
 */
typedef unsigned long llgo_dword;
typedef long long llgo_time_t;
typedef int llgo_bool;
typedef void *llgo_srwlock;
typedef void *llgo_condition_variable;
typedef void *llgo_init_once;

#if defined(_WIN64)
typedef unsigned long long llgo_size_t;
#else
typedef unsigned int llgo_size_t;
#endif

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

__declspec(dllimport) void LLGO_WINAPI
AcquireSRWLockExclusive(llgo_srwlock *lock);
__declspec(dllimport) void LLGO_WINAPI
AcquireSRWLockShared(llgo_srwlock *lock);
__declspec(dllimport) void LLGO_WINAPI
ReleaseSRWLockExclusive(llgo_srwlock *lock);
__declspec(dllimport) void LLGO_WINAPI
ReleaseSRWLockShared(llgo_srwlock *lock);
__declspec(dllimport) unsigned char LLGO_WINAPI
TryAcquireSRWLockExclusive(llgo_srwlock *lock);
__declspec(dllimport) unsigned char LLGO_WINAPI
TryAcquireSRWLockShared(llgo_srwlock *lock);

__declspec(dllimport) void LLGO_WINAPI
WakeConditionVariable(llgo_condition_variable *condition);
__declspec(dllimport) void LLGO_WINAPI
WakeAllConditionVariable(llgo_condition_variable *condition);
__declspec(dllimport) llgo_bool LLGO_WINAPI SleepConditionVariableSRW(
    llgo_condition_variable *condition, llgo_srwlock *lock,
    llgo_dword milliseconds, llgo_dword flags);
__declspec(dllimport) llgo_bool LLGO_WINAPI WaitOnAddress(
    volatile void *address, void *compare_address,
    llgo_size_t address_size, llgo_dword milliseconds);
__declspec(dllimport) void LLGO_WINAPI WakeByAddressSingle(void *address);

typedef struct {
    llgo_dword low;
    llgo_dword high;
} llgo_filetime;

__declspec(dllimport) void LLGO_WINAPI
GetSystemTimeAsFileTime(llgo_filetime *time);

typedef llgo_bool(LLGO_WINAPI *llgo_init_once_fn)(
    llgo_init_once *once, void *parameter, void **context);
__declspec(dllimport) llgo_bool LLGO_WINAPI InitOnceExecuteOnce(
    llgo_init_once *once, llgo_init_once_fn callback, void *parameter,
    void **context);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetLastError(void);

#define LLGO_INFINITE ((llgo_dword)0xffffffffUL)

enum {
    llgo_error_busy = 16,
    llgo_error_invalid_parameter = 22,
    llgo_error_timeout = 1460,
    llgo_timedout = 110,
};

typedef void (*llgo_once_fn)(void);
typedef void (*llgo_once_context_fn)(void *);

typedef struct {
    llgo_once_context_fn callback;
    void *data;
} llgo_once_call;

typedef struct {
    llgo_once_fn fn;
} llgo_once_raw_call;

static llgo_bool LLGO_WINAPI llgo_once_callback(
    llgo_init_once *once, void *parameter, void **context)
{
    llgo_once_call *call = (llgo_once_call *)parameter;
    (void)once;
    (void)context;
    call->callback(call->data);
    return 1;
}

static int llgo_win_once_execute(llgo_init_once *once,
                                 llgo_once_context_fn callback, void *data)
{
    llgo_once_call call;
    if (callback == 0)
        return 87; /* ERROR_INVALID_PARAMETER */
    call.callback = callback;
    call.data = data;
    if (InitOnceExecuteOnce(once, llgo_once_callback, &call, 0))
        return 0;
    return (int)GetLastError();
}

static void llgo_win_once_invoke_raw(void *data)
{
    ((llgo_once_raw_call *)data)->fn();
}

int llgo_win_once(llgo_init_once *once, llgo_once_fn fn)
{
    llgo_once_raw_call call;
    if (fn == 0)
        return 87; /* ERROR_INVALID_PARAMETER */
    call.fn = fn;
    return llgo_win_once_execute(once, llgo_win_once_invoke_raw, &call);
}

int llgo_win_once_context(llgo_init_once *once,
                          llgo_once_context_fn callback, void *data)
{
    return llgo_win_once_execute(once, callback, data);
}

void llgo_win_mutex_lock(llgo_srwlock *lock)
{
    AcquireSRWLockExclusive(lock);
}

void llgo_win_mutex_unlock(llgo_srwlock *lock)
{
    ReleaseSRWLockExclusive(lock);
}

/*
 * github.com/goplus/lib/c/pthread/sync uses the same SRWLOCK layout as the
 * runtime. Keep its extended mutex and rwlock entry points in this object so
 * an LLGo process has one native synchronization backend instead of two.
 */
int llgo_win_mutex_trylock(llgo_srwlock *lock)
{
    return TryAcquireSRWLockExclusive(lock) ? 0 : llgo_error_busy;
}

void llgo_win_rwlock_rlock(llgo_srwlock *lock)
{
    AcquireSRWLockShared(lock);
}

int llgo_win_rwlock_tryrlock(llgo_srwlock *lock)
{
    return TryAcquireSRWLockShared(lock) ? 0 : llgo_error_busy;
}

void llgo_win_rwlock_runlock(llgo_srwlock *lock)
{
    ReleaseSRWLockShared(lock);
}

void llgo_win_rwlock_lock(llgo_srwlock *lock)
{
    AcquireSRWLockExclusive(lock);
}

int llgo_win_rwlock_trylock(llgo_srwlock *lock)
{
    return TryAcquireSRWLockExclusive(lock) ? 0 : llgo_error_busy;
}

void llgo_win_rwlock_unlock(llgo_srwlock *lock)
{
    ReleaseSRWLockExclusive(lock);
}

int llgo_win_cond_signal(llgo_condition_variable *condition)
{
    WakeConditionVariable(condition);
    return 0;
}

int llgo_win_cond_broadcast(llgo_condition_variable *condition)
{
    WakeAllConditionVariable(condition);
    return 0;
}

int llgo_win_cond_wait(llgo_condition_variable *condition,
                       llgo_srwlock *lock)
{
    if (SleepConditionVariableSRW(condition, lock, LLGO_INFINITE, 0))
        return 0;
    return (int)GetLastError();
}

typedef struct {
    llgo_time_t sec;
    long nsec;
} llgo_timespec;

static unsigned long long llgo_unix_time_100ns(void)
{
    llgo_filetime now;
    unsigned long long ticks;
    GetSystemTimeAsFileTime(&now);
    ticks = ((unsigned long long)now.high << 32) | now.low;
    /* Number of 100ns intervals from 1601-01-01 to 1970-01-01. */
    return ticks - 116444736000000000ULL;
}

int llgo_win_cond_timedwait(llgo_condition_variable *condition,
                            llgo_srwlock *lock,
                            const llgo_timespec *abstime)
{
    unsigned long long deadline;
    unsigned long long now;
    unsigned long long remaining;
    llgo_dword milliseconds;
    llgo_dword error;

    if (abstime == 0 || abstime->sec < 0 || abstime->nsec < 0 ||
        abstime->nsec >= 1000000000L)
        return llgo_error_invalid_parameter;
    deadline = (unsigned long long)abstime->sec * 10000000ULL
             + (unsigned long long)abstime->nsec / 100ULL;
    now = llgo_unix_time_100ns();
    if (deadline <= now)
        return llgo_timedout;
    remaining = deadline - now;
    /* Round up so a sub-millisecond remainder cannot time out early. */
    remaining = (remaining + 9999ULL) / 10000ULL;
    milliseconds = remaining >= LLGO_INFINITE
                 ? LLGO_INFINITE - 1
                 : (llgo_dword)remaining;
    if (SleepConditionVariableSRW(condition, lock, milliseconds, 0))
        return 0;
    error = GetLastError();
    return error == llgo_error_timeout ? llgo_timedout : (int)error;
}

int llgo_win_wait_uint32(volatile unsigned int *address,
                         unsigned int value)
{
    if (WaitOnAddress(address, &value, sizeof(value), LLGO_INFINITE))
        return 0;
    return (int)GetLastError();
}

void llgo_win_wake_uint32(unsigned int *address)
{
    WakeByAddressSingle(address);
}
