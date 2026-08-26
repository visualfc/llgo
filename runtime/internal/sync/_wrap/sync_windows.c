/* Native Windows synchronization for the hosted LLGo runtime.
 *
 * Keep this file independent of the Windows SDK headers. The declarations
 * below are the complete Kernel32 surface used here; clang still applies the
 * target's MSVC ABI and emits ordinary Kernel32 imports.
 */
typedef unsigned long llgo_dword;
typedef int llgo_bool;
typedef void *llgo_srwlock;
typedef void *llgo_condition_variable;
typedef void *llgo_init_once;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

__declspec(dllimport) void LLGO_WINAPI
AcquireSRWLockExclusive(llgo_srwlock *lock);
__declspec(dllimport) void LLGO_WINAPI
ReleaseSRWLockExclusive(llgo_srwlock *lock);

__declspec(dllimport) void LLGO_WINAPI
WakeConditionVariable(llgo_condition_variable *condition);
__declspec(dllimport) void LLGO_WINAPI
WakeAllConditionVariable(llgo_condition_variable *condition);
__declspec(dllimport) llgo_bool LLGO_WINAPI SleepConditionVariableSRW(
    llgo_condition_variable *condition, llgo_srwlock *lock,
    llgo_dword milliseconds, llgo_dword flags);

typedef llgo_bool(LLGO_WINAPI *llgo_init_once_fn)(
    llgo_init_once *once, void *parameter, void **context);
__declspec(dllimport) llgo_bool LLGO_WINAPI InitOnceExecuteOnce(
    llgo_init_once *once, llgo_init_once_fn callback, void *parameter,
    void **context);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetLastError(void);

#define LLGO_INFINITE ((llgo_dword)0xffffffffUL)

typedef struct {
    void *code;
    void *context;
} llgo_go_func;

extern void llgo_win_once_invoke(llgo_go_func *fn);

static llgo_bool LLGO_WINAPI llgo_once_callback(
    llgo_init_once *once, void *parameter, void **context)
{
    (void)once;
    (void)context;
    llgo_win_once_invoke((llgo_go_func *)parameter);
    return 1;
}

int llgo_win_once(llgo_init_once *once, llgo_go_func *fn)
{
    if (fn == 0 || fn->code == 0)
        return 87; /* ERROR_INVALID_PARAMETER */
    if (InitOnceExecuteOnce(once, llgo_once_callback, fn, 0))
        return 0;
    return (int)GetLastError();
}

void llgo_win_mutex_lock(llgo_srwlock *lock)
{
    AcquireSRWLockExclusive(lock);
}

void llgo_win_mutex_unlock(llgo_srwlock *lock)
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
