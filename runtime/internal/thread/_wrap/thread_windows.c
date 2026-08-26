/* Native Windows thread and fiber-local-storage support for LLGo.
 *
 * This file intentionally does not include Windows SDK headers. Keeping the
 * declarations local lets the compiler target the MSVC ABI while the SDK is
 * needed only by the final native link.
 */
typedef __SIZE_TYPE__ llgo_size_t;
typedef unsigned long llgo_dword;
typedef unsigned int llgo_uint;
typedef int llgo_bool;
typedef void *llgo_handle;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef llgo_dword(LLGO_WINAPI *llgo_win_thread_start)(void *arg);
typedef void(LLGO_WINAPI *llgo_win_fls_callback)(void *value);

__declspec(dllimport) llgo_handle LLGO_WINAPI CreateThread(
    void *attributes, llgo_size_t stack_size, llgo_win_thread_start start,
    void *arg, llgo_dword flags, llgo_dword *thread_id);
__declspec(dllimport) void LLGO_WINAPI ExitThread(llgo_dword exit_code);
__declspec(dllimport) llgo_bool LLGO_WINAPI CloseHandle(llgo_handle handle);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetLastError(void);
__declspec(dllimport) llgo_handle LLGO_WINAPI GetProcessHeap(void);
__declspec(dllimport) void *LLGO_WINAPI HeapAlloc(
    llgo_handle heap, llgo_dword flags, llgo_size_t bytes);
__declspec(dllimport) llgo_bool LLGO_WINAPI HeapFree(
    llgo_handle heap, llgo_dword flags, void *memory);
__declspec(dllimport) llgo_dword LLGO_WINAPI
FlsAlloc(llgo_win_fls_callback callback);
__declspec(dllimport) llgo_bool LLGO_WINAPI FlsFree(llgo_dword index);
__declspec(dllimport) void *LLGO_WINAPI FlsGetValue(llgo_dword index);
__declspec(dllimport) llgo_bool LLGO_WINAPI
FlsSetValue(llgo_dword index, void *value);

#if defined(LLGO_USE_BDWGC)
llgo_handle LLGO_WINAPI GC_CreateThread(
    void *attributes, llgo_size_t stack_size, llgo_win_thread_start start,
    void *arg, llgo_dword flags, llgo_dword *thread_id);
void LLGO_WINAPI GC_ExitThread(llgo_dword exit_code);
#endif

enum {
    llgo_error_not_enough_memory = 8,
    llgo_stack_size_is_a_reservation = 0x00010000UL,
};

#define LLGO_FLS_OUT_OF_INDEXES ((llgo_dword)0xffffffffUL)

/* ExitProcess runs FLS callbacks after terminating the other threads. Those
 * threads may have held a collector lock, so an FLS callback must not re-enter
 * Go or BDWGC once process shutdown begins. The process will reclaim the
 * deliberately retained sidecar allocation. */
static llgo_dword llgo_process_exiting;

void llgo_win_thread_begin_process_exit(void)
{
    __atomic_store_n(&llgo_process_exiting, 1, __ATOMIC_RELEASE);
}

typedef void *(*llgo_thread_routine)(void *arg);

typedef struct {
    llgo_thread_routine routine;
    void *arg;
} llgo_thread_start_data;

static llgo_dword LLGO_WINAPI llgo_thread_start(void *raw)
{
    llgo_thread_start_data data = *(llgo_thread_start_data *)raw;
    HeapFree(GetProcessHeap(), 0, raw);
    data.routine(data.arg);
    return 0;
}

int llgo_win_thread_create_detached(llgo_size_t stack_size,
                                    llgo_thread_routine routine, void *arg)
{
    llgo_thread_start_data *data;
    llgo_handle thread;
    llgo_dword flags = 0;
    llgo_dword error;

    if (routine == 0)
        return 87; /* ERROR_INVALID_PARAMETER */
    data = (llgo_thread_start_data *)HeapAlloc(
        GetProcessHeap(), 0, sizeof(*data));
    if (data == 0)
        return llgo_error_not_enough_memory;
    data->routine = routine;
    data->arg = arg;
    if (stack_size != 0)
        flags |= llgo_stack_size_is_a_reservation;
#if defined(LLGO_USE_BDWGC)
    thread = GC_CreateThread(0, stack_size, llgo_thread_start, data, flags, 0);
#else
    thread = CreateThread(0, stack_size, llgo_thread_start, data, flags, 0);
#endif
    if (thread == 0) {
        error = GetLastError();
        HeapFree(GetProcessHeap(), 0, data);
        return (int)error;
    }
    CloseHandle(thread);
    return 0;
}

void llgo_win_thread_exit(void)
{
#if defined(LLGO_USE_BDWGC)
    GC_ExitThread(0);
#else
    ExitThread(0);
#endif
}

typedef void (*llgo_key_destructor)(void *value);

typedef struct {
    llgo_key_destructor destructor;
    void *value;
} llgo_fls_value;

static void LLGO_WINAPI llgo_fls_destructor(void *raw)
{
    llgo_fls_value value;
    if (raw == 0 ||
        __atomic_load_n(&llgo_process_exiting, __ATOMIC_ACQUIRE))
        return;
    value = *(llgo_fls_value *)raw;
    if (value.destructor != 0 && value.value != 0)
        value.destructor(value.value);
    HeapFree(GetProcessHeap(), 0, raw);
}

int llgo_win_fls_create(llgo_uint *index)
{
    llgo_dword value;
    if (index == 0)
        return 87; /* ERROR_INVALID_PARAMETER */
    value = FlsAlloc(llgo_fls_destructor);
    if (value == LLGO_FLS_OUT_OF_INDEXES)
        return (int)GetLastError();
    *index = value;
    return 0;
}

int llgo_win_fls_delete(llgo_uint index)
{
    if (FlsFree(index))
        return 0;
    return (int)GetLastError();
}

void *llgo_win_fls_get(llgo_uint index)
{
    llgo_fls_value *slot = (llgo_fls_value *)FlsGetValue(index);
    return slot != 0 ? slot->value : 0;
}

int llgo_win_fls_set(llgo_uint index, llgo_key_destructor destructor,
                     void *value)
{
    llgo_fls_value *slot = (llgo_fls_value *)FlsGetValue(index);
    llgo_bool ok;
    llgo_dword error;

    if (value == 0) {
        if (slot == 0)
            return 0;
        ok = FlsSetValue(index, 0);
        if (!ok)
            return (int)GetLastError();
        HeapFree(GetProcessHeap(), 0, slot);
        return 0;
    }
    if (slot != 0) {
        /* Key.Set always supplies the destructor fixed by Key.Create. */
        slot->destructor = destructor;
        slot->value = value;
        return 0;
    }
    slot = (llgo_fls_value *)HeapAlloc(GetProcessHeap(), 0, sizeof(*slot));
    if (slot == 0)
        return llgo_error_not_enough_memory;
    slot->destructor = destructor;
    slot->value = value;
    ok = FlsSetValue(index, slot);
    if (ok)
        return 0;
    error = GetLastError();
    HeapFree(GetProcessHeap(), 0, slot);
    return (int)error;
}
