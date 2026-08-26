typedef __SIZE_TYPE__ llgo_size_t;
typedef __UINTPTR_TYPE__ llgo_uintptr;
typedef unsigned long llgo_dword;
typedef int llgo_bool;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef llgo_dword(LLGO_WINAPI *llgo_thread_start)(void *parameter);
typedef llgo_uintptr (*llgo_callback)(llgo_uintptr argument);
typedef llgo_uintptr(LLGO_WINAPI *llgo_stdcall_callback)(llgo_uintptr argument);
typedef struct {
    unsigned int low;
    unsigned int high;
} llgo_callback_pair;
typedef llgo_uintptr (*llgo_pair_callback)(llgo_callback_pair value);
typedef llgo_uintptr (*llgo_no_arg_callback)(void);

__declspec(dllimport) void *LLGO_WINAPI
CreateThread(void *attributes, llgo_size_t stack_size,
             llgo_thread_start start, void *parameter,
             llgo_dword flags, llgo_dword *thread_id);
__declspec(dllimport) llgo_dword LLGO_WINAPI
WaitForSingleObject(void *handle, llgo_dword milliseconds);
__declspec(dllimport) llgo_bool LLGO_WINAPI CloseHandle(void *handle);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetLastError(void);

typedef struct {
    union {
        llgo_callback cdecl;
        llgo_stdcall_callback stdcall;
    } callback;
    llgo_uintptr argument;
    llgo_uintptr result;
    llgo_dword repeats;
    int cleanstack;
} llgo_callback_context;

static llgo_dword LLGO_WINAPI llgo_foreign_thread_start(void *parameter)
{
    llgo_callback_context *context = (llgo_callback_context *)parameter;
    llgo_dword i;
    for (i = 0; i < context->repeats; ++i) {
        llgo_uintptr argument = context->argument + i;
        if (context->cleanstack)
            context->result = context->callback.stdcall(argument);
        else
            context->result = context->callback.cdecl(argument);
    }
    return 0;
}

static int llgo_windows_run_foreign_callback(llgo_callback_context *context,
                                             llgo_uintptr *result)
{
    const llgo_dword infinite = 0xffffffffUL;
    void *thread = CreateThread(0, 0, llgo_foreign_thread_start,
                                context, 0, 0);
    llgo_dword error;
    if (thread == 0)
        return (int)GetLastError();
    if (WaitForSingleObject(thread, infinite) != 0) {
        error = GetLastError();
        CloseHandle(thread);
        return (int)error;
    }
    CloseHandle(thread);
    *result = context->result;
    return 0;
}

int llgo_windows_call_foreign_thread(llgo_callback callback,
                                     llgo_uintptr argument,
                                     llgo_uintptr *result)
{
    llgo_callback_context context = {{callback}, argument, 0, 1, 0};
    return llgo_windows_run_foreign_callback(&context, result);
}

int llgo_windows_call_foreign_thread_stdcall(llgo_stdcall_callback callback,
                                             llgo_uintptr argument,
                                             llgo_uintptr *result)
{
    llgo_callback_context context = {{0}, argument, 0, 1, 1};
    context.callback.stdcall = callback;
    return llgo_windows_run_foreign_callback(&context, result);
}

int llgo_windows_call_foreign_thread_cdecl(llgo_callback callback,
                                           llgo_uintptr argument,
                                           llgo_uintptr *result)
{
    llgo_callback_context context = {{callback}, argument, 0, 1, 0};
    return llgo_windows_run_foreign_callback(&context, result);
}

int llgo_windows_repeat_foreign_thread_cdecl(llgo_callback callback,
                                             llgo_uintptr argument,
                                             llgo_dword repeats,
                                             llgo_uintptr *result)
{
    llgo_callback_context context = {{callback}, argument, 0, repeats, 0};
    return llgo_windows_run_foreign_callback(&context, result);
}

llgo_uintptr llgo_windows_call_pair_callback(llgo_pair_callback callback,
                                             llgo_callback_pair value)
{
    return callback(value);
}

llgo_uintptr llgo_windows_call_no_arg_callback(llgo_no_arg_callback callback)
{
    return callback();
}
