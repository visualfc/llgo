/* Minimal Win32 probe used by the native runtime smoke test. */
typedef unsigned long llgo_dword;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

__declspec(dllimport) llgo_dword LLGO_WINAPI GetCurrentThreadId(void);

llgo_dword llgo_windows_current_thread_id(void)
{
    return GetCurrentThreadId();
}
