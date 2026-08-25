#define _CRT_SECURE_NO_WARNINGS
#include <stdint.h>
#include <windows.h>

extern char *getenv(const char *name);

uintptr_t llgo_windows_invalid_address(void)
{
    return 1;
}

int llgo_windows_unrecovered_fault(void)
{
    return getenv("LLGO_TEST_UNRECOVERED_FAULT") != 0;
}

static LONG llgo_foreign_fault_count;

static LONG CALLBACK
llgo_continue_foreign_fault(EXCEPTION_POINTERS *exception)
{
    EXCEPTION_RECORD *record = exception->ExceptionRecord;
    if (record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION &&
        record->NumberParameters >= 2 && record->ExceptionInformation[1] == 0) {
        InterlockedIncrement(&llgo_foreign_fault_count);
        return EXCEPTION_CONTINUE_EXECUTION;
    }
    return EXCEPTION_CONTINUE_SEARCH;
}

int llgo_windows_foreign_fault_on_go_thread(void)
{
    ULONG_PTR information[2] = {0, 0};
    PVOID handler = AddVectoredExceptionHandler(0, llgo_continue_foreign_fault);
    if (handler == 0)
        return -1;
    llgo_foreign_fault_count = 0;
    RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 2, information);
    RemoveVectoredExceptionHandler(handler);
    return (int)llgo_foreign_fault_count;
}

static DWORD WINAPI
llgo_raise_foreign_fault(void *unused)
{
    ULONG_PTR information[2] = {0, 0};
    (void)unused;
    RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 2, information);
    return 0;
}

int llgo_windows_foreign_fault_on_native_thread(void)
{
    DWORD exit_code;
    HANDLE thread;
    int result = -1;
    PVOID handler = AddVectoredExceptionHandler(0, llgo_continue_foreign_fault);
    if (handler == 0)
        return -1;
    llgo_foreign_fault_count = 0;
    thread = CreateThread(0, 0, llgo_raise_foreign_fault, 0, 0, 0);
    if (thread != 0) {
        if (WaitForSingleObject(thread, INFINITE) == WAIT_OBJECT_0 &&
            GetExitCodeThread(thread, &exit_code) && exit_code == 0)
            result = (int)llgo_foreign_fault_count;
        CloseHandle(thread);
    }
    RemoveVectoredExceptionHandler(handler);
    return result;
}
