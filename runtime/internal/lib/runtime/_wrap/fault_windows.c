/* Windows vectored exception bridge for recoverable Go hardware faults.
 * Keep the declarations local so the runtime shim remains cross-compilable
 * with an MSVC target even when Windows SDK headers are not on the host. */
#include <stdint.h>

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef long llgo_long;
typedef unsigned long llgo_dword;

typedef struct llgo_exception_record {
    llgo_dword code;
    llgo_dword flags;
    struct llgo_exception_record *record;
    void *address;
    llgo_dword parameter_count;
    uintptr_t information[15];
} llgo_exception_record;

typedef struct {
    llgo_exception_record *record;
    void *context;
} llgo_exception_pointers;

typedef llgo_long(LLGO_WINAPI *llgo_vectored_handler)(
    llgo_exception_pointers *exception);
typedef void (*llgo_fault_callback)(void *context, int signal);

__declspec(dllimport) void *LLGO_WINAPI AddVectoredExceptionHandler(
    llgo_dword first, llgo_vectored_handler handler);

enum {
    llgo_exception_access_violation = 0xc0000005UL,
    llgo_exception_in_page_error = 0xc0000006UL,
    llgo_exception_int_divide_by_zero = 0xc0000094UL,
    llgo_exception_continue_search = 0,
    llgo_sigfpe = 8,
    llgo_sigsegv = 11,
};

static llgo_fault_callback llgo_fault_go;
static _Thread_local int llgo_in_fault;

static llgo_long LLGO_WINAPI
llgo_fault_handler(llgo_exception_pointers *exception)
{
    llgo_exception_record *record;
    int signal;

    if (exception == 0 || exception->record == 0 ||
        exception->context == 0 || llgo_fault_go == 0 || llgo_in_fault)
        return llgo_exception_continue_search;
    record = exception->record;
    switch (record->code) {
    case llgo_exception_access_violation:
    case llgo_exception_in_page_error:
        /* Match the Go runtime's recoverable nil-fault boundary. Accesses
         * elsewhere remain genuine process faults unless a later runtime
         * implementation explicitly enables panic-on-fault semantics. */
        if (record->parameter_count < 2 || record->information[1] >= 0x1000)
            return llgo_exception_continue_search;
        signal = llgo_sigsegv;
        break;
    case llgo_exception_int_divide_by_zero:
        signal = llgo_sigfpe;
        break;
    default:
        return llgo_exception_continue_search;
    }

    llgo_in_fault = 1;
    /* The callback converts the exception to LLGo's normal panic path and
     * longjmps out. It returns only if that invariant is broken. */
    llgo_fault_go(exception->context, signal);
    llgo_in_fault = 0;
    return llgo_exception_continue_search;
}

int llgo_install_windows_fault_handler(llgo_fault_callback callback)
{
    llgo_fault_go = callback;
    return AddVectoredExceptionHandler(1, llgo_fault_handler) != 0;
}

void llgo_windows_fault_capture_done(void)
{
    /* The Go callback calls this before entering the non-local panic path. */
    llgo_in_fault = 0;
}
