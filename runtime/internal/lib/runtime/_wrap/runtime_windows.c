/* Keep the runtime shim independent of Windows SDK headers. Clang still
 * applies the target's MSVC ABI and emits ordinary Kernel32 imports. */
#include <stdint.h>

typedef __SIZE_TYPE__ llgo_size_t;
typedef unsigned long llgo_dword;
typedef unsigned short llgo_word;
typedef __UINTPTR_TYPE__ llgo_uintptr;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef struct {
    union { llgo_dword oemid; struct { llgo_word arch; llgo_word reserved; }; };
    llgo_dword page_size;
    void *min_app_address;
    void *max_app_address;
    llgo_uintptr active_processor_mask;
    llgo_dword num_processors;
    llgo_dword processor_type;
    llgo_dword allocation_granularity;
    llgo_word processor_level;
    llgo_word processor_revision;
} llgo_system_info;

typedef struct {
    void *base_address;
    void *allocation_base;
    llgo_dword allocation_protect;
#if defined(_WIN64)
    llgo_word partition_id;
#endif
    llgo_size_t region_size;
    llgo_dword state;
    llgo_dword protect;
    llgo_dword type;
} llgo_memory_basic_information;

__declspec(dllimport) int LLGO_WINAPI
GetProcessAffinityMask(void *process, llgo_uintptr *process_mask,
                       llgo_uintptr *system_mask);
__declspec(dllimport) void LLGO_WINAPI GetSystemInfo(llgo_system_info *info);
__declspec(dllimport) llgo_size_t LLGO_WINAPI
VirtualQuery(const void *address, llgo_memory_basic_information *info,
             llgo_size_t length);
__declspec(dllimport) int LLGO_WINAPI
QueryPerformanceCounter(long long *counter);
__declspec(dllimport) int LLGO_WINAPI
QueryPerformanceFrequency(long long *frequency);

static long long llgo_nanotime_frequency;

typedef struct {
    llgo_dword low;
    llgo_dword high;
} llgo_filetime;

__declspec(dllimport) void LLGO_WINAPI
GetSystemTimePreciseAsFileTime(llgo_filetime *time);
__declspec(dllimport) void *LLGO_WINAPI
LoadLibraryExW(const llgo_word *filename, void *file, llgo_dword flags);
__declspec(dllimport) void *LLGO_WINAPI
GetProcAddress(void *module, const char *name);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetLastError(void);

#if defined(_WIN64)
/* RUNTIME_FUNCTION differs between AMD64 and ARM64; only its pointer is used. */
typedef struct llgo_runtime_function llgo_runtime_function;

__declspec(dllimport) void LLGO_WINAPI RtlCaptureContext(void *context);
__declspec(dllimport) llgo_runtime_function *LLGO_WINAPI
RtlLookupFunctionEntry(llgo_uintptr pc, llgo_uintptr *image_base,
                       void *history_table);
__declspec(dllimport) void *LLGO_WINAPI
RtlVirtualUnwind(llgo_dword handler_type, llgo_uintptr image_base,
                 llgo_uintptr pc, llgo_runtime_function *function_entry,
                 void *context, void **handler_data,
                 llgo_uintptr *establisher_frame, void *context_pointers);

__attribute__((noinline)) void *
llgo_windows_capture_context(void *context, llgo_size_t pc_offset)
{
    llgo_runtime_function *function_entry;
    llgo_uintptr image_base;
    llgo_uintptr pc;
    llgo_uintptr frame;
    void *handler_data;

    RtlCaptureContext(context);
    /* Unwind this wrapper while its captured stack frame is still live. */
    pc = *(llgo_uintptr *)((unsigned char *)context + pc_offset);
    function_entry = RtlLookupFunctionEntry(pc, &image_base, 0);
    if (function_entry == 0)
        return 0;
    /* HandlerData is a required output on ARM64, even for a no-handler walk. */
    RtlVirtualUnwind(0, image_base, pc, function_entry, context,
                     &handler_data, &frame, 0);
    return context;
}

llgo_runtime_function *
llgo_windows_lookup_function_entry(llgo_uintptr pc, llgo_uintptr *image_base)
{
    return RtlLookupFunctionEntry(pc, image_base, 0);
}

void *llgo_windows_virtual_unwind(llgo_uintptr image_base, llgo_uintptr pc,
                                  llgo_runtime_function *function_entry,
                                  void *context,
                                  llgo_uintptr *establisher_frame)
{
    void *handler_data;
    /* HandlerData is a required output on ARM64, even for a no-handler walk. */
    return RtlVirtualUnwind(0, image_base, pc, function_entry, context,
                            &handler_data, establisher_frame, 0);
}
#endif

enum {
    llgo_mem_commit = 0x1000,
    llgo_page_noaccess = 0x01,
    llgo_page_execute = 0x10,
    llgo_page_guard = 0x100,
};

int llgo_maxprocs(void)
{
    llgo_uintptr mask;
    llgo_uintptr system_mask;
    int count = 0;

    /* Match Go's getCPUCount: report CPUs available to this process, rather
     * than all active processors on the machine. */
    if (GetProcessAffinityMask((void *)(intptr_t)-1, &mask, &system_mask)) {
        while (mask != 0) {
            count += (int)(mask & 1);
            mask >>= 1;
        }
        if (count != 0)
            return count;
    }
    {
        llgo_system_info info;
        GetSystemInfo(&info);
        return info.num_processors == 0 ? 1 : (int)info.num_processors;
    }
}

int llgo_mem_readable(void *p)
{
    llgo_memory_basic_information info;
    llgo_dword protect;
    if (p == 0 || VirtualQuery(p, &info, sizeof(info)) == 0 ||
        info.state != llgo_mem_commit)
        return 0;
    protect = info.protect & 0xff;
    return protect != llgo_page_noaccess && protect != llgo_page_execute &&
           (info.protect & llgo_page_guard) == 0;
}

int llgo_nanotime_init(void)
{
    return QueryPerformanceFrequency(&llgo_nanotime_frequency) &&
           llgo_nanotime_frequency > 0;
}

long long llgo_nanotime(void)
{
    long long counter;
    long long seconds;
    long long remainder;
    if (!QueryPerformanceCounter(&counter) || llgo_nanotime_frequency <= 0)
        return 0;
    seconds = counter / llgo_nanotime_frequency;
    remainder = counter % llgo_nanotime_frequency;
    return seconds * 1000000000LL +
           remainder * 1000000000LL / llgo_nanotime_frequency;
}

void llgo_walltime(long long *seconds, long *nanoseconds)
{
    llgo_filetime now;
    unsigned long long ticks;
    GetSystemTimePreciseAsFileTime(&now);
    ticks = ((unsigned long long)now.high << 32) | now.low;
    ticks -= 116444736000000000ULL;
    *seconds = (long long)(ticks / 10000000ULL);
    *nanoseconds = (long)((ticks % 10000000ULL) * 100ULL);
}

llgo_uintptr llgo_load_library(const llgo_word *filename, llgo_dword flags,
                               llgo_dword *error)
{
    void *module = LoadLibraryExW(filename, 0, flags);
    *error = module == 0 ? GetLastError() : 0;
    return (llgo_uintptr)module;
}

llgo_uintptr llgo_get_proc_address(llgo_uintptr module,
                                   const unsigned char *name,
                                   llgo_dword *error)
{
    void *proc = GetProcAddress((void *)module, (const char *)name);
    *error = proc == 0 ? GetLastError() : 0;
    return (llgo_uintptr)proc;
}
