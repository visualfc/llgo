/* CPU profiling for native Windows LLGo executables.
 *
 * This follows the Go runtime's Windows model: a dedicated native thread
 * periodically suspends the other process threads, snapshots their CONTEXT,
 * and resumes them immediately. Sample collection stays entirely in C so it
 * does not enter Go or allocate GC-managed memory from the profiler thread.
 *
 * Keep the declarations independent of Windows SDK headers, like the rest of
 * the Windows runtime shim, so cross compilation only needs the import libs. */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef unsigned long llgo_dword;
typedef long llgo_long;
typedef int llgo_bool;
typedef void *llgo_handle;
typedef __SIZE_TYPE__ llgo_size_t;
typedef __UINTPTR_TYPE__ llgo_uintptr;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef llgo_dword(LLGO_WINAPI *llgo_thread_start)(void *arg);

typedef struct {
    llgo_dword size;
    llgo_dword usage;
    llgo_dword thread_id;
    llgo_dword owner_process_id;
    llgo_long base_priority;
    llgo_long priority_delta;
    llgo_dword flags;
} llgo_thread_entry;

__declspec(dllimport) llgo_handle LLGO_WINAPI
CreateToolhelp32Snapshot(llgo_dword flags, llgo_dword process_id);
__declspec(dllimport) llgo_bool LLGO_WINAPI
Thread32First(llgo_handle snapshot, llgo_thread_entry *entry);
__declspec(dllimport) llgo_bool LLGO_WINAPI
Thread32Next(llgo_handle snapshot, llgo_thread_entry *entry);
__declspec(dllimport) llgo_handle LLGO_WINAPI OpenThread(llgo_dword access,
                                                         llgo_bool inherit,
                                                         llgo_dword thread_id);
__declspec(dllimport) llgo_dword LLGO_WINAPI SuspendThread(llgo_handle thread);
__declspec(dllimport) llgo_dword LLGO_WINAPI ResumeThread(llgo_handle thread);
__declspec(dllimport) llgo_bool LLGO_WINAPI GetThreadContext(llgo_handle thread,
                                                             void *context);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetCurrentProcessId(void);
__declspec(dllimport) llgo_dword LLGO_WINAPI GetCurrentThreadId(void);
__declspec(dllimport) llgo_handle LLGO_WINAPI GetCurrentProcess(void);
__declspec(dllimport) llgo_handle LLGO_WINAPI GetCurrentThread(void);
__declspec(dllimport) llgo_bool LLGO_WINAPI
ReadProcessMemory(llgo_handle process, const void *address, void *buffer,
                  llgo_size_t size, llgo_size_t *read);
__declspec(dllimport) llgo_handle LLGO_WINAPI
CreateThread(void *attributes, llgo_size_t stack_size, llgo_thread_start start,
             void *arg, llgo_dword flags, llgo_dword *thread_id);
__declspec(dllimport) llgo_handle LLGO_WINAPI
CreateEventW(void *attributes, llgo_bool manual_reset, llgo_bool initial_state,
             const unsigned short *name);
__declspec(dllimport) llgo_bool LLGO_WINAPI SetEvent(llgo_handle event);
__declspec(dllimport) llgo_dword LLGO_WINAPI
WaitForSingleObject(llgo_handle handle, llgo_dword milliseconds);
__declspec(dllimport) llgo_bool LLGO_WINAPI
SetThreadPriority(llgo_handle thread, int priority);
__declspec(dllimport) llgo_bool LLGO_WINAPI CloseHandle(llgo_handle handle);
__declspec(dllimport) llgo_handle LLGO_WINAPI GetProcessHeap(void);
__declspec(dllimport) void *LLGO_WINAPI HeapAlloc(llgo_handle heap,
                                                  llgo_dword flags,
                                                  llgo_size_t bytes);
__declspec(dllimport) llgo_bool LLGO_WINAPI HeapFree(llgo_handle heap,
                                                     llgo_dword flags,
                                                     void *memory);
__declspec(dllimport) llgo_bool LLGO_WINAPI SwitchToThread(void);

#if defined(_WIN64)
extern void *llgo_windows_lookup_function_entry(llgo_uintptr pc,
                                                llgo_uintptr *image_base);
extern void *llgo_windows_virtual_unwind(llgo_uintptr image_base,
                                         llgo_uintptr pc, void *function_entry,
                                         void *context,
                                         llgo_uintptr *establisher_frame);
#endif

enum {
    llgo_snap_thread = 0x00000004UL,
    llgo_thread_suspend_resume = 0x0002UL,
    llgo_thread_get_context = 0x0008UL,
    llgo_wait_object_0 = 0,
    llgo_wait_timeout = 258,
    llgo_infinite = 0xffffffffUL,
    llgo_heap_zero_memory = 0x00000008UL,
    llgo_thread_priority_highest = 2,
};

#define LLGO_INVALID_HANDLE ((llgo_handle)(llgo_uintptr) - 1)
#define LLGO_SUSPEND_FAILED ((llgo_dword)0xffffffffUL)
#define LLGO_PROF_STACK 64
#define LLGO_PROF_SAMPLES 2048

struct llgo_prof_sample {
    uint32_t n;
    llgo_uintptr pc[LLGO_PROF_STACK];
};

static struct llgo_prof_sample *llgo_prof_ring;
static unsigned int llgo_prof_read_index;
static unsigned int llgo_prof_write_index;
static volatile int llgo_prof_ring_lock;
static volatile int llgo_prof_active;
static volatile int llgo_prof_sampler_running;
static volatile uint64_t llgo_prof_lost;
static llgo_handle llgo_prof_thread;
static llgo_handle llgo_prof_stop_event;

static int llgo_prof_ring_try_lock(void)
{
    return __atomic_exchange_n(&llgo_prof_ring_lock, 1, __ATOMIC_ACQUIRE) == 0;
}

static void llgo_prof_ring_lock_wait(void)
{
    unsigned int spins = 0;
    while (!llgo_prof_ring_try_lock()) {
        if (++spins == 64) {
            SwitchToThread();
            spins = 0;
        }
    }
}

static void llgo_prof_ring_unlock(void)
{
    __atomic_store_n(&llgo_prof_ring_lock, 0, __ATOMIC_RELEASE);
}

static void llgo_prof_drop(void)
{
    __atomic_fetch_add(&llgo_prof_lost, 1, __ATOMIC_RELAXED);
}

#if defined(_WIN64) && (defined(_M_ARM64) || defined(__aarch64__) ||           \
                        defined(_M_X64) || defined(__x86_64__))
#define LLGO_PROF_CONTEXT_SUPPORTED 1

#if defined(_M_ARM64) || defined(__aarch64__)
#define LLGO_PROF_CONTEXT_SIZE 912
#define LLGO_PROF_CONTEXT_FLAGS_OFFSET 0
#define LLGO_PROF_CONTEXT_SP_OFFSET 256
#define LLGO_PROF_CONTEXT_PC_OFFSET 264
#define LLGO_PROF_CONTEXT_LR_OFFSET 248
#define LLGO_PROF_CONTEXT_CONTROL 0x00400003UL
#define LLGO_PROF_CONTEXT_HAS_LR 1
#else
#define LLGO_PROF_CONTEXT_SIZE 1232
#define LLGO_PROF_CONTEXT_FLAGS_OFFSET 48
#define LLGO_PROF_CONTEXT_SP_OFFSET 152
#define LLGO_PROF_CONTEXT_PC_OFFSET 248
#define LLGO_PROF_CONTEXT_CONTROL 0x00100001UL
#define LLGO_PROF_CONTEXT_HAS_LR 0
#endif

static llgo_uintptr llgo_prof_context_word(const unsigned char *context,
                                           size_t offset)
{
    llgo_uintptr value;
    memcpy(&value, context + offset, sizeof(value));
    return value;
}

static void llgo_prof_set_context_word(unsigned char *context, size_t offset,
                                       llgo_uintptr value)
{
    memcpy(context + offset, &value, sizeof(value));
}

static int llgo_prof_unwind_one(unsigned char *context)
{
    llgo_uintptr pc =
        llgo_prof_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET);
    llgo_uintptr sp =
        llgo_prof_context_word(context, LLGO_PROF_CONTEXT_SP_OFFSET);
    llgo_uintptr image_base = 0;
    llgo_uintptr frame = 0;
    void *entry;

    if (pc < 4096)
        return 0;
    entry = llgo_windows_lookup_function_entry(pc, &image_base);
    if (entry != 0) {
        llgo_windows_virtual_unwind(image_base, pc, entry, context, &frame);
        return llgo_prof_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET) >=
                   4096 &&
               (llgo_prof_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET) !=
                    pc ||
                llgo_prof_context_word(context, LLGO_PROF_CONTEXT_SP_OFFSET) !=
                    sp);
    }

#if LLGO_PROF_CONTEXT_HAS_LR
    {
        llgo_uintptr lr =
            llgo_prof_context_word(context, LLGO_PROF_CONTEXT_LR_OFFSET);
        if (lr < 4096)
            return 0;
        llgo_prof_set_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET, lr);
        llgo_prof_set_context_word(context, LLGO_PROF_CONTEXT_LR_OFFSET, 0);
        return 1;
    }
#else
    {
        llgo_uintptr ret = 0;
        llgo_size_t read = 0;
        if ((sp & (sizeof(llgo_uintptr) - 1)) != 0 ||
            !ReadProcessMemory(GetCurrentProcess(), (const void *)sp, &ret,
                               sizeof(ret), &read) ||
            read != sizeof(ret) || ret < 4096)
            return 0;
        llgo_prof_set_context_word(context, LLGO_PROF_CONTEXT_SP_OFFSET,
                                   sp + sizeof(llgo_uintptr));
        llgo_prof_set_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET, ret);
        return 1;
    }
#endif
}

static int llgo_prof_capture(llgo_handle thread,
                             struct llgo_prof_sample *sample)
{
    unsigned char storage[LLGO_PROF_CONTEXT_SIZE + 15];
    unsigned char *context =
        (unsigned char *)(((llgo_uintptr)(storage + 15)) & ~(llgo_uintptr)15);
    llgo_dword flags = LLGO_PROF_CONTEXT_CONTROL;
    llgo_uintptr pc;

    memset(context, 0, LLGO_PROF_CONTEXT_SIZE);
    memcpy(context + LLGO_PROF_CONTEXT_FLAGS_OFFSET, &flags, sizeof(flags));
    if (!GetThreadContext(thread, context))
        return 0;
    pc = llgo_prof_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET);
    if (pc < 4096)
        return 0;

    sample->n = 1;
    /* runtime.CallersFrames subtracts one from each sampled PC. Preserve the
     * interrupted instruction rather than attributing it to its predecessor. */
    sample->pc[0] = pc + 1;
    while (sample->n < LLGO_PROF_STACK && llgo_prof_unwind_one(context)) {
        pc = llgo_prof_context_word(context, LLGO_PROF_CONTEXT_PC_OFFSET);
        sample->pc[sample->n++] = pc;
    }
    return 1;
}
#else
#define LLGO_PROF_CONTEXT_SUPPORTED 0
#endif

static void llgo_prof_record(const struct llgo_prof_sample *sample)
{
    unsigned int next;

    if (!__atomic_load_n(&llgo_prof_active, __ATOMIC_ACQUIRE))
        return;
    llgo_prof_ring_lock_wait();
    if (!__atomic_load_n(&llgo_prof_active, __ATOMIC_RELAXED) ||
        llgo_prof_ring == 0) {
        llgo_prof_ring_unlock();
        return;
    }
    next = llgo_prof_write_index + 1;
    if (next == LLGO_PROF_SAMPLES)
        next = 0;
    if (next == llgo_prof_read_index) {
        llgo_prof_ring_unlock();
        llgo_prof_drop();
        return;
    }
    llgo_prof_ring[llgo_prof_write_index] = *sample;
    llgo_prof_write_index = next;
    llgo_prof_ring_unlock();
}

static void llgo_prof_sample_process(void)
{
#if LLGO_PROF_CONTEXT_SUPPORTED
    llgo_handle snapshot;
    llgo_dword process_id = GetCurrentProcessId();
    llgo_dword sampler_id = GetCurrentThreadId();
    llgo_thread_entry entry;

    snapshot = CreateToolhelp32Snapshot(llgo_snap_thread, 0);
    if (snapshot == LLGO_INVALID_HANDLE) {
        llgo_prof_drop();
        return;
    }
    memset(&entry, 0, sizeof(entry));
    entry.size = sizeof(entry);
    if (Thread32First(snapshot, &entry)) {
        do {
            llgo_handle thread;
            struct llgo_prof_sample sample;
            int captured = 0;

            if (!__atomic_load_n(&llgo_prof_active, __ATOMIC_ACQUIRE))
                break;
            if (entry.owner_process_id != process_id ||
                entry.thread_id == sampler_id)
                continue;
            thread =
                OpenThread(llgo_thread_suspend_resume | llgo_thread_get_context,
                           0, entry.thread_id);
            if (thread == 0)
                continue;
            if (SuspendThread(thread) != LLGO_SUSPEND_FAILED) {
                captured = llgo_prof_capture(thread, &sample);
                ResumeThread(thread);
            }
            CloseHandle(thread);
            if (captured)
                llgo_prof_record(&sample);
        } while (Thread32Next(snapshot, &entry));
    }
    CloseHandle(snapshot);
#endif
}

static llgo_dword LLGO_WINAPI llgo_profiler_thread(void *arg)
{
    llgo_dword period = (llgo_dword)(llgo_uintptr)arg;

    SetThreadPriority(GetCurrentThread(), llgo_thread_priority_highest);
    for (;;) {
        llgo_dword wait = WaitForSingleObject(llgo_prof_stop_event, period);
        if (wait == llgo_wait_object_0 ||
            !__atomic_load_n(&llgo_prof_active, __ATOMIC_ACQUIRE))
            break;
        if (wait != llgo_wait_timeout)
            break;
        llgo_prof_sample_process();
    }
    __atomic_store_n(&llgo_prof_sampler_running, 0, __ATOMIC_RELEASE);
    return 0;
}

/* Returns 1 on success, 0 while an old profile is still draining, and -1 if
 * the platform sampler cannot be started. */
int llgo_cpu_profile_start(int hz)
{
#if LLGO_PROF_CONTEXT_SUPPORTED
    llgo_dword period;
    llgo_handle heap;

    if (hz <= 0)
        return -1;
    period = (llgo_dword)(1000 / hz);
    if (period == 0)
        period = 1;

    llgo_prof_ring_lock_wait();
    if (__atomic_load_n(&llgo_prof_active, __ATOMIC_RELAXED) ||
        __atomic_load_n(&llgo_prof_sampler_running, __ATOMIC_RELAXED) ||
        llgo_prof_read_index != llgo_prof_write_index) {
        llgo_prof_ring_unlock();
        return 0;
    }
    heap = GetProcessHeap();
    if (llgo_prof_ring == 0) {
        llgo_prof_ring = (struct llgo_prof_sample *)HeapAlloc(
            heap, llgo_heap_zero_memory,
            sizeof(struct llgo_prof_sample) * LLGO_PROF_SAMPLES);
        if (llgo_prof_ring == 0) {
            llgo_prof_ring_unlock();
            return -1;
        }
    }
    llgo_prof_read_index = 0;
    llgo_prof_write_index = 0;
    __atomic_store_n(&llgo_prof_lost, 0, __ATOMIC_RELAXED);
    llgo_prof_stop_event = CreateEventW(0, 1, 0, 0);
    if (llgo_prof_stop_event == 0) {
        HeapFree(heap, 0, llgo_prof_ring);
        llgo_prof_ring = 0;
        llgo_prof_ring_unlock();
        return -1;
    }
    __atomic_store_n(&llgo_prof_active, 1, __ATOMIC_RELEASE);
    __atomic_store_n(&llgo_prof_sampler_running, 1, __ATOMIC_RELEASE);
    llgo_prof_thread = CreateThread(0, 0, llgo_profiler_thread,
                                    (void *)(llgo_uintptr)period, 0, 0);
    if (llgo_prof_thread == 0) {
        __atomic_store_n(&llgo_prof_active, 0, __ATOMIC_RELEASE);
        __atomic_store_n(&llgo_prof_sampler_running, 0, __ATOMIC_RELEASE);
        CloseHandle(llgo_prof_stop_event);
        llgo_prof_stop_event = 0;
        HeapFree(heap, 0, llgo_prof_ring);
        llgo_prof_ring = 0;
        llgo_prof_ring_unlock();
        return -1;
    }
    llgo_prof_ring_unlock();
    return 1;
#else
    (void)hz;
    return -1;
#endif
}

void llgo_cpu_profile_stop(void)
{
    llgo_handle thread;
    llgo_handle event;

    if (!__atomic_exchange_n(&llgo_prof_active, 0, __ATOMIC_ACQ_REL))
        return;
    thread = llgo_prof_thread;
    event = llgo_prof_stop_event;
    if (event != 0)
        SetEvent(event);
    if (thread != 0)
        WaitForSingleObject(thread, llgo_infinite);
    if (thread != 0)
        CloseHandle(thread);
    if (event != 0)
        CloseHandle(event);
    llgo_prof_thread = 0;
    llgo_prof_stop_event = 0;
}

int llgo_cpu_profile_refresh_signal(void) { return 0; }

int llgo_cpu_profile_drain(llgo_uintptr *pc, uint32_t *lengths, int max_records,
                           int max_stack, uint64_t *lost, int *empty)
{
    struct llgo_prof_sample *sample;
    unsigned int i, n;
    int records = 0;

    if (pc == 0 || lengths == 0 || max_records <= 0 || max_stack <= 0 ||
        lost == 0 || empty == 0)
        return 0;
    llgo_prof_ring_lock_wait();
    *lost = __atomic_exchange_n(&llgo_prof_lost, 0, __ATOMIC_RELAXED);
    while (llgo_prof_ring != 0 &&
           llgo_prof_read_index != llgo_prof_write_index &&
           records < max_records) {
        sample = &llgo_prof_ring[llgo_prof_read_index];
        n = sample->n;
        if (n > (unsigned int)max_stack)
            n = (unsigned int)max_stack;
        lengths[records] = n;
        for (i = 0; i < n; i++)
            pc[(size_t)records * (size_t)max_stack + i] = sample->pc[i];
        records++;
        llgo_prof_read_index++;
        if (llgo_prof_read_index == LLGO_PROF_SAMPLES)
            llgo_prof_read_index = 0;
    }
    *empty = llgo_prof_read_index == llgo_prof_write_index;
    if (*empty && !__atomic_load_n(&llgo_prof_active, __ATOMIC_ACQUIRE) &&
        !__atomic_load_n(&llgo_prof_sampler_running, __ATOMIC_ACQUIRE) &&
        llgo_prof_ring != 0) {
        HeapFree(GetProcessHeap(), 0, llgo_prof_ring);
        llgo_prof_ring = 0;
        llgo_prof_read_index = 0;
        llgo_prof_write_index = 0;
    }
    llgo_prof_ring_unlock();
    return records;
}

int llgo_cpu_profile_test_fault_recovery(void) { return -1; }
