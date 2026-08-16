/* CPU profiling for native LLGo executables.
 *
 * SIGPROF interrupts arbitrary code, so the handler only snapshots register
 * state and frame-pointer slots into a fixed ring. It does not allocate,
 * acquire a blocking lock, or call into Go. Ordinary Go code drains the ring
 * later and converts it to runtime/pprof's raw record stream. */
#define _XOPEN_SOURCE 700
#define _DARWIN_C_SOURCE 1
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>
#include <sys/time.h>

#if defined(__APPLE__) || defined(__linux__)
#include <ucontext.h>
#endif

#define LLGO_PROF_STACK 64
#define LLGO_PROF_SAMPLES 2048
#define LLGO_PROF_MAX_FP_STRIDE (1u << 20)

struct llgo_prof_sample {
    uint32_t n;
    uintptr_t pc[LLGO_PROF_STACK];
};

static struct llgo_prof_sample llgo_prof_ring[LLGO_PROF_SAMPLES];
static unsigned int llgo_prof_read_index;
static unsigned int llgo_prof_write_index;
static volatile int llgo_prof_lock;
static volatile int llgo_prof_active;
static volatile uint64_t llgo_prof_lost;
#if defined(__APPLE__) || defined(__linux__)
static struct sigaction llgo_prof_previous_action;
static int llgo_prof_previous_valid;
#endif

extern int llgo_mem_readable(void *p);

static int llgo_prof_try_lock(void)
{
    return __atomic_exchange_n(&llgo_prof_lock, 1, __ATOMIC_ACQUIRE) == 0;
}

static void llgo_prof_lock_wait(void)
{
    while (!llgo_prof_try_lock()) {
    }
}

static void llgo_prof_unlock(void)
{
    __atomic_store_n(&llgo_prof_lock, 0, __ATOMIC_RELEASE);
}

static void llgo_prof_drop(void)
{
    __atomic_fetch_add(&llgo_prof_lost, 1, __ATOMIC_RELAXED);
}

#if defined(__APPLE__) || defined(__linux__)
static void llgo_prof_signal(int sig, siginfo_t *info, void *uctx);

static int llgo_prof_action_is_ours(const struct sigaction *sa)
{
    return (sa->sa_flags & SA_SIGINFO) != 0 &&
           sa->sa_sigaction == llgo_prof_signal;
}

static int llgo_prof_install_signal_locked(void)
{
    struct sigaction current;
    struct sigaction sa;

    if (sigaction(SIGPROF, 0, &current) != 0)
        return -1;
    if (llgo_prof_action_is_ours(&current))
        return 0;

    /* Keep the disposition that was current immediately before this install.
     * A default disposition is normalized to ignore, as the Go runtime does:
     * a final pending timer signal must not terminate the process after Stop. */
    llgo_prof_previous_action = current;
    if ((current.sa_flags & SA_SIGINFO) == 0 &&
        current.sa_handler == SIG_DFL)
        llgo_prof_previous_action.sa_handler = SIG_IGN;
    llgo_prof_previous_valid = 1;

    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = llgo_prof_signal;
    sa.sa_mask = current.sa_mask;
    sa.sa_flags = SA_SIGINFO | SA_RESTART;
#ifdef SA_ONSTACK
    sa.sa_flags |= current.sa_flags & SA_ONSTACK;
#endif
    return sigaction(SIGPROF, &sa, 0);
}

static void llgo_prof_restore_signal_locked(void)
{
    struct sigaction current;

    if (!llgo_prof_previous_valid || sigaction(SIGPROF, 0, &current) != 0)
        return;
    /* Do not overwrite a handler installed by foreign code that did not use
     * the coordinated os/signal path below. */
    if (llgo_prof_action_is_ours(&current))
        sigaction(SIGPROF, &llgo_prof_previous_action, 0);
}

static void llgo_prof_signal(int sig, siginfo_t *info, void *uctx)
{
    uintptr_t pc = 0, fp = 0;
    uintptr_t word = sizeof(uintptr_t);
    unsigned int next;
    struct llgo_prof_sample *sample;
    int active;
    ucontext_t *uc = (ucontext_t *)uctx;
    int saved_errno = errno;
    (void)sig;
    (void)info;
    (void)uc;

    active = __atomic_load_n(&llgo_prof_active, __ATOMIC_ACQUIRE);
    /* Like the Go runtime, the profiler owns SIGPROF while active. In
     * particular, timer samples are not forwarded to os/signal watchers. */
    if (!active) {
        errno = saved_errno;
        return;
    }
    if (!llgo_prof_try_lock()) {
        llgo_prof_drop();
        errno = saved_errno;
        return;
    }
    active = __atomic_load_n(&llgo_prof_active, __ATOMIC_RELAXED);
    if (!active)
        goto done;
#if defined(__APPLE__) && defined(__aarch64__)
    pc = (uintptr_t)uc->uc_mcontext->__ss.__pc;
    fp = (uintptr_t)uc->uc_mcontext->__ss.__fp;
#elif defined(__APPLE__) && defined(__x86_64__)
    pc = (uintptr_t)uc->uc_mcontext->__ss.__rip;
    fp = (uintptr_t)uc->uc_mcontext->__ss.__rbp;
#elif defined(__linux__) && defined(__aarch64__)
    pc = (uintptr_t)uc->uc_mcontext.pc;
    fp = (uintptr_t)uc->uc_mcontext.regs[29];
#elif defined(__linux__) && defined(__x86_64__)
    pc = (uintptr_t)uc->uc_mcontext.gregs[16 /* REG_RIP */];
    fp = (uintptr_t)uc->uc_mcontext.gregs[10 /* REG_RBP */];
#endif
    if (pc == 0) {
        llgo_prof_drop();
        goto done;
    }

    next = llgo_prof_write_index + 1;
    if (next == LLGO_PROF_SAMPLES)
        next = 0;
    if (next == llgo_prof_read_index) {
        llgo_prof_drop();
        goto done;
    }

    sample = &llgo_prof_ring[llgo_prof_write_index];
    sample->n = 1;
    /* runtime.CallersFrames subtracts one from every sampled PC. */
    sample->pc[0] = pc + 1;
    while (fp != 0 && sample->n < LLGO_PROF_STACK) {
        uintptr_t prev, ret;
        if ((fp & (word - 1)) != 0 ||
            !llgo_mem_readable((void *)fp) ||
            !llgo_mem_readable((void *)(fp + word)))
            break;
        prev = *(uintptr_t *)fp;
        ret = *(uintptr_t *)(fp + word);
        if (ret < 4096)
            break;
        sample->pc[sample->n++] = ret;
        if (prev <= fp || prev - fp > LLGO_PROF_MAX_FP_STRIDE ||
            (prev & (word - 1)) != 0)
            break;
        fp = prev;
    }
    llgo_prof_write_index = next;
done:
    llgo_prof_unlock();
    errno = saved_errno;
}
#endif

/* Returns 1 on success, 0 while an old profile is still draining, and -1
 * when the OS rejects SIGPROF or ITIMER_PROF setup. */
int llgo_cpu_profile_start(int hz)
{
#if defined(__APPLE__) || defined(__linux__)
    struct itimerval timer;
    uint64_t usec;
    int saved_errno = errno;

    if (hz <= 0) {
        errno = saved_errno;
        return -1;
    }
    llgo_prof_lock_wait();
    if (__atomic_load_n(&llgo_prof_active, __ATOMIC_RELAXED) ||
        llgo_prof_read_index != llgo_prof_write_index) {
        llgo_prof_unlock();
        errno = saved_errno;
        return 0;
    }
    if (llgo_prof_install_signal_locked() != 0) {
        llgo_prof_unlock();
        errno = saved_errno;
        return -1;
    }
    llgo_prof_read_index = 0;
    llgo_prof_write_index = 0;
    __atomic_store_n(&llgo_prof_lost, 0, __ATOMIC_RELAXED);
    __atomic_store_n(&llgo_prof_active, 1, __ATOMIC_RELEASE);

    usec = 1000000u / (unsigned int)hz;
    if (usec == 0)
        usec = 1;
    memset(&timer, 0, sizeof(timer));
    timer.it_interval.tv_sec = (time_t)(usec / 1000000u);
    timer.it_interval.tv_usec = (suseconds_t)(usec % 1000000u);
    timer.it_value = timer.it_interval;
    if (setitimer(ITIMER_PROF, &timer, 0) != 0) {
        __atomic_store_n(&llgo_prof_active, 0, __ATOMIC_RELEASE);
        llgo_prof_restore_signal_locked();
        llgo_prof_unlock();
        errno = saved_errno;
        return -1;
    }
    llgo_prof_unlock();
    errno = saved_errno;
    return 1;
#else
    (void)hz;
    return -1;
#endif
}

void llgo_cpu_profile_stop(void)
{
#if defined(__APPLE__) || defined(__linux__)
    struct itimerval timer;
    int saved_errno = errno;
    llgo_prof_lock_wait();
    memset(&timer, 0, sizeof(timer));
    setitimer(ITIMER_PROF, &timer, 0);
    __atomic_store_n(&llgo_prof_active, 0, __ATOMIC_RELEASE);
    llgo_prof_restore_signal_locked();
    llgo_prof_unlock();
    errno = saved_errno;
#endif
}

/* Serialize a libuv SIGPROF watcher update with profiler start/stop. While the
 * lock is held, an interrupting profiling signal is dropped instead of
 * blocking in signal context. */
void llgo_cpu_profile_signal_update_begin(void)
{
#if defined(__APPLE__) || defined(__linux__)
    llgo_prof_lock_wait();
#endif
}

int llgo_cpu_profile_signal_update_end(void)
{
#if defined(__APPLE__) || defined(__linux__)
    int ret = 0;
    if (__atomic_load_n(&llgo_prof_active, __ATOMIC_RELAXED))
        ret = llgo_prof_install_signal_locked();
    llgo_prof_unlock();
    return ret;
#else
    return 0;
#endif
}

int llgo_cpu_profile_read(uintptr_t *pc, int cap)
{
    struct llgo_prof_sample *sample;
    unsigned int i;
    int n;

    if (pc == 0 || cap <= 0)
        return 0;
    llgo_prof_lock_wait();
    if (llgo_prof_read_index == llgo_prof_write_index) {
        llgo_prof_unlock();
        return 0;
    }
    sample = &llgo_prof_ring[llgo_prof_read_index];
    n = (int)sample->n;
    if (n > cap)
        n = cap;
    for (i = 0; i < (unsigned int)n; i++)
        pc[i] = sample->pc[i];
    llgo_prof_read_index++;
    if (llgo_prof_read_index == LLGO_PROF_SAMPLES)
        llgo_prof_read_index = 0;
    llgo_prof_unlock();
    return n;
}

uint64_t llgo_cpu_profile_take_lost(void)
{
    return __atomic_exchange_n(&llgo_prof_lost, 0, __ATOMIC_RELAXED);
}

int llgo_cpu_profile_empty(void)
{
    int empty;
    llgo_prof_lock_wait();
    empty = llgo_prof_read_index == llgo_prof_write_index;
    llgo_prof_unlock();
    return empty;
}
