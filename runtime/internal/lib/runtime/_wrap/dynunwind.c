/* Dynamically-loaded libunwind for fault-site C stacks.
 *
 * The FP-walk in unwind_llgo.go breaks on C frames compiled without frame
 * pointers, and on linux dladdr cannot name non-dynamic C symbols. This
 * code resolves libunwind AT RUNTIME (dlopen/dlsym at handler-install
 * time — never in the signal handler itself; no link-time -lunwind) and
 * unwinds the fault ucontext with DWARF/compact unwind info, resuming with
 * the FP chain where unwind info runs out.
 *
 * Flavors probed, in order:
 *   darwin: unw_* live in libSystem — dlsym(RTLD_DEFAULT), no dlopen.
 *           unw_context_t is a register buffer (LLVM layout); we translate
 *           the mcontext thread state into it.
 *   linux:  1) nongnu libunwind (libunwind.so.8): symbols are arch-prefixed
 *              (_ULx86_64_* / _ULaarch64_*); unw_init_local accepts the
 *              signal ucontext_t directly, and unw_get_proc_name reads
 *              .symtab so static C symbols get named.
 *           2) LLVM libunwind (libunwind.so.1): plain unw_* symbols;
 *              context translated like darwin.
 *
 * Async-signal-safety: dlopen/dlsym/getenv happen once at init. In the
 * handler only unw_init_local(2)/unw_step/unw_get_reg run; nongnu libunwind
 * documents these as thread-safe and usable from signal handlers for local
 * unwinding (remote/global state is only touched on first use — we do a
 * throwaway warm-up unwind at init to pre-fault lazy state). The debug
 * printing (snprintf/write) is for the experiment only.
 */
#define _XOPEN_SOURCE 700
#define _DARWIN_C_SOURCE 1
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <stddef.h>

#if (defined(__APPLE__) || defined(__linux__)) && (defined(__aarch64__) || defined(__x86_64__))

#include <dlfcn.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ucontext.h>
#include <unistd.h>

#define DYNUNW_MAX 64
#define DYNUNW_NAME_LEN 64

/* Results of the last fault-time unwind; read by the Go side. */
static uintptr_t dynunw_pcs[DYNUNW_MAX];
static int dynunw_count;
static uintptr_t dynunw_end_fp; /* fp at the frame the unwind stopped on */
static char dynunw_names[DYNUNW_MAX][DYNUNW_NAME_LEN];
static uintptr_t dynunw_offs[DYNUNW_MAX];
static long long dynunw_ns; /* handler-path cost of the dynamic unwind */

static int dynunw_enabled;   /* LLGO_EXP_DYNUNWIND=1 */
static int dynunw_debug;     /* LLGO_EXP_DYNUNWIND_DEBUG=1: print in handler */

/* Function-pointer view of whichever libunwind flavor we resolved.
 * All flavors share these shapes (unw_word_t == uintptr_t here). */
typedef int (*unw_init_local_fn)(void *cursor, void *ctx);
typedef int (*unw_init_local2_fn)(void *cursor, void *ctx, int flags);
typedef int (*unw_step_fn)(void *cursor);
typedef int (*unw_get_reg_fn)(void *cursor, int reg, uintptr_t *val);
typedef int (*unw_get_proc_name_fn)(void *cursor, char *buf, size_t len,
                                    uintptr_t *off);

static unw_init_local_fn p_init_local;
static unw_init_local2_fn p_init_local2; /* preferred: UNW_INIT_SIGNAL_FRAME */
static unw_step_fn p_step;
static unw_get_reg_fn p_get_reg;
static unw_get_proc_name_fn p_get_proc_name;
/* Whether to call unw_get_proc_name inside the fault handler. Darwin's
 * implementation is in-memory (compact unwind / dyld tables); nongnu's
 * reads /proc/self/maps and mmaps ELF .symtab — not async-signal-safe,
 * so it is off by default on linux (LLGO_DYNUNWIND_NAMES=1 re-enables
 * for diagnostics; frame pcs and end-fp are still captured). */
static int dynunw_capture_names;

static int dynunw_reg_ip; /* UNW_REG_IP for the resolved flavor */
static int dynunw_reg_fp; /* frame-pointer regnum for the resolved flavor */
static int dynunw_ctx_is_ucontext; /* nongnu: pass the ucontext_t through */

const char *llgo_dynunwind_flavor = "none";

/* Cursor/context buffers. Sized generously: nongnu aarch64's unw_cursor_t is
 * 4096 words (33KB); LLVM's is ~1.5KB. Static so the signal stack does not
 * pay for them (the fault path is not reentrant: the runtime converts the
 * fault to a panic and never returns to the handler). */
static uint64_t dynunw_cursor[8192];
static uint64_t dynunw_ctx[1024];

#define UNW_INIT_SIGNAL_FRAME 1

/* ---- context translation ---------------------------------------------- */

/* LLVM libunwind's unw_context_t is the unw_getcontext register dump:
 *   x86_64:  rax rbx rcx rdx rdi rsi rbp rsp r8..r15 rip   (17 words)
 *   aarch64: x0..x28 fp lr sp pc ra_sign_state             (34 words)
 * Both match (or prefix-match) the platform thread state closely enough to
 * fill from the fault mcontext. */
static void *dynunw_make_ctx(void *uctx)
{
    ucontext_t *uc = (ucontext_t *)uctx;
    if (dynunw_ctx_is_ucontext)
        return uctx; /* nongnu linux: unw_context_t IS ucontext_t */
#if defined(__APPLE__) && defined(__aarch64__)
    /* __darwin_arm_thread_state64: x[29] fp lr sp pc cpsr(32) pad(32).
     * First 33 words line up with LLVM's GPRs; word 33 is ra_sign_state
     * there vs cpsr here — zero it (no PAC signing state carried over). */
    memcpy(dynunw_ctx, &uc->uc_mcontext->__ss, 33 * sizeof(uint64_t));
    dynunw_ctx[33] = 0;
#elif defined(__APPLE__) && defined(__x86_64__)
    /* __darwin_x86_thread_state64 is exactly LLVM's GPR order for the first
     * 17 words (rax..r15, rip). */
    memcpy(dynunw_ctx, &uc->uc_mcontext->__ss, 17 * sizeof(uint64_t));
#elif defined(__linux__) && defined(__x86_64__)
    {
        /* linux gregs order: R8 R9 R10 R11 R12 R13 R14 R15 RDI RSI RBP RBX
         * RDX RAX RCX RSP RIP ... — permute into LLVM order. */
        greg_t *g = uc->uc_mcontext.gregs;
        dynunw_ctx[0] = (uint64_t)g[13];  /* rax */
        dynunw_ctx[1] = (uint64_t)g[11];  /* rbx */
        dynunw_ctx[2] = (uint64_t)g[14];  /* rcx */
        dynunw_ctx[3] = (uint64_t)g[12];  /* rdx */
        dynunw_ctx[4] = (uint64_t)g[8];   /* rdi */
        dynunw_ctx[5] = (uint64_t)g[9];   /* rsi */
        dynunw_ctx[6] = (uint64_t)g[10];  /* rbp */
        dynunw_ctx[7] = (uint64_t)g[15];  /* rsp */
        dynunw_ctx[8] = (uint64_t)g[0];   /* r8 */
        dynunw_ctx[9] = (uint64_t)g[1];   /* r9 */
        dynunw_ctx[10] = (uint64_t)g[2];  /* r10 */
        dynunw_ctx[11] = (uint64_t)g[3];  /* r11 */
        dynunw_ctx[12] = (uint64_t)g[4];  /* r12 */
        dynunw_ctx[13] = (uint64_t)g[5];  /* r13 */
        dynunw_ctx[14] = (uint64_t)g[6];  /* r14 */
        dynunw_ctx[15] = (uint64_t)g[7];  /* r15 */
        dynunw_ctx[16] = (uint64_t)g[16]; /* rip */
    }
#elif defined(__linux__) && defined(__aarch64__)
    memcpy(dynunw_ctx, uc->uc_mcontext.regs, 31 * sizeof(uint64_t)); /* x0..x30 */
    dynunw_ctx[31] = (uint64_t)uc->uc_mcontext.sp;
    dynunw_ctx[32] = (uint64_t)uc->uc_mcontext.pc;
    dynunw_ctx[33] = 0;
#endif
    return dynunw_ctx;
}

/* ---- flavor resolution (init time, NOT in the handler) ----------------- */

static void dynunw_use_llvm_regnums(void)
{
    dynunw_reg_ip = -1; /* LLVM UNW_REG_IP */
#if defined(__aarch64__)
    dynunw_reg_fp = 29; /* UNW_ARM64_FP */
#else
    dynunw_reg_fp = 6; /* UNW_X86_64_RBP */
#endif
}

static int dynunw_resolve(void)
{
#if defined(__APPLE__)
    /* unw_* are exported by libSystem's libunwind — already loaded. */
    p_init_local = (unw_init_local_fn)dlsym(RTLD_DEFAULT, "unw_init_local");
    p_init_local2 =
        (unw_init_local2_fn)dlsym(RTLD_DEFAULT, "unw_init_local2");
    p_step = (unw_step_fn)dlsym(RTLD_DEFAULT, "unw_step");
    p_get_reg = (unw_get_reg_fn)dlsym(RTLD_DEFAULT, "unw_get_reg");
    p_get_proc_name =
        (unw_get_proc_name_fn)dlsym(RTLD_DEFAULT, "unw_get_proc_name");
    if (p_init_local && p_step && p_get_reg) {
        dynunw_use_llvm_regnums();
        dynunw_ctx_is_ucontext = 0;
        dynunw_capture_names = 1;
        llgo_dynunwind_flavor = "darwin-libsystem";
        return 1;
    }
    return 0;
#elif defined(__linux__)
#if defined(__x86_64__)
    static const char *pfx = "_ULx86_64_";
    dynunw_reg_ip = 16; /* UNW_X86_64_RIP */
    dynunw_reg_fp = 6;  /* UNW_X86_64_RBP */
#else
    static const char *pfx = "_ULaarch64_";
    dynunw_reg_ip = 32; /* UNW_AARCH64_PC */
    dynunw_reg_fp = 29; /* UNW_AARCH64_X29 */
#endif
    /* 1) nongnu libunwind: arch-prefixed local-only symbols; ucontext in;
     *    .symtab-aware unw_get_proc_name (names static C symbols). */
    {
        const char *names[] = {"libunwind.so.8", "libunwind.so", 0};
        int i;
        for (i = 0; names[i]; i++) {
            void *h = dlopen(names[i], RTLD_NOW | RTLD_GLOBAL);
            char sym[64];
            if (!h)
                continue;
#define RESOLVE(dst, name, type)                                              \
    do {                                                                      \
        snprintf(sym, sizeof(sym), "%s%s", pfx, name);                        \
        dst = (type)dlsym(h, sym);                                            \
    } while (0)
            RESOLVE(p_init_local, "init_local", unw_init_local_fn);
            RESOLVE(p_init_local2, "init_local2", unw_init_local2_fn);
            RESOLVE(p_step, "step", unw_step_fn);
            RESOLVE(p_get_reg, "get_reg", unw_get_reg_fn);
            RESOLVE(p_get_proc_name, "get_proc_name", unw_get_proc_name_fn);
#undef RESOLVE
            if (p_init_local && p_step && p_get_reg) {
                dynunw_ctx_is_ucontext = 1;
                {
                    const char *e = getenv("LLGO_DYNUNWIND_NAMES");
                    dynunw_capture_names = e && e[0] == '1' && !e[1];
                }
                llgo_dynunwind_flavor = "linux-nongnu";
                return 1;
            }
        }
    }
    /* 2) LLVM libunwind: plain unw_* names, register-buffer context. */
    {
        const char *names[] = {"libunwind.so.1", "libunwind.so", 0};
        int i;
        for (i = 0; names[i]; i++) {
            void *h = dlopen(names[i], RTLD_NOW | RTLD_GLOBAL);
            if (!h)
                continue;
            p_init_local = (unw_init_local_fn)dlsym(h, "unw_init_local");
            p_init_local2 = (unw_init_local2_fn)dlsym(h, "unw_init_local2");
            p_step = (unw_step_fn)dlsym(h, "unw_step");
            p_get_reg = (unw_get_reg_fn)dlsym(h, "unw_get_reg");
            p_get_proc_name =
                (unw_get_proc_name_fn)dlsym(h, "unw_get_proc_name");
            if (p_init_local && p_step && p_get_reg) {
                dynunw_use_llvm_regnums();
                dynunw_ctx_is_ucontext = 0;
                llgo_dynunwind_flavor = "linux-llvm";
                return 1;
            }
        }
    }
    return 0;
#else
    return 0;
#endif
}

/* ---- the fault-time unwind --------------------------------------------- */

static int dynunw_run(void *uctx)
{
    void *ctx = dynunw_make_ctx(uctx);
    int n = 0, first = 1;
    dynunw_end_fp = 0;
    if (p_init_local2) {
        if (p_init_local2(dynunw_cursor, ctx, UNW_INIT_SIGNAL_FRAME) != 0)
            return 0;
    } else if (p_init_local(dynunw_cursor, ctx) != 0) {
        return 0;
    }
    while (n < DYNUNW_MAX) {
        uintptr_t ip = 0, off = 0;
        if (p_get_reg(dynunw_cursor, dynunw_reg_ip, &ip) != 0 || ip == 0)
            break;
        dynunw_pcs[n] = ip;
        dynunw_names[n][0] = 0;
        dynunw_offs[n] = 0;
        if (dynunw_capture_names && p_get_proc_name &&
            p_get_proc_name(dynunw_cursor, dynunw_names[n], DYNUNW_NAME_LEN,
                            &off) == 0)
            dynunw_offs[n] = off;
        (void)first;
        n++;
        if (p_get_reg(dynunw_cursor, dynunw_reg_fp, &dynunw_end_fp) != 0)
            dynunw_end_fp = 0;
        first = 0;
        if (p_step(dynunw_cursor) <= 0)
            break;
    }
    return n;
}

/* Called from llgo_fault_trampoline (fault.c) with the raw ucontext. */
void llgo_dynunwind_capture(void *uctx)
{
    struct timespec t0, t1;
    if (!dynunw_enabled || !p_step)
        return;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    dynunw_count = dynunw_run(uctx);
    clock_gettime(CLOCK_MONOTONIC, &t1);
    dynunw_ns = (long long)(t1.tv_sec - t0.tv_sec) * 1000000000LL +
                (t1.tv_nsec - t0.tv_nsec);
    if (dynunw_debug) {
        char line[160];
        int i, len;
        len = snprintf(line, sizeof(line),
                       "[dynunwind] flavor=%s frames=%d cost=%lldns\n",
                       llgo_dynunwind_flavor, dynunw_count, dynunw_ns);
        write(2, line, len);
        for (i = 0; i < dynunw_count; i++) {
            len = snprintf(line, sizeof(line), "[dynunwind] #%02d %p %s+0x%lx\n",
                           i, (void *)dynunw_pcs[i],
                           dynunw_names[i][0] ? dynunw_names[i] : "??",
                           (unsigned long)dynunw_offs[i]);
            write(2, line, len);
        }
    }
}

/* Called once from llgo_install_fault_handler: resolve + warm up. */
void llgo_dynunwind_init(void)
{
    /* On by default when a libunwind flavor resolves; LLGO_DYNUNWIND=0
     * forces the plain FP-walk. */
    const char *e = getenv("LLGO_DYNUNWIND");
    dynunw_enabled = !(e && *e == '0');
    e = getenv("LLGO_DYNUNWIND_DEBUG");
    dynunw_debug = e && *e && *e != '0';
    if (!dynunw_enabled)
        return;
    if (!dynunw_resolve()) {
        dynunw_enabled = 0;
        if (dynunw_debug)
            write(2, "[dynunwind] no libunwind flavor resolved\n", 41);
        return;
    }
    if (dynunw_debug) {
        char line[96];
        int len = snprintf(line, sizeof(line), "[dynunwind] resolved flavor=%s\n",
                           llgo_dynunwind_flavor);
        write(2, line, len);
    }
    /* Warm-up: nongnu libunwind builds lazy per-process state (address-space
     * setup, dl_iterate_phdr caches) on first use; do that in normal context
     * so the handler-time path avoids first-use allocation. Unwinding our
     * own live context here is safe and exercises the whole path. */
#if defined(__linux__)
    {
        ucontext_t uc;
        if (dynunw_ctx_is_ucontext && getcontext(&uc) == 0) {
            dynunw_run(&uc);
            dynunw_count = 0;
        }
        /* LLVM flavor: no ucontext path for the warm-up (unw_getcontext is a
         * register dump we cannot fabricate portably here); its first-use
         * state is per-cursor anyway. */
    }
#endif /* getcontext is deprecated on darwin; darwin needs no warm-up (all
        * state lives in libSystem, already initialized by normal use). */
}

/* ---- accessors for the Go side ----------------------------------------- */

uintptr_t *llgo_dynunwind_pcbuf(void) { return dynunw_pcs; }
int llgo_dynunwind_pccount(void) { return dynunw_count; }
uintptr_t llgo_dynunwind_endfp(void) { return dynunw_end_fp; }

/* Symbol name libunwind reported for frame i ("" when unknown). nongnu
 * libunwind reads .symtab, naming static C symbols dladdr cannot see. */
const char *llgo_dynunwind_name(int i)
{
    if (i < 0 || i >= dynunw_count)
        return "";
    return dynunw_names[i];
}

#else /* unsupported platform */

void llgo_dynunwind_init(void) {}
void llgo_dynunwind_capture(void *uctx) { (void)uctx; }
uintptr_t *llgo_dynunwind_pcbuf(void) { return 0; }
int llgo_dynunwind_pccount(void) { return 0; }
uintptr_t llgo_dynunwind_endfp(void) { return 0; }
const char *llgo_dynunwind_name(int i) { (void)i; return ""; }

#endif
