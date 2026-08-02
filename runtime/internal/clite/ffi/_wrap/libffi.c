#include <ffi.h>

void *llgo_ffi_closure_alloc(void **code) {
    return ffi_closure_alloc(sizeof(ffi_closure), code);
}

/*
 * Use libffi's Go ABI directly when its static-chain register is LLGo's nest
 * register. ARM needs only a final register bridge from libffi's IP/R12 to
 * swiftself/R10. AArch64 Apple and Android reserve X18, so their stock libffi
 * has no Go ABI and still needs the public-ffi_call trampoline.
 */
#if !defined(_WIN32) &&                                                    \
    (defined(__x86_64__) || defined(__i386__) || defined(__riscv) ||       \
     defined(__riscv__) ||                                                 \
     (defined(__aarch64__) && !defined(__APPLE__) && !defined(__ANDROID__)))
#define LLGO_FFI_CALL_GO_DIRECT 1
#elif !defined(_WIN32) && defined(__arm__)
#define LLGO_FFI_CALL_GO_ARM_BRIDGE 1
#elif !defined(_WIN32) && defined(__aarch64__) &&                          \
    (defined(__APPLE__) || defined(__ANDROID__))
#define LLGO_FFI_CALL_PUBLIC_TRAMPOLINE 1
#endif

#if (defined(LLGO_FFI_CALL_GO_DIRECT) ||                                   \
     defined(LLGO_FFI_CALL_GO_ARM_BRIDGE)) &&                              \
    !defined(FFI_GO_CLOSURES)
#error "LLGo hidden closure environments require libffi Go closures on this target"
#endif

#if defined(LLGO_FFI_CALL_GO_DIRECT)

void llgo_ffi_call_with_env(ffi_cif *cif, void (*fn)(void), void *rvalue,
                            void **avalue, void *env) {
    ffi_call_go(cif, fn, rvalue, avalue, env);
}

#elif defined(LLGO_FFI_CALL_GO_ARM_BRIDGE)

struct llgo_ffi_call_context {
    void (*target)(void);
    void *env;
    void *saved_callee;
    void *saved_self;
    void *saved_return;
};

/*
 * ffi_call_go enters this function with the context in IP/R12 and all real
 * arguments already marshalled. Preserve the callee-saved registers used by
 * the continuation, install swiftself/R10, and enter the target without
 * changing SP or any argument register.
 */
__attribute__((naked)) static void llgo_ffi_env_trampoline(void) {
    __asm__ volatile(
        "str r4, [r12, #8]\n\t"
        "str r10, [r12, #12]\n\t"
        "str lr, [r12, #16]\n\t"
        "mov r4, r12\n\t"
        "ldr r10, [r12, #4]\n\t"
        "ldr r12, [r12, #0]\n\t"
        "blx r12\n\t"
        "ldr r12, [r4, #8]\n\t"
        "ldr r10, [r4, #12]\n\t"
        "ldr lr, [r4, #16]\n\t"
        "mov r4, r12\n\t"
        "bx lr");
}

void llgo_ffi_call_with_env(ffi_cif *cif, void (*fn)(void), void *rvalue,
                            void **avalue, void *env) {
    struct llgo_ffi_call_context call = {
        .target = fn,
        .env = env,
    };
    ffi_call_go(cif, llgo_ffi_env_trampoline, rvalue, avalue, &call);
}

#elif defined(LLGO_FFI_CALL_PUBLIC_TRAMPOLINE)

/*
 * Public ffi_call cannot transport swiftself. Keep the real target and env in
 * per-thread state while libffi marshals the arguments. Its final target saves
 * those arguments, obtains the state, installs X20, and enters the real entry
 * with the original SP.
 */
struct llgo_ffi_call_context {
    void (*target)(void);
    void *env;
    void *saved_callee;
    void *saved_self;
    void *saved_return;
};

static _Thread_local struct llgo_ffi_call_context llgo_ffi_call_current;

__attribute__((noinline, used)) static struct llgo_ffi_call_context *
llgo_ffi_current_call(void) {
    return &llgo_ffi_call_current;
}

#if defined(__APPLE__)
#define LLGO_ASM_CSYM(name) "_" #name
#else
#define LLGO_ASM_CSYM(name) #name
#endif

__attribute__((naked)) static void llgo_ffi_env_trampoline(void) {
    __asm__ volatile(
        "sub sp, sp, #224\n\t"
        "stp x0, x1, [sp, #0]\n\t"
        "stp x2, x3, [sp, #16]\n\t"
        "stp x4, x5, [sp, #32]\n\t"
        "stp x6, x7, [sp, #48]\n\t"
        "str x8, [sp, #64]\n\t"
        "str x30, [sp, #72]\n\t"
        "stp q0, q1, [sp, #80]\n\t"
        "stp q2, q3, [sp, #112]\n\t"
        "stp q4, q5, [sp, #144]\n\t"
        "stp q6, q7, [sp, #176]\n\t"
        "bl " LLGO_ASM_CSYM(llgo_ffi_current_call) "\n\t"
        "mov x16, x0\n\t"
        "ldr x17, [x16, #0]\n\t"
        "str x19, [x16, #16]\n\t"
        "str x20, [x16, #24]\n\t"
        "ldr x15, [sp, #72]\n\t"
        "str x15, [x16, #32]\n\t"
        "mov x19, x16\n\t"
        "ldr x20, [x16, #8]\n\t"
        "ldp q0, q1, [sp, #80]\n\t"
        "ldp q2, q3, [sp, #112]\n\t"
        "ldp q4, q5, [sp, #144]\n\t"
        "ldp q6, q7, [sp, #176]\n\t"
        "ldr x8, [sp, #64]\n\t"
        "ldp x0, x1, [sp, #0]\n\t"
        "ldp x2, x3, [sp, #16]\n\t"
        "ldp x4, x5, [sp, #32]\n\t"
        "ldp x6, x7, [sp, #48]\n\t"
        "add sp, sp, #224\n\t"
        "blr x17\n\t"
        "ldr x16, [x19, #16]\n\t"
        "ldr x20, [x19, #24]\n\t"
        "ldr x30, [x19, #32]\n\t"
        "mov x19, x16\n\t"
        "ret");
}

void llgo_ffi_call_with_env(ffi_cif *cif, void (*fn)(void), void *rvalue,
                            void **avalue, void *env) {
    struct llgo_ffi_call_context previous = llgo_ffi_call_current;
    llgo_ffi_call_current.target = fn;
    llgo_ffi_call_current.env = env;
    ffi_call(cif, llgo_ffi_env_trampoline, rvalue, avalue);
    llgo_ffi_call_current = previous;
}

#endif
