#if defined(__linux__)
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <features.h>
#endif

#if defined(_WIN32)

/* Keep the debug shim independent of the Windows SDK. A native LLGo host may
 * use an LLVM/MinGW toolchain without Visual Studio installed, and these are
 * the complete Kernel32 declarations needed by this file. */
typedef __UINTPTR_TYPE__ uintptr_t;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef int llgo_win_bool;
typedef unsigned long llgo_win_dword;
typedef void *llgo_win_module;

__declspec(dllimport) llgo_win_dword LLGO_WINAPI GetLastError(void);
__declspec(dllimport) void LLGO_WINAPI SetLastError(llgo_win_dword error);
__declspec(dllimport) llgo_win_module LLGO_WINAPI GetModuleHandleA(const char *name);
__declspec(dllimport) llgo_win_bool LLGO_WINAPI GetModuleHandleExA(
    llgo_win_dword flags, const char *address, llgo_win_module *module);
__declspec(dllimport) void *LLGO_WINAPI GetProcAddress(llgo_win_module module,
                                                      const char *name);

typedef struct {
    const char *dli_fname;
    void *dli_fbase;
    const char *dli_sname;
    void *dli_saddr;
} llgo_dl_info;

enum {
    llgo_get_module_handle_from_address = 0x00000004,
    llgo_get_module_handle_unchanged_refcount = 0x00000002,
};

#else

#include <dlfcn.h>
#include <errno.h>
#include <stdint.h>
typedef Dl_info llgo_dl_info;

#endif

void *llgo_address() {
    return __builtin_return_address(0);
}

int llgo_addrinfo(void *addr, llgo_dl_info *info) {
#if defined(_WIN32)
    llgo_win_dword saved_error = GetLastError();
    llgo_win_module module = 0;
    int ret = 0;
    if (info != 0) {
        info->dli_fname = 0;
        info->dli_fbase = 0;
        info->dli_sname = 0;
        info->dli_saddr = 0;
        if (GetModuleHandleExA(llgo_get_module_handle_from_address |
                                   llgo_get_module_handle_unchanged_refcount,
                               (const char *)addr, &module)) {
            info->dli_fbase = module;
            ret = 1;
        }
    }
    SetLastError(saved_error);
    return ret;
#else
    int saved_errno = errno;
    int ret = dladdr(addr, info);
    errno = saved_errno;
    return ret;
#endif
}

void *llgo_symbol(char *name) {
#if defined(_WIN32)
    llgo_win_dword saved_error = GetLastError();
    llgo_win_module module = GetModuleHandleA(0);
    /* PE has no RTLD_DEFAULT equivalent, so this fallback intentionally
     * covers only symbols exported by the main image. */
    void *ret = module != 0 ? GetProcAddress(module, name) : 0;
    SetLastError(saved_error);
    return ret;
#else
    int saved_errno = errno;
    void *ret = dlsym(RTLD_DEFAULT, name);
    errno = saved_errno;
    return ret;
#endif
}

void llgo_stacktrace(int skip, void *ctx, int (*fn)(void *ctx, void *pc, void *offset, void *sp, char *name)) {
    /* Frame-pointer chain walk. LLGo compiles every Go function with
     * "frame-pointer"="non-leaf", so [fp] is the previous frame pointer and
     * [fp+1] the return address on both arm64 and x86-64. This replaces the
     * libunwind cursor: no unwind tables, no -lunwind, and it keeps working
     * through any frame that maintains the chain (C code compiled with
     * frame pointers included). The walk stops at the first frame that
     * breaks chain discipline.
     *
     * The Go-side walker (runtime/internal/lib/runtime/unwind_llgo.go
     * fpCallers) implements the same discipline plus a text-range bound the
     * frame tables provide; keep the chain guards below in sync with it. */
#if defined(_WIN32)
    llgo_win_dword saved_error = GetLastError();
#else
    int saved_errno = errno;
#endif
    uintptr_t fp = (uintptr_t)__builtin_frame_address(0);
    int depth = 0;
    while (fp) {
        uintptr_t prev = *(uintptr_t *)fp;
        uintptr_t pc = *((uintptr_t *)fp + 1);
        if (pc < 4096)
            break;
        if (depth >= skip) {
#if defined(_WIN32)
            const char *name = "";
            uintptr_t offset = 0;
#else
            Dl_info info;
            const char *name = "";
            uintptr_t offset = 0;
            if (dladdr((void *)pc, &info) && info.dli_sname) {
                name = info.dli_sname;
                offset = pc - (uintptr_t)info.dli_saddr;
            }
#endif
            if (fn(ctx, (void *)pc, (void *)offset, (void *)fp, (char *)name) == 0)
                break;
        }
        depth++;
        if (prev <= fp || prev - fp > (uintptr_t)1 << 20 || (prev & (sizeof(uintptr_t) - 1)))
            break;
        fp = prev;
    }
#if defined(_WIN32)
    SetLastError(saved_error);
#else
    errno = saved_errno;
#endif
}
