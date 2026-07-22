#include <unistd.h>
#include <stdint.h>

#if defined(__APPLE__)
// The patched runtime provides os.runtime_args even when package os is not
// linked. Darwin's external linker diagnoses its reference to
// os.executablePath before dead stripping the unused function. Provide a weak,
// zero-valued Go string so runtime-only shared/static libraries remain
// linkable; package os supplies the strong definition when it is present.
struct llgo_go_string {
    const char *p;
    intptr_t n;
};
struct llgo_go_string llgo_os_executable_path __asm("_os.executablePath")
    __attribute__((weak));
#endif

int llgo_maxprocs()
{
#ifdef _SC_NPROCESSORS_ONLN
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
#else
    return 1;
#endif
}

__attribute__((noinline)) void *llgo_framepointer(void)
{
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_frame_address(0);
#else
    return 0;
#endif
}
