/* Seed the Windows runtime PRNG from the system-preferred CSPRNG without
 * requiring a bcrypt import library in the target sysroot. */
#include <stdint.h>

typedef __SIZE_TYPE__ llgo_size_t;
typedef unsigned long llgo_dword;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

typedef void *llgo_module;
typedef long(LLGO_WINAPI *llgo_bcrypt_gen_random)(
    void *algorithm, unsigned char *buffer, llgo_dword size,
    llgo_dword flags);

__declspec(dllimport) llgo_module LLGO_WINAPI LoadLibraryA(const char *name);
__declspec(dllimport) void *LLGO_WINAPI GetProcAddress(
    llgo_module module, const char *name);
__declspec(dllimport) int LLGO_WINAPI FreeLibrary(llgo_module module);

#define LLGO_BCRYPT_USE_SYSTEM_PREFERRED_RNG ((llgo_dword)0x00000002UL)

int llgo_windows_random(void *data, llgo_size_t size)
{
    llgo_module module;
    llgo_bcrypt_gen_random random;
    long status;

    if (data == 0 || size == 0 || size > UINT32_MAX)
        return 0;
    module = LoadLibraryA("bcrypt.dll");
    if (module == 0)
        return 0;
    random = (llgo_bcrypt_gen_random)GetProcAddress(module,
                                                    "BCryptGenRandom");
    if (random == 0) {
        FreeLibrary(module);
        return 0;
    }
    status = random(0, (unsigned char *)data, (llgo_dword)size,
                    LLGO_BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    FreeLibrary(module);
    return status == 0;
}
