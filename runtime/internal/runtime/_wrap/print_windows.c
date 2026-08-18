/* Runtime-owned stderr output must use Go's byte semantics. The Universal C
 * Runtime opens stderr in text mode, where fwrite/fputc turn LF into CRLF.
 * Bypass that translation without changing the FILE mode observed by C code. */
#include <stdint.h>

typedef __SIZE_TYPE__ llgo_size_t;
typedef unsigned long llgo_dword;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

__declspec(dllimport) void *LLGO_WINAPI GetStdHandle(llgo_dword handle);
__declspec(dllimport) int LLGO_WINAPI WriteFile(
    void *file, const void *buffer, llgo_dword size, llgo_dword *written,
    void *overlapped);

#define LLGO_STD_ERROR_HANDLE ((llgo_dword)-12)

void llgo_print_write(const void *data, llgo_size_t size)
{
    void *file = GetStdHandle(LLGO_STD_ERROR_HANDLE);
    const unsigned char *p = (const unsigned char *)data;

    if (file == 0 || file == (void *)(intptr_t)-1)
        return;
    while (size != 0) {
        llgo_dword chunk = size > UINT32_MAX ? UINT32_MAX : (llgo_dword)size;
        llgo_dword written = 0;
        if (!WriteFile(file, p, chunk, &written, 0) || written == 0)
            return;
        p += written;
        size -= written;
    }
}

void llgo_print_byte(unsigned char value)
{
    llgo_print_write(&value, 1);
}
