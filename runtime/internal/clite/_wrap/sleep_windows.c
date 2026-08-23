/* Keep the Windows API calling convention behind a C bridge. On 386, direct
 * Go declarations use cdecl while Sleep is stdcall; the conventions coincide
 * only on 64-bit Windows. */
typedef unsigned long llgo_dword;

#if defined(_WIN64)
#define LLGO_WINAPI
#else
#define LLGO_WINAPI __attribute__((stdcall))
#endif

__declspec(dllimport) void LLGO_WINAPI Sleep(llgo_dword milliseconds);

void llgo_windows_sleep(llgo_dword milliseconds)
{
    Sleep(milliseconds);
}
