#include <stdint.h>

extern char *getenv(const char *name);

uintptr_t llgo_windows_invalid_address(void)
{
    return 1;
}

int llgo_windows_unrecovered_fault(void)
{
    return getenv("LLGO_TEST_UNRECOVERED_FAULT") != 0;
}
