#include <gc.h>

void llgo_GC_reachable(void *ptr) {
    GC_reachable_here(ptr);
}