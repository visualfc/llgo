#include <stdint.h>

/* volatile: a bare NULL store is UB and clang propagates it into
 * "unreachable", turning the recursion below into an infinite loop. */
static int32_t *volatile cexc_null;
volatile int32_t cexc_marks;
volatile int32_t cexc_den;

void cexc_leaf_segv(void) {
    *cexc_null = 42;
}

void cexc_mid_segv(int32_t depth) {
    if (depth > 0) {
        cexc_mid_segv(depth - 1);
        cexc_marks++;
        return;
    }
    cexc_leaf_segv();
    cexc_marks++;
}

void cexc_segv(int32_t depth) {
    cexc_mid_segv(depth);
    cexc_marks++;
}

int32_t cexc_div(int32_t den) {
    cexc_den = den;
    return 1000 / cexc_den;
}
