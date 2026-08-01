/* llgo_framepointer lives in the runtime core (not the public runtime
 * package): Recover() records the recovering frame through it, and
 * programs that never import "runtime" still link the core. */
__attribute__((noinline)) void *llgo_framepointer(void)
{
#if defined(__GNUC__) || defined(__clang__)
    /* Read the saved caller FP before this helper's frame becomes invalid. */
    void **frame = (void **)__builtin_frame_address(0);
    return frame ? *frame : 0;
#else
    return 0;
#endif
}
