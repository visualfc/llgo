package runtime

// zerobase is the shared, non-nil address returned for zero-sized allocations.
// It is static storage, not a heap object, and must never be freed.
var zerobase uintptr
