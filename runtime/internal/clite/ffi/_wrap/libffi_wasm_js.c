#ifndef __EMSCRIPTEN__
#error "wasm32 libffi wrap requires emcc so FFI_DEFAULT_ABI is WASM32_EMSCRIPTEN"
#endif

#include "../wasm32/include/ffi.h"

_Static_assert(FFI_DEFAULT_ABI == FFI_WASM32_EMSCRIPTEN,
               "LLGo's wasm DefaultAbi constant is stale");
_Static_assert(FFI_WASM32_EMSCRIPTEN == 2,
               "LLGo's wasm32 Emscripten ABI constant is stale");
/* wasm32 pointers are 4 bytes, matching unsigned, so Go's ffi.Cif Extra field
 * overlays nfixedargs. Memory64 is compiled against the stub, not this wrap. */
_Static_assert(sizeof(ffi_cif) == 7 * sizeof(unsigned),
               "LLGo ffi.Cif does not match wasm32 ffi_cif");

void *llgo_ffi_closure_alloc(void **code) {
  return ffi_closure_alloc(sizeof(ffi_closure), code);
}

/* Wasm has no hidden nest/swiftself register. CallWithEnv on this target
 * already prepends env to the CIF and argument list, so the C hop is ffi_call. */
void llgo_ffi_call_with_env(ffi_cif *cif, void (*fn)(void), void *rvalue,
                            void **avalue, void *env) {
  (void)env;
  ffi_call(cif, fn, rvalue, avalue);
}
