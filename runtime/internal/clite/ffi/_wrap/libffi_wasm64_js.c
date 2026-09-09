#ifndef __EMSCRIPTEN__
#error "wasm64 libffi wrap requires emcc so FFI_DEFAULT_ABI is WASM64_EMSCRIPTEN"
#endif

#include "../wasm64/include/ffi.h"
#include <stddef.h>

_Static_assert(sizeof(void *) == 8, "wasm64 wrap requires 64-bit pointers");
_Static_assert(FFI_DEFAULT_ABI == FFI_WASM64_EMSCRIPTEN,
               "LLGo's wasm DefaultAbi constant is stale");
_Static_assert(FFI_WASM64_EMSCRIPTEN == 2,
               "LLGo's wasm64 Emscripten ABI constant is stale");
/* LP64: pointers are 8 bytes, nfixedargs sits at offset 32, Extra overlays it. */
_Static_assert(offsetof(ffi_cif, nfixedargs) == 32,
               "LLGo ffi.Cif Extra does not match wasm64 nfixedargs");
_Static_assert(sizeof(ffi_cif) == 40,
               "LLGo ffi.Cif does not match wasm64 ffi_cif");

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
