# Vendored wasm32 libffi

Prebuilt static archive for `js/wasm` Emscripten. WASI and Emscripten
Memory64 keep the C stub in `_wrap/libffi_wasm_stub.c`.

- Upstream: [libffi](https://sourceware.org/libffi/) 3.8.0 (`include/ffi.h`)
- Target: `wasm32-unknown-emscripten`
- Archive SHA-256: `0c430418bf3ea946b550aa81a1c962deb1f2e9a5b162d9134f9931545d60d0e7`
- License: MIT, as in `include/ffi.h`

Rebuild with the Emscripten SDK, then replace `libffi.a` and the generated
headers:

```sh
emconfigure ./configure --host=wasm32-unknown-emscripten --enable-static --disable-shared
emmake make
cp include/ffi.h include/ffitarget.h "$LLGO_ROOT/runtime/internal/clite/ffi/wasm32/include/"
cp .libs/libffi.a "$LLGO_ROOT/runtime/internal/clite/ffi/wasm32/libffi.a"
```
