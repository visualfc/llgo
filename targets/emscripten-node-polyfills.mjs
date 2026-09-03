import fs from "node:fs";
import path from "node:path";

// Go's js/wasm syscall implementation reads these host objects during package
// initialization. wasm_exec.js normally supplies them; Emscripten owns the
// module bootstrap here, so expose Node's native implementations before the
// generated module is instantiated.
globalThis.fs ??= fs;
globalThis.path ??= path;
