import { spawnSync } from "node:child_process";
import { fileURLToPath, pathToFileURL } from "node:url";

import "./emscripten-node-polyfills.mjs";

if (process.argv.length < 3) {
	throw new Error("usage: node emscripten-memory64-runner.mjs <module.mjs> [arguments...]");
}

// A memory section whose limits use the memory64 flag. Older Node releases
// expose this behind --experimental-wasm-memory64, while newer releases enable
// it by default and reject the obsolete command-line flag. Probe the engine so
// llgo run works with both families without imposing a Node-version-specific
// emulator command on every user.
const memory64Probe = new Uint8Array([
	0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
	0x05, 0x03, 0x01, 0x04, 0x01,
]);

if (!WebAssembly.validate(memory64Probe)) {
	if (process.env.LLGO_MEMORY64_NODE_RETRY === "1") {
		throw new Error("this Node release does not support WebAssembly Memory64");
	}
	const child = spawnSync(
		process.execPath,
		["--experimental-wasm-memory64", fileURLToPath(import.meta.url), ...process.argv.slice(2)],
		{
			stdio: "inherit",
			env: { ...process.env, LLGO_MEMORY64_NODE_RETRY: "1" },
		},
	);
	if (child.error) {
		throw child.error;
	}
	process.exit(child.status ?? 1);
}

const loaded = await import(pathToFileURL(process.argv[2]));
if (typeof loaded.default !== "function") {
	throw new Error(`${process.argv[2]} does not export an Emscripten module factory`);
}
let exitStatus = 0;
try {
	await loaded.default({
		arguments: process.argv.slice(3),
		onExit: status => {
			// emscripten_force_exit reports the fatal status before Asyncify unwinds,
			// but Emscripten may report a second, normal exit after the unwind. Keep
			// the failure so the runner preserves the program's process contract.
			if (status !== 0) {
				exitStatus = status;
			}
		},
		preRun: [module => {
			if (module.ENV != null) {
				Object.assign(module.ENV, process.env);
			}
		}],
	});
} catch (error) {
	// Emscripten 4.x can reject its ES-module factory with ExitStatus even
	// after onExit has reported an ordinary exit. Node 24.19 treats that
	// rejection as an uncaught exception unless the host consumes it.
	if (error?.name !== "ExitStatus" || !Number.isInteger(error.status)) {
		throw error;
	}
	if (error.status !== 0) {
		exitStatus = error.status;
	}
}
process.exitCode = exitStatus;
