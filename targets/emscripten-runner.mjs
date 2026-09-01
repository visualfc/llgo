import { pathToFileURL } from "node:url";

import "./emscripten-node-polyfills.mjs";

if (process.argv.length < 3) {
	throw new Error("usage: node emscripten-runner.mjs <module.mjs> [arguments...]");
}

const loaded = await import(pathToFileURL(process.argv[2]));
if (typeof loaded.default !== "function") {
	throw new Error(`${process.argv[2]} does not export an Emscripten module factory`);
}
let exitStatus = 0;
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
process.exitCode = exitStatus;
