import { pathToFileURL } from "node:url";

import "./emscripten-node-polyfills.mjs";
import { runEmscriptenModule } from "./emscripten-exit-status.mjs";

if (process.argv.length < 3) {
	throw new Error("usage: node emscripten-runner.mjs <module.mjs> [arguments...]");
}

const loaded = await import(pathToFileURL(process.argv[2]));
if (typeof loaded.default !== "function") {
	throw new Error(`${process.argv[2]} does not export an Emscripten module factory`);
}
await runEmscriptenModule(loaded.default, {
	arguments: process.argv.slice(3),
	preRun: [module => {
		if (module.ENV != null) {
			Object.assign(module.ENV, process.env);
		}
	}],
});
