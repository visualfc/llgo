import { pathToFileURL } from "node:url";

if (process.argv.length !== 3) {
	throw new Error("usage: node emscripten-runner.mjs <module.mjs>");
}

const loaded = await import(pathToFileURL(process.argv[2]));
if (typeof loaded.default !== "function") {
	throw new Error(`${process.argv[2]} does not export an Emscripten module factory`);
}
await loaded.default();
