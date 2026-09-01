export async function runEmscriptenModule(factory, options) {
	let exitStatus = 0;

	const recordExit = status => {
		// A fatal exit may be followed by a normal exit while Asyncify unwinds.
		// Preserve the first nonzero status as the process result.
		if (status !== 0) {
			exitStatus = status;
		}
		process.exitCode = exitStatus;
	};
	const consumeExitStatus = error => {
		if (error?.name !== "ExitStatus" || !Number.isInteger(error.status)) {
			return false;
		}
		recordExit(error.status);
		return true;
	};
	const uninstall = () => {
		process.off("uncaughtException", handleUncaughtException);
		process.off("unhandledRejection", handleUnhandledRejection);
	};
	const handleUncaughtException = error => {
		if (!consumeExitStatus(error)) {
			uninstall();
			throw error;
		}
	};
	const handleUnhandledRejection = reason => {
		if (!consumeExitStatus(reason)) {
			uninstall();
			throw reason;
		}
	};

	// A short program rejects the module factory directly. With Asyncify, the
	// same ExitStatus can arrive after that factory has already resolved, either
	// as an uncaught exception or an unhandled rejection. Keep these handlers
	// installed through process shutdown so all three paths share one contract.
	process.on("uncaughtException", handleUncaughtException);
	process.on("unhandledRejection", handleUnhandledRejection);

	try {
		await factory({ ...options, onExit: recordExit });
	} catch (error) {
		if (!consumeExitStatus(error)) {
			uninstall();
			throw error;
		}
	}
	process.exitCode = exitStatus;
}
