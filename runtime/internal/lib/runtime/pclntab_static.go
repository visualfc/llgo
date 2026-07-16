//go:build !llgo_pclntab_external

package runtime

// Non-external builds have no sidecar path, probe string, filesystem call or
// loader state machine linked into the program. In embedded mode the main
// module initializes the tables directly; in none mode it initializes the
// same ABI globals to nil/zero.
func ensureRuntimePCLN() bool { return true }

func runtimePCLNReady() bool { return true }
