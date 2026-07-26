package runtime

import _ "unsafe"

type pluginInitTask struct{}

// LLGo does not yet integrate modules loaded by the Go plugin package with its
// runtime package metadata. Return a deterministic error instead of leaving
// the standard plugin package with unresolved runtime symbols.
//
//go:linkname plugin_lastmoduleinit plugin.lastmoduleinit
func plugin_lastmoduleinit() (string, map[string]any, []*pluginInitTask, string) {
	return "", nil, nil, "plugin loading is not supported by llgo"
}

//go:linkname runtime_doInit runtime.doInit
func runtime_doInit([]*pluginInitTask) {}
