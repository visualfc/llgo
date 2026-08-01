//go:build darwin || linux

package plugin_test

import (
	"path/filepath"
	"plugin"
	"reflect"
	"strings"
	"testing"
)

func TestOpenMissingPlugin(t *testing.T) {
	path := filepath.Join(t.TempDir(), "missing.so")
	if _, err := plugin.Open(path); err == nil {
		t.Fatal("Open of a missing plugin succeeded")
	} else if !strings.Contains(err.Error(), "missing") {
		t.Fatalf("Open error %q does not identify the missing plugin", err)
	}
}

func TestPluginAPISurface(t *testing.T) {
	pluginType := reflect.TypeOf((*plugin.Plugin)(nil)).Elem()
	if pluginType.Name() != "Plugin" || pluginType.PkgPath() != "plugin" {
		t.Fatalf("unexpected Plugin type: %v from %q", pluginType, pluginType.PkgPath())
	}

	lookup := (*plugin.Plugin).Lookup
	if reflect.ValueOf(lookup).Pointer() == 0 {
		t.Fatal("Plugin.Lookup has no callable entry point")
	}

	var symbol plugin.Symbol = "llgo"
	if got, ok := symbol.(string); !ok || got != "llgo" {
		t.Fatalf("Symbol did not preserve its dynamic value: %#v", symbol)
	}
}
