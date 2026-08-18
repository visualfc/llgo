//go:build windows

package plugin_test

import (
	"plugin"
	"strings"
	"testing"
)

func TestWindowsPluginStub(t *testing.T) {
	opened, err := plugin.Open("llgo-plugin-not-present")
	if opened != nil {
		t.Fatalf("Open returned plugin %v on unsupported Windows platform", opened)
	}
	if err == nil || !strings.Contains(err.Error(), "not implemented") {
		t.Fatalf("Open error = %v, want not implemented", err)
	}
}

func TestWindowsPluginSymbols(t *testing.T) {
	_ = t
	_ = plugin.Open
	_ = (*plugin.Plugin).Lookup
	var _ *plugin.Plugin
	var _ plugin.Symbol
}
