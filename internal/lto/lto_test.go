package lto

import "testing"

func TestParse(t *testing.T) {
	tests := []struct {
		value string
		want  Mode
	}{
		{"thin", Thin},
		{"full", Full},
	}
	for _, tt := range tests {
		got, err := Parse(tt.value)
		if err != nil {
			t.Fatalf("Parse(%q) error: %v", tt.value, err)
		}
		if got != tt.want {
			t.Fatalf("Parse(%q) = %v, want %v", tt.value, got, tt.want)
		}
	}
}

func TestParseInvalid(t *testing.T) {
	for _, value := range []string{"", "true", "false", "off", "thinlto", "fulllto", "fat"} {
		if _, err := Parse(value); err == nil {
			t.Fatalf("Parse(%q) expected error", value)
		}
	}
}

func TestModeString(t *testing.T) {
	tests := []struct {
		mode Mode
		want string
	}{
		{Off, "off"},
		{Full, "full"},
		{Thin, "thin"},
		{Mode(99), "Mode(99)"},
	}
	for _, tt := range tests {
		if got := tt.mode.String(); got != tt.want {
			t.Fatalf("%v.String() = %q, want %q", tt.mode, got, tt.want)
		}
	}
}

func TestModeEnabled(t *testing.T) {
	tests := []struct {
		mode Mode
		want bool
	}{
		{Off, false},
		{Full, true},
		{Thin, true},
		{Mode(99), false},
	}
	for _, tt := range tests {
		if got := tt.mode.Enabled(); got != tt.want {
			t.Fatalf("%v.Enabled() = %t, want %t", tt.mode, got, tt.want)
		}
	}
}

func TestClangFlag(t *testing.T) {
	if got := Off.ClangFlag(); got != "" {
		t.Fatalf("Off.ClangFlag() = %q, want empty", got)
	}
	if got := Full.ClangFlag(); got != "-flto=full" {
		t.Fatalf("Full.ClangFlag() = %q, want -flto=full", got)
	}
	if got := Thin.ClangFlag(); got != "-flto=thin" {
		t.Fatalf("Thin.ClangFlag() = %q, want -flto=thin", got)
	}
}

func TestPassPluginLinkerFlags(t *testing.T) {
	if got, err := (PassPlugin{}).LinkerFlags("linux"); err != nil || got != nil {
		t.Fatalf("disabled plugin flags = %v, err = %v; want nil, nil", got, err)
	}

	if _, err := (PassPlugin{Path: "/tmp/libLLGOLTOPlugin.dylib"}).LinkerFlags("darwin"); err == nil {
		t.Fatal("darwin plugin flags expected error")
	}

	if (PassPlugin{}).Enabled() {
		t.Fatal("empty plugin should be disabled")
	}

	plugin := PassPlugin{Path: "/tmp/libLLGOLTOPlugin.so"}
	want := []string{"-Wl,--load-pass-plugin=/tmp/libLLGOLTOPlugin.so"}
	got, err := plugin.LinkerFlags("linux")
	if err != nil {
		t.Fatalf("LinkerFlags() error: %v", err)
	}
	if !sameStrings(got, want) {
		t.Fatalf("LinkerFlags() = %v, want %v", got, want)
	}
}

func sameStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
