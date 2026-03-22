package flags

import (
	"bytes"
	"flag"
	"strings"
	"testing"

	"github.com/goplus/llgo/internal/build"
	"github.com/goplus/llgo/internal/lto"
	"github.com/goplus/llgo/internal/optlevel"
)

func TestBuildOptimizationFlags(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want optlevel.Level
	}{
		{name: "O0 bool flag", args: []string{"-O0"}, want: optlevel.O0},
		{name: "O3 bool flag", args: []string{"-O3"}, want: optlevel.O3},
		{name: "Oz bool flag", args: []string{"-Oz"}, want: optlevel.Oz},
		{name: "O equals value", args: []string{"-O=s"}, want: optlevel.Os},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			OptLevel = optlevel.Unset
			fs := flag.NewFlagSet(tt.name, flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(tt.args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", tt.args, err)
			}
			if OptLevel != tt.want {
				t.Fatalf("OptLevel = %v, want %v", OptLevel, tt.want)
			}
		})
	}
}

func TestBuildOptimizationFlagInvalid(t *testing.T) {
	OptLevel = optlevel.Unset
	fs := flag.NewFlagSet("invalid", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse([]string{"-O=fast"}); err == nil {
		t.Fatal("Parse(-O=fast) expected error")
	}
}

func TestBuildOptimizationFlagsMutuallyExclusive(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "bool flags conflict", args: []string{"-O2", "-O3"}},
		{name: "bool and valued conflict", args: []string{"-O3", "-O=2"}},
		{name: "duplicate bool flag", args: []string{"-O2", "-O2"}},
		{name: "duplicate valued flag", args: []string{"-O=2", "-O=2"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			OptLevel = optlevel.Unset
			fs := flag.NewFlagSet(tt.name, flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(tt.args); err == nil {
				t.Fatalf("Parse(%v) expected conflict error", tt.args)
			}
		})
	}
}

func TestBuildLTOFlags(t *testing.T) {
	tests := []struct {
		name      string
		args      []string
		want      lto.Mode
		specified bool
	}{
		{name: "default off", args: nil, want: lto.Off, specified: false},
		{name: "thin value", args: []string{"-lto=thin"}, want: lto.Thin, specified: true},
		{name: "full value", args: []string{"-lto=full"}, want: lto.Full, specified: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fs := flag.NewFlagSet(tt.name, flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(tt.args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", tt.args, err)
			}
			if LTO.Specified != tt.specified {
				t.Fatalf("LTO.Specified = %v, want %v", LTO.Specified, tt.specified)
			}
			if LTO.Mode != tt.want {
				t.Fatalf("LTO.Mode = %v, want %v", LTO.Mode, tt.want)
			}
			conf := &build.Config{}
			if err := UpdateConfig(conf); err != nil {
				t.Fatalf("UpdateConfig error: %v", err)
			}
			if conf.LTO != tt.want {
				t.Fatalf("conf.LTO = %v, want %v", conf.LTO, tt.want)
			}
		})
	}
}

func TestBuildLTOFlagInvalid(t *testing.T) {
	tests := [][]string{
		{"-lto"},
		{"-lto=true"},
		{"-lto=false"},
		{"-lto=off"},
		{"-lto=fat"},
	}
	for _, args := range tests {
		fs := flag.NewFlagSet("invalid-lto", flag.ContinueOnError)
		fs.SetOutput(new(bytes.Buffer))
		AddBuildFlags(fs)
		if err := fs.Parse(args); err == nil {
			t.Fatalf("Parse(%v) expected error", args)
		}
	}
}

func TestBuildGlobalDCEFlags(t *testing.T) {
	tests := []struct {
		name      string
		args      []string
		want      bool
		specified bool
	}{
		{name: "default auto", args: nil, want: false, specified: false},
		{name: "enabled implicit bool", args: []string{"-globaldce"}, want: true, specified: true},
		{name: "enabled value", args: []string{"-globaldce=true"}, want: true, specified: true},
		{name: "disabled value", args: []string{"-globaldce=false"}, want: false, specified: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fs := flag.NewFlagSet(tt.name, flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(tt.args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", tt.args, err)
			}
			if GoGlobalDCE.Specified != tt.specified {
				t.Fatalf("GoGlobalDCE.Specified = %v, want %v", GoGlobalDCE.Specified, tt.specified)
			}
			if GoGlobalDCE.Enabled != tt.want {
				t.Fatalf("GoGlobalDCE.Enabled = %v, want %v", GoGlobalDCE.Enabled, tt.want)
			}
			conf := &build.Config{LTO: lto.Full}
			if err := UpdateConfig(conf); err != nil {
				t.Fatalf("UpdateConfig error: %v", err)
			}
			if conf.GoGlobalDCESpecified != tt.specified {
				t.Fatalf("conf.GoGlobalDCESpecified = %v, want %v", conf.GoGlobalDCESpecified, tt.specified)
			}
			if tt.specified && conf.GoGlobalDCE != tt.want {
				t.Fatalf("conf.GoGlobalDCE = %v, want %v", conf.GoGlobalDCE, tt.want)
			}
		})
	}
}

func TestBuildGlobalDCEFlagInvalid(t *testing.T) {
	fs := flag.NewFlagSet("invalid-globaldce", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse([]string{"-globaldce=maybe"}); err == nil {
		t.Fatal("Parse(-globaldce=maybe) expected error")
	}
}

func TestUpdateConfigRejectsGlobalDCEWithoutFullLTO(t *testing.T) {
	tests := [][]string{
		{"-globaldce"},
		{"-globaldce=true"},
		{"-lto=thin", "-globaldce"},
	}

	for _, args := range tests {
		t.Run(strings.Join(args, " "), func(t *testing.T) {
			fs := flag.NewFlagSet("globaldce-requires-fulllto", flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", args, err)
			}
			err := UpdateConfig(&build.Config{})
			if err == nil {
				t.Fatal("UpdateConfig expected error")
			}
			if !strings.Contains(err.Error(), "full LTO") {
				t.Fatalf("UpdateConfig error = %v, want full LTO error", err)
			}
		})
	}
}

func TestUpdateConfigAllowsGlobalDCEDisableWithoutFullLTO(t *testing.T) {
	fs := flag.NewFlagSet("globaldce-disabled", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse([]string{"-lto=thin", "-globaldce=false"}); err != nil {
		t.Fatalf("Parse unexpected error: %v", err)
	}
	conf := &build.Config{}
	if err := UpdateConfig(conf); err != nil {
		t.Fatalf("UpdateConfig unexpected error: %v", err)
	}
	if conf.LTO != lto.Thin {
		t.Fatalf("conf.LTO = %v, want %v", conf.LTO, lto.Thin)
	}
	if !conf.GoGlobalDCESpecified || conf.GoGlobalDCE {
		t.Fatalf("globaldce config = (%v,%v), want (true,false)", conf.GoGlobalDCESpecified, conf.GoGlobalDCE)
	}
}

func TestUpdateConfigPreservesLTOWhenUnspecified(t *testing.T) {
	fs := flag.NewFlagSet("lto-unspecified", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse(nil); err != nil {
		t.Fatalf("Parse(nil) unexpected error: %v", err)
	}

	conf := &build.Config{LTO: lto.Full}
	if err := UpdateConfig(conf); err != nil {
		t.Fatalf("UpdateConfig error: %v", err)
	}
	if conf.LTO != lto.Full {
		t.Fatalf("conf.LTO = %v, want %v", conf.LTO, lto.Full)
	}
}
