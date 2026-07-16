package flags

import (
	"bytes"
	"flag"
	"reflect"
	"strings"
	"testing"

	"github.com/goplus/llgo/cmd/internal/base"
	"github.com/goplus/llgo/internal/build"
	"github.com/goplus/llgo/internal/buildenv"
	"github.com/goplus/llgo/internal/lto"
	"github.com/goplus/llgo/internal/optlevel"
)

func TestApplyGoBuildFlags(t *testing.T) {
	cmd := new(base.Command)
	captured := base.PassBuildFlags(cmd)
	if err := cmd.Flag.Parse([]string{"-ldflags=-s -w", "-gcflags=all=-N", "."}); err != nil {
		t.Fatal(err)
	}

	conf := &build.Config{GoBuildFlags: []string{"-tags=existing"}}
	ApplyGoBuildFlags(conf, captured.Args)
	want := []string{"-tags=existing", "-ldflags=-s -w", "-gcflags=all=-N"}
	if !reflect.DeepEqual(conf.GoBuildFlags, want) {
		t.Fatalf("GoBuildFlags = %v, want %v", conf.GoBuildFlags, want)
	}
}

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

func TestBuildPthreadStackSizeFlag(t *testing.T) {
	tests := []struct {
		arg  string
		want int64
	}{
		{arg: "33554432", want: 33554432},
		{arg: "32MB", want: 32 * 1024 * 1024},
		{arg: "1024KB", want: 1024 * 1024},
		{arg: "1GiB", want: 1024 * 1024 * 1024},
		{arg: "0", want: 0},
	}

	for _, tt := range tests {
		t.Run(tt.arg, func(t *testing.T) {
			fs := flag.NewFlagSet("pthread-stack-size", flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse([]string{"-pthread-stack-size=" + tt.arg}); err != nil {
				t.Fatalf("Parse unexpected error: %v", err)
			}
			conf := &build.Config{}
			if err := UpdateConfig(conf); err != nil {
				t.Fatalf("UpdateConfig error: %v", err)
			}
			if conf.PthreadStackSize != tt.want {
				t.Fatalf("conf.PthreadStackSize = %d, want %d", conf.PthreadStackSize, tt.want)
			}
		})
	}
}

func TestBuildPthreadStackSizeFlagRejectsNegative(t *testing.T) {
	fs := flag.NewFlagSet("pthread-stack-size-negative", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse([]string{"-pthread-stack-size=-1"}); err == nil {
		t.Fatal("Parse expected error")
	}
}

func TestBuildLTOPassPluginFlags(t *testing.T) {
	fs := flag.NewFlagSet("lto-pass-plugin", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	args := []string{
		"-lto=full",
		"-lto-pass-plugin=/tmp/libLLGOLTOPlugin.so",
	}
	if err := fs.Parse(args); err != nil {
		t.Fatalf("Parse(%v) unexpected error: %v", args, err)
	}

	conf := &build.Config{}
	if err := UpdateConfig(conf); err != nil {
		t.Fatalf("UpdateConfig error: %v", err)
	}
	if conf.LTOPlugin.Path != "/tmp/libLLGOLTOPlugin.so" {
		t.Fatalf("conf.LTOPlugin.Path = %q", conf.LTOPlugin.Path)
	}
}

func TestBuildLTOPassPluginRequiresFullLTO(t *testing.T) {
	tests := [][]string{
		{"-lto-pass-plugin=/tmp/libLLGOLTOPlugin.so"},
		{"-lto=thin", "-lto-pass-plugin=/tmp/libLLGOLTOPlugin.so"},
	}

	for _, args := range tests {
		t.Run(strings.Join(args, " "), func(t *testing.T) {
			fs := flag.NewFlagSet("lto-pass-plugin-requires-fulllto", flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", args, err)
			}
			if err := UpdateConfig(&build.Config{}); err == nil {
				t.Fatal("UpdateConfig expected error")
			}
		})
	}
}

func TestDevLTOGlobalDCEBuildFlags(t *testing.T) {
	if !buildenv.Dev {
		fs := flag.NewFlagSet("nodev-globaldce", flag.ContinueOnError)
		fs.SetOutput(new(bytes.Buffer))
		AddBuildFlags(fs)
		if flag := fs.Lookup("globaldce"); flag != nil {
			t.Fatalf("non-dev build registered -globaldce flag: %v", flag)
		}
		if err := fs.Parse([]string{"-globaldce"}); err == nil {
			t.Fatal("non-dev build should reject -globaldce")
		}
		return
	}

	tests := []struct {
		name        string
		args        []string
		wantFlag    *bool
		wantDisable bool
	}{
		{name: "default auto", args: nil, wantFlag: nil, wantDisable: false},
		{name: "enabled implicit bool", args: []string{"-globaldce"}, wantFlag: boolPtr(true), wantDisable: false},
		{name: "enabled value", args: []string{"-globaldce=true"}, wantFlag: boolPtr(true), wantDisable: false},
		{name: "disabled value", args: []string{"-globaldce=false"}, wantFlag: boolPtr(false), wantDisable: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fs := flag.NewFlagSet(tt.name, flag.ContinueOnError)
			fs.SetOutput(new(bytes.Buffer))
			AddBuildFlags(fs)
			if err := fs.Parse(tt.args); err != nil {
				t.Fatalf("Parse(%v) unexpected error: %v", tt.args, err)
			}
			if !sameBoolPtr(GoGlobalDCE, tt.wantFlag) {
				t.Fatalf("GoGlobalDCE = %v, want %v", GoGlobalDCE, tt.wantFlag)
			}
			conf := &build.Config{LTO: lto.Full}
			if err := UpdateConfig(conf); err != nil {
				t.Fatalf("UpdateConfig error: %v", err)
			}
			if conf.DisableGoGlobalDCE != tt.wantDisable {
				t.Fatalf("conf.DisableGoGlobalDCE = %v, want %v", conf.DisableGoGlobalDCE, tt.wantDisable)
			}
		})
	}
}

func boolPtr(v bool) *bool {
	return &v
}

func sameBoolPtr(got, want *bool) bool {
	if got == nil || want == nil {
		return got == want
	}
	return *got == *want
}

func TestDevLTOGlobalDCEBuildFlagInvalid(t *testing.T) {
	fs := flag.NewFlagSet("invalid-globaldce", flag.ContinueOnError)
	fs.SetOutput(new(bytes.Buffer))
	AddBuildFlags(fs)
	if err := fs.Parse([]string{"-globaldce=maybe"}); err == nil {
		t.Fatal("Parse(-globaldce=maybe) expected error")
	}
}

func TestDevLTOGlobalDCEUpdateConfigRejectsWithoutFullLTO(t *testing.T) {
	if !buildenv.Dev {
		t.Skip("-globaldce is only registered in dev builds")
	}

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

func TestDevLTOGlobalDCEUpdateConfigAllowsDisableWithoutFullLTO(t *testing.T) {
	if !buildenv.Dev {
		t.Skip("-globaldce is only registered in dev builds")
	}

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
	if !conf.DisableGoGlobalDCE {
		t.Fatal("globaldce=false should set DisableGoGlobalDCE")
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
