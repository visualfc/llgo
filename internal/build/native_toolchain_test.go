//go:build !llgo

package build

import (
	"bytes"
	"fmt"
	"os"
	"reflect"
	"slices"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/crosscompile"
)

func TestParseNativeToolchainInput(t *testing.T) {
	commands := commandEnv{
		dir: "/work",
		environ: []string{
			"CC=old-clang",
			`CC=clang '--target=x86_64-pc-windows-msvc'`,
			`CXX="C:/Program Files/LLVM/bin/clang++.exe" --target=x86_64-pc-windows-msvc`,
		},
	}
	options := LinkOptions{
		ExternalLinker:      `clang "--target=x86_64-pc-windows-msvc"`,
		ExternalLinkerFlags: `'/debug:dwarf' /incremental:no`,
	}
	got, err := parseNativeToolchainInput(commands, options)
	if err != nil {
		t.Fatal(err)
	}
	if got.Dir != commands.dir || !reflect.DeepEqual(got.Environ, commands.environ) {
		t.Fatalf("invocation state = dir %q env %q", got.Dir, got.Environ)
	}
	for name, values := range map[string]struct {
		got  []string
		want []string
	}{
		"CC":          {got.CC, []string{"clang", "--target=x86_64-pc-windows-msvc"}},
		"CXX":         {got.CXX, []string{"C:/Program Files/LLVM/bin/clang++.exe", "--target=x86_64-pc-windows-msvc"}},
		"-extld":      {got.ExternalLinker, []string{"clang", "--target=x86_64-pc-windows-msvc"}},
		"-extldflags": {got.ExternalFlags, []string{"/debug:dwarf", "/incremental:no"}},
	} {
		if !reflect.DeepEqual(values.got, values.want) {
			t.Errorf("%s = %q, want %q", name, values.got, values.want)
		}
	}

	got.Environ[0] = "changed"
	if commands.environ[0] == "changed" {
		t.Fatal("parseNativeToolchainInput aliased the invocation environment")
	}
}

func TestParseNativeToolchainInputErrors(t *testing.T) {
	for _, test := range []struct {
		name    string
		environ []string
		options LinkOptions
		wantErr string
	}{
		{name: "unterminated CC", environ: []string{"CC='clang"}, wantErr: "could not parse CC"},
		{name: "empty CXX", environ: []string{"CXX=  "}, wantErr: "CXX requires a non-empty command"},
		{name: "unterminated extld", options: LinkOptions{ExternalLinker: `'clang`}, wantErr: "could not parse -extld"},
		{name: "unterminated extldflags", options: LinkOptions{ExternalLinkerFlags: `'/debug`}, wantErr: "could not parse -extldflags"},
	} {
		t.Run(test.name, func(t *testing.T) {
			_, err := parseNativeToolchainInput(commandEnv{environ: test.environ}, test.options)
			if err == nil || !strings.Contains(err.Error(), test.wantErr) {
				t.Fatalf("error = %v, want substring %q", err, test.wantErr)
			}
		})
	}
}

func TestParseNativeToolchainInputAcceptsEmptyLinkerFlags(t *testing.T) {
	got, err := parseNativeToolchainInput(commandEnv{}, LinkOptions{
		ExternalLinker:      "  ",
		ExternalLinkerFlags: "\t",
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(got.ExternalLinker) != 0 || len(got.ExternalFlags) != 0 {
		t.Fatalf("external inputs = %q, %q; want empty", got.ExternalLinker, got.ExternalFlags)
	}
}

func TestCommandEnvLookupUsesLastValue(t *testing.T) {
	environ := []string{"CC=first", "malformed", "CXX=clang++", "CC=last"}
	commands := commandEnv{environ: environ}
	if got := commands.lookup("CC"); got != "last" {
		t.Fatalf("lookup(CC) = %q, want last", got)
	}
	if got := commands.lookup("CXX"); got != "clang++" {
		t.Fatalf("lookup(CXX) = %q, want clang++", got)
	}
	if got := commands.lookup("MISSING"); got != "" {
		t.Fatalf("lookup(MISSING) = %q", got)
	}
}

func TestNativeToolchainCommands(t *testing.T) {
	if os.Getenv("LLGO_NATIVE_COMMAND_HELPER") == "1" {
		separator := slices.Index(os.Args, "--")
		if separator < 0 {
			os.Exit(2)
		}
		_, _ = fmt.Fprint(os.Stdout, strings.Join(os.Args[separator+1:], " "))
		os.Exit(0)
	}

	helper := func(kind string) []string {
		return []string{os.Args[0], "-test.run=^TestNativeToolchainCommands$", "--", kind}
	}
	cc := helper("cc")
	cxx := helper("cxx")
	ctx := &context{
		buildConf: &Config{},
		crossCompile: crosscompile.Export{
			CC:      cc[0],
			CCArgs:  cc[1:],
			CXX:     cxx[0],
			CXXArgs: cxx[1:],
		},
		commands: commandEnv{
			dir:     t.TempDir(),
			environ: append(os.Environ(), "LLGO_NATIVE_COMMAND_HELPER=1"),
		},
	}
	run := func(name, want string, command func(*bytes.Buffer) error) {
		t.Helper()
		t.Run(name, func(t *testing.T) {
			var output bytes.Buffer
			if err := command(&output); err != nil {
				t.Fatal(err)
			}
			if got := output.String(); got != want {
				t.Fatalf("command arguments = %q, want %q", got, want)
			}
		})
	}
	run("C source", "cc input", func(output *bytes.Buffer) error {
		cmd := ctx.compilerForSource("source.c")
		cmd.Stdout, cmd.Stderr = output, output
		return cmd.Compile("input")
	})
	run("C++ source", "cxx input", func(output *bytes.Buffer) error {
		cmd := ctx.compilerForSource("source.cpp")
		cmd.Stdout, cmd.Stderr = output, output
		return cmd.Compile("input")
	})
	run("default linker", "cxx input", func(output *bytes.Buffer) error {
		cmd := ctx.linker()
		cmd.Stdout, cmd.Stderr = output, output
		return cmd.Link("input")
	})

	extld := helper("extld")
	ctx.crossCompile.Linker = extld[0]
	ctx.crossCompile.LinkerArgs = extld[1:]
	run("explicit external linker", "extld input", func(output *bytes.Buffer) error {
		cmd := ctx.linker()
		cmd.Stdout, cmd.Stderr = output, output
		return cmd.Link("input")
	})
}
