package flags

import (
	"flag"
	"io"
	"testing"
)

func TestCoverageFlagImplications(t *testing.T) {
	for _, option := range []string{
		"-cover",
		"-covermode=set",
		"-covermode=count",
		"-covermode=atomic",
		"-covermode=",
		"-coverprofile=out",
		"-coverprofile=",
		"-coverpkg=./...",
		"-coverpkg=",
	} {
		t.Run(option, func(t *testing.T) {
			fs := flag.NewFlagSet("test", flag.ContinueOnError)
			AddTestBinaryFlags(fs)
			if err := fs.Parse([]string{option}); err != nil {
				t.Fatal(err)
			}
			if !Cover {
				t.Fatal("coverage was not enabled")
			}
		})
	}
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	AddTestBinaryFlags(fs)
	if err := fs.Parse([]string{"-covermode=unknown"}); err == nil {
		t.Fatal("accepted unknown coverage mode")
	}
}

func TestCoverageBuildFlags(t *testing.T) {
	for _, args := range [][]string{
		{"-cover"},
		{"-covermode", "atomic"},
		{"-coverpkg", "./..."},
	} {
		fs := flag.NewFlagSet("build", flag.ContinueOnError)
		AddCoverageFlags(fs)
		if err := fs.Parse(args); err != nil {
			t.Fatal(err)
		}
		if !Cover {
			t.Fatalf("coverage not enabled by %v", args)
		}
		if fs.Lookup("coverprofile") != nil {
			t.Fatal("build must not expose test-only -coverprofile")
		}
	}
	Cover, CoverMode, CoverPkg = false, "", ""
}
