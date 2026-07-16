//go:build !llgo

package run

import (
	"reflect"
	"testing"

	"github.com/goplus/llgo/cmd/internal/base"
	"github.com/goplus/llgo/internal/build"
)

func TestPassGoBuildFlags(t *testing.T) {
	cmd := new(base.Command)
	goBuildFlags := base.PassBuildFlags(cmd)
	if err := cmd.Flag.Parse([]string{"-ldflags=-s -w", "-gcflags=all=-N", "."}); err != nil {
		t.Fatal(err)
	}

	conf := &build.Config{GoBuildFlags: []string{"-tags=existing"}}
	passGoBuildFlags(conf, goBuildFlags)
	want := []string{"-tags=existing", "-ldflags=-s -w", "-gcflags=all=-N"}
	if !reflect.DeepEqual(conf.GoBuildFlags, want) {
		t.Fatalf("GoBuildFlags = %v, want %v", conf.GoBuildFlags, want)
	}
}

func TestRunAndCmpTestBuildFlagsAreIndependent(t *testing.T) {
	if runGoBuildFlags == cmpTestGoBuildFlags {
		t.Fatal("run and cmptest share a build-flag collector")
	}
	if Cmd.Flag.Lookup("ldflags") == nil || CmpTestCmd.Flag.Lookup("ldflags") == nil {
		t.Fatal("run or cmptest does not register -ldflags")
	}
}
