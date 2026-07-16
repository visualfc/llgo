//go:build !llgo

package install

import (
	"reflect"
	"testing"

	"github.com/goplus/llgo/cmd/internal/base"
	"github.com/goplus/llgo/internal/build"
)

func TestPassGoBuildFlags(t *testing.T) {
	cmd := new(base.Command)
	goBuildFlags := base.PassBuildFlags(cmd)
	if err := cmd.Flag.Parse([]string{"-ldflags=-s -w", "."}); err != nil {
		t.Fatal(err)
	}

	conf := &build.Config{GoBuildFlags: []string{"-tags=existing"}}
	passGoBuildFlags(conf, goBuildFlags)
	want := []string{"-tags=existing", "-ldflags=-s -w"}
	if !reflect.DeepEqual(conf.GoBuildFlags, want) {
		t.Fatalf("GoBuildFlags = %v, want %v", conf.GoBuildFlags, want)
	}
}
