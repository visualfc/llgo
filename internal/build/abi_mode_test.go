//go:build !llgo

package build

import (
	"fmt"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/cabi"
)

func TestLargeArrayReturnAllABIModes(t *testing.T) {
	for mode := cabi.ModeNone; mode <= cabi.ModeAllFunc; mode++ {
		t.Run(fmt.Sprintf("abi%d", mode), func(t *testing.T) {
			name := "large-array-return"
			if runtime.GOOS == "windows" {
				name += ".exe"
			}
			bin := filepath.Join(t.TempDir(), name)
			conf := NewDefaultConf(ModeBuild)
			conf.AbiMode = mode
			conf.OutFile = bin
			if _, err := Do([]string{"./testdata/largearrayreturn"}, conf); err != nil {
				t.Fatalf("build large-array fixture with ABI mode %d: %v", mode, err)
			}
			lines := strings.Split(strings.TrimSpace(runBinary(t, bin)), "\n")
			if len(lines) < 2 {
				t.Fatalf("output has fewer than two lines: %q", lines)
			}
			if got, want := strings.Join(lines[len(lines)-2:], "\n"), "99 0 0 0 98 98 6\n97"; got != want {
				t.Fatalf("output = %q, want %q", got, want)
			}
		})
	}
}
