//go:build !llgo

package build

import (
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/optlevel"
)

func TestShallowPanicTracebackBuildModes(t *testing.T) {
	for _, test := range []struct {
		name  string
		opt   optlevel.Level
		dwarf DWARFMode
	}{
		{name: "O0_DWARF", opt: optlevel.O0, dwarf: DWARFPreserve},
		{name: "O0_no_DWARF", opt: optlevel.O0, dwarf: DWARFOmit},
		{name: "O2_DWARF", opt: optlevel.O2, dwarf: DWARFPreserve},
		{name: "O2_no_DWARF", opt: optlevel.O2, dwarf: DWARFOmit},
	} {
		t.Run(test.name, func(t *testing.T) {
			name := "shallow-panic"
			if runtime.GOOS == "windows" {
				name += ".exe"
			}
			bin := filepath.Join(t.TempDir(), name)
			conf := NewDefaultConf(ModeBuild)
			conf.OptLevel = test.opt
			conf.LinkOptions.DWARF = test.dwarf
			conf.OutFile = bin
			if _, err := Do([]string{"./testdata/shallowpanic"}, conf); err != nil {
				t.Fatalf("build shallow panic fixture: %v", err)
			}
			output, err := exec.Command(bin).CombinedOutput()
			if err == nil {
				t.Fatalf("panic fixture unexpectedly succeeded:\n%s", output)
			}
			for _, want := range []string{
				"panic: shallow-panic",
				"goroutine 1 [running]:",
				"main.panicSite(",
				"main.main(",
			} {
				if !strings.Contains(string(output), want) {
					t.Fatalf("panic traceback is missing %q:\n%s", want, output)
				}
			}
		})
	}
}
