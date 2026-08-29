//go:build !llgo
// +build !llgo

package build

import (
	"testing"

	"github.com/xgo-dev/llgo/internal/cabi"
	llruntime "github.com/xgo-dev/llgo/runtime"
)

func TestInternalRuntimeSysUsesPlan9AsmWithoutAltPkg(t *testing.T) {
	conf := &Config{Goarch: "arm64", AbiMode: cabi.ModeAllFunc}
	if !plan9asmEnabledByDefault(conf, "internal/runtime/sys") {
		t.Fatal("plan9asm should be enabled by default for internal/runtime/sys on arm64")
	}
	if hasAltPkgForTarget(conf, "internal/runtime/sys") {
		t.Fatal("internal/runtime/sys should use its source patch instead of an alt package")
	}
}

func TestPlan9AsmDefaultsSupport386(t *testing.T) {
	conf := &Config{Goarch: "386", AbiMode: cabi.ModeAllFunc}
	for _, pkgPath := range []string{
		"internal/bytealg",
		"internal/chacha8rand",
		"internal/cpu",
		"internal/runtime/atomic",
		"internal/runtime/syscall/windows",
		"math",
	} {
		if !plan9asmEnabledByDefault(conf, pkgPath) {
			t.Errorf("plan9asm should be enabled by default for %s on 386", pkgPath)
		}
		if llruntime.SourcePatchReplacesAsmForGOARCH(pkgPath, "386") {
			t.Errorf("%s should retain the Go 386 assembly implementation", pkgPath)
		}
	}
	if !hasAltPkgForTarget(conf, "runtime") {
		t.Fatal("runtime should keep using the LLGo alternate package")
	}
}

func TestInternalRuntimeAtomicUsesSourcePatchOnArm(t *testing.T) {
	conf := &Config{Goarch: "arm", AbiMode: cabi.ModeAllFunc}
	if hasAltPkgForTarget(conf, "internal/runtime/atomic") {
		t.Fatal("internal/runtime/atomic should use its source patch on arm")
	}

	conf = &Config{Goarch: "arm64", AbiMode: cabi.ModeAllFunc}
	if hasAltPkgForTarget(conf, "internal/runtime/atomic") {
		t.Fatal("internal/runtime/atomic should keep plan9asm/std paths on arm64")
	}
}
