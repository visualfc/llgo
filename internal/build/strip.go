/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

func dwarfStripArgs(opts linkFlagOptions) []string {
	if !opts.EffectiveOmitDWARF() {
		return nil
	}
	return []string{"--strip-debug"}
}

func validateDwarfStripSupport(conf *Config, opts linkFlagOptions) error {
	if conf.Target != "" {
		if opts.omitDWARF.set && !opts.omitDWARF.value {
			return fmt.Errorf("-ldflags=-w=false is not supported with -target=%q because target linkers currently always suppress debug sections", conf.Target)
		}
		return nil
	}
	if !opts.EffectiveOmitDWARF() {
		return nil
	}
	if conf.BuildMode != BuildModeExe {
		return fmt.Errorf("DWARF stripping requested by -ldflags is not supported for -buildmode=%s", conf.BuildMode)
	}
	if conf.Goos != "darwin" && conf.Goos != "linux" {
		return fmt.Errorf("DWARF stripping requested by -ldflags is not supported for GOOS=%s", conf.Goos)
	}
	if conf.Goos == "darwin" && runtime.GOOS != "darwin" {
		return fmt.Errorf("DWARF stripping for GOOS=darwin requires a Darwin host for final code signing")
	}
	return nil
}

// stripLinkedDWARF removes any debug sections that reached the final artifact,
// including sections contributed by native objects. It deliberately runs after
// rewritePrebuiltFuncTab: the pclntab rewrite still needs the native symbol
// table, and --strip-debug leaves LLGo's runtime funcinfo/pcline payload intact.
//
// stripSymbols is parsed now so -s has Go's implied -w behavior. Removing the
// remaining native symbol table is a separate backend step tracked by #2111.
func stripLinkedDWARF(ctx *context, out string, verbose bool) error {
	args := dwarfStripArgs(ctx.linkFlags)
	if len(args) == 0 {
		return nil
	}
	if ctx.buildConf.BuildMode != BuildModeExe {
		return nil
	}
	if ctx.buildConf.Goos != "darwin" && ctx.buildConf.Goos != "linux" {
		return nil
	}

	// Embedded target linkers already receive -S and may produce a container
	// format that llvm-strip cannot rewrite in place.
	if ctx.buildConf.Target != "" || ctx.buildConf.Goarch == "wasm" {
		return nil
	}

	tool := filepath.Join(ctx.env.BinDir(), "llvm-strip")
	codeSignID := ""
	if ctx.buildConf.Goos == "darwin" && runtime.GOOS == "darwin" {
		codeSignID = codeSignIdentifier(out)
	}
	info, err := os.Stat(out)
	if err != nil {
		return fmt.Errorf("stat linked output %s: %w", out, err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(out), "."+filepath.Base(out)+".strip-*")
	if err != nil {
		return fmt.Errorf("create temporary stripped output: %w", err)
	}
	tmpPath := tmp.Name()
	if err := tmp.Close(); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("close temporary stripped output: %w", err)
	}
	defer os.Remove(tmpPath)

	args = append(args, "-o", tmpPath, out)
	if ctx.shouldPrintCommands(verbose) {
		fmt.Fprintln(os.Stderr, append([]string{tool}, args...))
	}
	cmd := exec.Command(tool, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("strip DWARF from %s: %w", out, err)
	}
	if err := os.Chmod(tmpPath, info.Mode()); err != nil {
		return fmt.Errorf("restore mode on stripped output %s: %w", out, err)
	}
	if ctx.buildConf.Goos == "darwin" && runtime.GOOS == "darwin" {
		if err := adHocCodeSign(ctx, tmpPath, codeSignID, verbose); err != nil {
			return err
		}
	}
	if err := os.Rename(tmpPath, out); err != nil {
		return fmt.Errorf("replace %s with stripped output: %w", out, err)
	}
	return nil
}

func codeSignIdentifier(out string) string {
	output, err := exec.Command("codesign", "-d", "--verbose=4", out).CombinedOutput()
	if err == nil {
		for _, line := range strings.Split(string(output), "\n") {
			if identifier, ok := strings.CutPrefix(line, "Identifier="); ok && identifier != "" {
				return identifier
			}
		}
	}
	return filepath.Base(out)
}

func adHocCodeSign(ctx *context, out, identifier string, verbose bool) error {
	args := []string{"-f", "-s", "-", "-i", identifier, out}
	if ctx.shouldPrintCommands(verbose) {
		fmt.Fprintln(os.Stderr, append([]string{"codesign"}, args...))
	}
	output, err := exec.Command("codesign", args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("ad-hoc sign stripped output %s: %w: %s", out, err, output)
	}
	return nil
}
