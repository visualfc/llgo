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

// Package lldb implements the "llgo lldb" command.
package lldb

import (
	_ "embed"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"github.com/goplus/llgo/cmd/internal/base"
	"github.com/goplus/llgo/internal/mockable"
)

const minimumLLDBVersion = 18

var (
	//go:embed llgo_plugin.py
	pluginSource []byte

	lldbVersionPattern = regexp.MustCompile(`(?i)\blldb(?:\s+version|-)?\s*([0-9]+)`)
	lldbPath           string
)

// Cmd is the llgo lldb command.
var Cmd = &base.Command{
	UsageLine: "llgo lldb [-lldb path] [--] executable [lldb arguments...]",
	Short:     "Debug an LLGo executable with LLDB",
}

func init() {
	Cmd.Run = runCmd
	Cmd.Flag.StringVar(&lldbPath, "lldb", "", "path to LLDB 18 or newer (default $LLGO_LLDB or auto-detect)")
}

func runCmd(cmd *base.Command, args []string) {
	if err := cmd.Flag.Parse(args); err != nil {
		mockable.Exit(2)
		return
	}
	if err := run(lldbPath, cmd.Flag.Args(), os.Stdin, os.Stdout, os.Stderr); err != nil {
		fmt.Fprintln(os.Stderr, err)
		mockable.Exit(1)
	}
}

func run(configuredPath string, args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	if len(args) == 0 {
		return errors.New("llgo lldb: no executable specified")
	}

	path, err := findLLDB(configuredPath)
	if err != nil {
		return err
	}

	pluginDir, err := os.MkdirTemp("", "llgo-lldb-")
	if err != nil {
		return fmt.Errorf("llgo lldb: create plugin directory: %w", err)
	}
	defer os.RemoveAll(pluginDir)

	pluginPath := filepath.Join(pluginDir, "llgo_plugin.py")
	if err := os.WriteFile(pluginPath, pluginSource, 0600); err != nil {
		return fmt.Errorf("llgo lldb: write plugin: %w", err)
	}

	lldbArgs := make([]string, 0, len(args)+2)
	lldbArgs = append(lldbArgs, "-O", lldbImportCommand(pluginPath))
	lldbArgs = append(lldbArgs, args...)

	command := exec.Command(path, lldbArgs...)
	command.Stdin = stdin
	command.Stdout = stdout
	command.Stderr = stderr
	if err := command.Run(); err != nil {
		return fmt.Errorf("llgo lldb: %w", err)
	}
	return nil
}

func findLLDB(configuredPath string) (string, error) {
	return findLLDBFrom(configuredPath, os.Getenv("LLGO_LLDB"), []string{
		"/opt/homebrew/bin/lldb",
		"/usr/local/bin/lldb",
		"/usr/bin/lldb",
		"lldb",
	})
}

func findLLDBFrom(configuredPath, environmentPath string, candidates []string) (string, error) {
	if configuredPath != "" {
		return validateLLDB(configuredPath)
	}
	if environmentPath != "" {
		return validateLLDB(environmentPath)
	}

	seen := make(map[string]bool)
	for _, candidate := range candidates {
		path, err := exec.LookPath(candidate)
		if err != nil || seen[path] {
			continue
		}
		seen[path] = true
		if path, err = validateLLDB(path); err == nil {
			return path, nil
		}
	}
	return "", fmt.Errorf("llgo lldb: LLDB %d or newer not found; install LLDB or set LLGO_LLDB", minimumLLDBVersion)
}

func validateLLDB(name string) (string, error) {
	path, err := exec.LookPath(name)
	if err != nil {
		return "", fmt.Errorf("llgo lldb: find %q: %w", name, err)
	}
	output, err := exec.Command(path, "--version").CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("llgo lldb: query %q version: %w", path, err)
	}
	major, ok := parseLLDBMajor(string(output))
	if !ok {
		return "", fmt.Errorf("llgo lldb: cannot parse LLDB version from %q", strings.TrimSpace(string(output)))
	}
	if major < minimumLLDBVersion {
		return "", fmt.Errorf("llgo lldb: %q is LLDB %d; version %d or newer is required", path, major, minimumLLDBVersion)
	}
	return path, nil
}

func parseLLDBMajor(version string) (int, bool) {
	match := lldbVersionPattern.FindStringSubmatch(version)
	if len(match) != 2 {
		return 0, false
	}
	major, err := strconv.Atoi(match[1])
	return major, err == nil
}

func lldbImportCommand(path string) string {
	path = strings.NewReplacer(`\`, `\\`, `"`, `\"`).Replace(path)
	return `command script import "` + path + `"`
}
