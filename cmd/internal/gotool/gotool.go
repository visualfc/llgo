/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Package gotool locates the Go toolchain used by LLGo compatibility commands.
package gotool

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// Find returns a real Go executable from an absolute PATH entry. Links to the
// running LLGo executable are skipped so exposing LLGo as "go" cannot recurse.
func Find(self, pathEnv string) (string, error) {
	selfInfo, err := os.Stat(self)
	if err != nil {
		return "", err
	}
	name := executableName(runtime.GOOS)
	for _, dir := range filepath.SplitList(pathEnv) {
		if !filepath.IsAbs(dir) {
			continue
		}
		candidate, err := exec.LookPath(filepath.Join(dir, name))
		if err != nil {
			continue
		}
		info, err := os.Stat(candidate)
		if err == nil && !os.SameFile(selfInfo, info) {
			return candidate, nil
		}
	}
	return "", errors.New("Go toolchain not found in PATH (a go link to llgo is not a Go toolchain)")
}

func executableName(goos string) string {
	if goos == "windows" {
		return "go.exe"
	}
	return "go"
}
