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

package littest

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/xgo-dev/llgo/internal/filecheck"
)

type Spec struct {
	Path    string
	PostABI bool
}

const (
	Marker        = "LITTEST"
	PostABIMarker = "LITTEST: POST-ABI"
)

func LoadSpec(pkgDir string) (Spec, error) {
	spec, ok, err := FindSpec(pkgDir)
	if err != nil {
		return Spec{}, err
	}
	if !ok {
		return Spec{}, fmt.Errorf("%s: missing // %s source lit spec", pkgDir, Marker)
	}
	return spec, nil
}

func Check(spec Spec, actual string, targetPrefixes ...string) error {
	if len(targetPrefixes) == 0 {
		return filecheck.Match(spec.Path, actual)
	}
	return filecheck.MatchWithTargetPrefixes(spec.Path, actual, targetPrefixes...)
}

func FindMarkedSourceFile(dir string) (string, bool, error) {
	spec, ok, err := FindSpec(dir)
	return spec.Path, ok, err
}

// FindSpec finds the source-embedded IR check in dir without requiring one.
func FindSpec(dir string) (Spec, bool, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return Spec{}, false, err
	}
	var spec Spec
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		if !IsSourceSpecFile(name) {
			continue
		}
		path := filepath.Join(dir, name)
		candidatePostABI, ok, err := ReadMarker(path)
		if err != nil {
			return Spec{}, false, err
		}
		if !ok {
			continue
		}
		if spec.Path != "" {
			return Spec{}, false, fmt.Errorf("%s: multiple source lit specs found: %s, %s", dir, filepath.Base(spec.Path), filepath.Base(path))
		}
		spec = Spec{Path: path, PostABI: candidatePostABI}
	}
	if spec.Path == "" {
		return Spec{}, false, nil
	}
	return spec, true, nil
}

func HasMarker(path string) (bool, error) {
	_, ok, err := ReadMarker(path)
	return ok, err
}

// ReadMarker reports whether the source's first-line marker selects post-ABI IR.
// Plain // LITTEST retains the existing check behavior; the POST-ABI form is
// an explicit opt-in to target-ABI-lowered, pre-optimization IR.
func ReadMarker(path string) (postABI, found bool, err error) {
	f, err := os.Open(path)
	if err != nil {
		return false, false, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	if !scanner.Scan() {
		return false, false, scanner.Err()
	}
	line := strings.TrimSpace(scanner.Text())
	if !strings.HasPrefix(line, "//") {
		return false, false, nil
	}
	switch strings.TrimSpace(strings.TrimPrefix(line, "//")) {
	case Marker:
		return false, true, nil
	case PostABIMarker:
		return true, true, nil
	default:
		return false, false, nil
	}
}

func IsSourceSpecFile(name string) bool {
	return filepath.Ext(name) == ".go" && !strings.HasSuffix(name, "_test.go")
}
