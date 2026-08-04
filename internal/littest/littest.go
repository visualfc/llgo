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
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/goplus/llgo/internal/filecheck"
)

type Spec struct {
	Path string
}

const Marker = "LITTEST"

// ErrSpecNotFound reports that a package has no source file marked LITTEST.
var ErrSpecNotFound = errors.New("IR check spec not found")

func LoadSpec(pkgDir string) (Spec, error) {
	marked, ok, err := FindMarkedSourceFile(pkgDir)
	if err != nil {
		return Spec{}, err
	}
	if !ok {
		return Spec{}, fmt.Errorf("%w: %s", ErrSpecNotFound, pkgDir)
	}
	return Spec{Path: marked}, nil
}

func Check(spec Spec, actual string) error {
	return filecheck.Match(spec.Path, actual)
}

func FindMarkedSourceFile(dir string) (string, bool, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return "", false, err
	}
	var marked string
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		if !IsSourceSpecFile(name) {
			continue
		}
		path := filepath.Join(dir, name)
		ok, err := HasMarker(path)
		if err != nil {
			return "", false, err
		}
		if !ok {
			continue
		}
		if marked != "" {
			return "", false, fmt.Errorf("%s: multiple source lit specs found: %s, %s", dir, filepath.Base(marked), filepath.Base(path))
		}
		marked = path
	}
	if marked == "" {
		return "", false, nil
	}
	return marked, true, nil
}

func HasMarker(path string) (bool, error) {
	f, err := os.Open(path)
	if err != nil {
		return false, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	if !scanner.Scan() {
		return false, scanner.Err()
	}
	line := strings.TrimSpace(scanner.Text())
	if !strings.HasPrefix(line, "//") {
		return false, nil
	}
	return strings.TrimSpace(strings.TrimPrefix(line, "//")) == Marker, nil
}

func IsSourceSpecFile(name string) bool {
	return filepath.Ext(name) == ".go" && !strings.HasSuffix(name, "_test.go")
}
