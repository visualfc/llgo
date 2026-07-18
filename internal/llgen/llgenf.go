/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
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

package llgen

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/goplus/llgo/internal/build"
	"github.com/goplus/llgo/internal/goflags"
)

func GenFrom(fileOrPkg string) string {
	pkg, err := genFrom(fileOrPkg, 0)
	check(err)
	out := pkg.LPkg.String()
	// Release the compile's LLVM context: golden suites call GenFrom for
	// every test dir inside one process, and undisposed contexts
	// accumulate C++-side memory for the whole run.
	pkg.LPkg.Prog.Dispose()
	return out
}

func genFrom(pkgPath string, abiMode build.AbiMode) (build.Package, error) {
	conf := &build.Config{
		Mode:    build.ModeGen,
		AbiMode: abiMode,
		GenLL:   true,
	}
	if err := applyGoBuildFlagsFile(conf, filepath.Join(pkgPath, "flags.txt")); err != nil {
		return nil, err
	}
	pkgs, err := build.Do([]string{pkgPath}, conf)
	if err != nil {
		return nil, err
	}
	return pkgs[0], nil
}

func DoFile(fileOrPkg, outFile string) {
	ret := GenFrom(fileOrPkg)
	err := os.WriteFile(outFile, []byte(ret), 0644)
	check(err)
}

func readGoBuildFlags(flagsFile string) ([]string, error) {
	data, err := os.ReadFile(flagsFile)
	if os.IsNotExist(err) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	flags, err := goflags.ParseFlagFile(string(data))
	if err != nil {
		return nil, fmt.Errorf("parse %s: %w", flagsFile, err)
	}
	return flags, nil
}

func applyGoBuildFlagsFile(conf *build.Config, flagsFile string) error {
	flags, err := readGoBuildFlags(flagsFile)
	if err != nil {
		return err
	}
	if err := goflags.ApplyBuildFlags(conf, flags); err != nil {
		return fmt.Errorf("apply %s: %w", flagsFile, err)
	}
	return nil
}

func SmartDoFile(pkgPath string) {
	SmartDoFileEx(pkgPath, 0)
}

func SmartDoFileEx(pkgPath string, abiMode build.AbiMode) {
	pkg, err := genFrom(pkgPath, abiMode)
	check(err)

	const autgenFile = "llgo_autogen.ll"
	dir, _ := filepath.Split(pkg.GoFiles[0])
	absDir, _ := filepath.Abs(dir)
	absDir = filepath.ToSlash(absDir)
	fname := autgenFile
	if inCompilerDir(absDir) {
		fname = "out.ll"
	}
	outFile := dir + fname

	b, err := os.ReadFile(outFile)
	if err == nil && len(b) == 1 && b[0] == ';' {
		return // skip to gen
	}

	if err = os.WriteFile(outFile, []byte(pkg.LPkg.String()), 0644); err != nil {
		panic(err)
	}
	if false && fname == autgenFile {
		genZip(absDir, "llgo_autogen.lla", autgenFile)
	}
}

func genZip(dir string, outFile, inFile string) {
	cmd := exec.Command("zip", outFile, inFile)
	cmd.Dir = dir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Run()
}

func inCompilerDir(dir string) bool {
	return strings.Contains(dir, "/cl/_test")
}
