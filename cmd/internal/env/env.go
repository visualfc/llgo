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

// Package env implements the "llgo env" command.
package env

import (
	"io"
	"os"

	"github.com/xgo-dev/llgo/cmd/internal/base"
	"github.com/xgo-dev/llgo/cmd/internal/gocommand"
)

var Cmd = &base.Command{
	UsageLine: "llgo env [-json] [-changed] [-u] [-w] [-target name] [var ...]",
	Short:     "Print Go and LLGo environment information",
	Run:       runCmd,
}

func runCmd(_ *base.Command, args []string) {
	gocommand.Exit("env", run(args, os.Stdin, os.Stdout, os.Stderr))
}

func run(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	if extendedQuery(args) {
		return runExtended(args, stdin, stdout, stderr)
	}
	return runGo(args, stdin, stdout, stderr)
}

func runGo(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	// Delegate parsing and formatting to Go, including GOENV, GOTOOLCHAIN,
	// target overrides and -json/-changed/-w/-u. These describe the underlying
	// Go toolchain, not LLGo's LLVM backend or its cross-compilation targets.
	return (gocommand.Invocation{
		Command: "env",
		Args:    args,
		Stdin:   stdin,
		Stdout:  stdout,
		Stderr:  stderr,
	}).Run()
}
