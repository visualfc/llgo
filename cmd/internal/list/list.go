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

// Package list implements the "llgo list" command.
package list

import (
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/xgo-dev/llgo/cmd/internal/gocommand"
)

// Main runs the list command with its original argument vector. Unlike normal
// LLGo commands, list must preserve flags owned by the underlying Go command.
func Main(args []string) {
	gocommand.Exit("list", run(args, os.Stdin, os.Stdout, os.Stderr))
}

func run(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	// Module queries do not select package source files, so omit LLGo's
	// implicit tags. Explicit target and user tags are still preserved.
	inv, err := gocommand.Build("list", args, !moduleMode(args))
	if err != nil {
		return err
	}
	inv.Stdin, inv.Stdout, inv.Stderr = stdin, stdout, stderr
	return inv.Run()
}

func moduleMode(args []string) bool {
	module := false
	for _, arg := range args {
		if arg == "--" {
			break
		}
		if arg == "-m" {
			module = true
		} else if value, ok := strings.CutPrefix(arg, "-m="); ok {
			// Match the boolean spellings accepted by Go flags. Invalid values
			// remain forwarded so the real go command emits its diagnostic.
			if enabled, err := strconv.ParseBool(value); err == nil {
				module = enabled
			}
		}
	}
	return module
}
