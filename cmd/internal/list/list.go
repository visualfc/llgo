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

	"github.com/xgo-dev/llgo/cmd/internal/gocommand"
)

// Main runs the list command with its original argument vector. Unlike normal
// LLGo commands, list must preserve flags owned by the underlying Go command.
func Main(args []string) {
	gocommand.Exit("list", run(args, os.Stdin, os.Stdout, os.Stderr))
}

func run(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	inv, err := gocommand.Build("list", args)
	if err != nil {
		return err
	}
	inv.Stdin, inv.Stdout, inv.Stderr = stdin, stdout, stderr
	return inv.Run()
}
