/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
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

// Package mod implements the "llgo mod" command.
package mod

import (
	"io"
	"os"

	"github.com/xgo-dev/llgo/cmd/internal/gocommand"
)

// Main delegates module management to Go, preserving its diagnostics and exit
// status. Module operations do not use LLGo-specific source-selection flags.
// In particular, Go's tidy/vendor scan all build tags except ignore, including
// dependencies imported only by llgo-tagged files; neither accepts -tags.
func Main(args []string) {
	gocommand.Exit("mod", run(args, os.Stdin, os.Stdout, os.Stderr))
}

func run(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	return (gocommand.Invocation{
		Command: "mod",
		Args:    args,
		Stdin:   stdin,
		Stdout:  stdout,
		Stderr:  stderr,
	}).Run()
}
