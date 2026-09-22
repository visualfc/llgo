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
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"

	"github.com/xgo-dev/llgo/cmd/internal/gotool"
	"github.com/xgo-dev/llgo/internal/mockable"
)

// Main delegates module management to Go, preserving its diagnostics and exit
// status. Module operations do not use LLGo-specific source-selection flags.
// In particular, Go's tidy/vendor scan all build tags except ignore, including
// dependencies imported only by llgo-tagged files; neither accepts -tags.
func Main(args []string) {
	if err := run(args, os.Stdin, os.Stdout, os.Stderr); err != nil {
		var exit *exec.ExitError
		if errors.As(err, &exit) && exit.ExitCode() > 0 {
			// Go has already written its diagnostic to stderr.
			mockable.Exit(exit.ExitCode())
		} else {
			fmt.Fprintln(os.Stderr, "llgo mod:", err)
			mockable.Exit(1)
		}
	}
}

func run(args []string, stdin io.Reader, stdout, stderr io.Writer) error {
	self, err := os.Executable()
	if err != nil {
		return err
	}
	goExe, err := gotool.Find(self, os.Getenv("PATH"))
	if err != nil {
		return err
	}
	// Keep the working directory and environment, including module/toolchain
	// settings. Find and ChildEnv prevent recursion when "go" points to LLGo.
	cmd := exec.Command(goExe, append([]string{"mod"}, args...)...)
	cmd.Stdin, cmd.Stdout, cmd.Stderr = stdin, stdout, stderr
	cmd.Env = gotool.ChildEnv(os.Environ())
	return cmd.Run()
}
