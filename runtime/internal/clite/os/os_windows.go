//go:build windows

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

// Package os exposes the hosted runtime's operating-system C API.
package os

import (
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const (
	LLGoFiles   = "_os/os_windows.c"
	LLGoPackage = "link"
)

//go:linkname Getenv C.getenv
func Getenv(name *c.Char) *c.Char

// ExitProcess terminates the process and all of its threads.
//
//go:linkname ExitProcess C.llgo_windows_exit_process
func ExitProcess(code uint32)
