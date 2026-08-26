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

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

//go:linkname c_memReadable C.llgo_mem_readable
func c_memReadable(p unsafe.Pointer) c.Int

func memReadable(addr uintptr) bool {
	return c_memReadable(unsafe.Pointer(addr)) != 0
}

// Windows hardware exceptions need a CONTEXT-aware exception handler. Until
// that backend is installed, explicit Go panic/recover still uses the normal
// runtime path and has no fault traceback to clear or print.
func clearFaultTraceback() {}

func faultTracebackActive() bool { return false }

func faultTraceback(skip int) bool { return false }
