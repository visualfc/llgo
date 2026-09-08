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

package build

import "strings"

// Retain the previous 64 MiB total minus 10 MiB process-stack heap budget,
// without capping static data at the remainder of that total. wasm-ld adds this
// heap reservation after static data and the process stack, so a normal module
// initially has 64 MiB plus its static data (rounded to Wasm pages). This does
// not change the process stack or impose a maximum linear-memory size.
const defaultWASIHeapFlag = "-Wl,--initial-heap=56623104"

func defaultWASIHeapArgs(ctx *context, args []string) []string {
	if ctx == nil || ctx.buildConf == nil || ctx.buildConf.Goos != "wasip1" || ctx.buildConf.Goarch != "wasm" {
		return nil
	}
	// Wait until all flags, including package link directives and the linker
	// command prefix, are available. An explicit initial-memory or initial-heap
	// option owns the complete policy; adding a default heap alongside an exact
	// initial-memory size could make an otherwise valid user setting too small.
	// User-supplied response-file contents are left to the external driver and
	// are not expanded by this argument inspection.
	for _, arg := range ctx.linker().LinkArguments(args...) {
		for _, option := range strings.Split(strings.TrimPrefix(arg, "-Wl,"), ",") {
			if option == "--initial-memory" || strings.HasPrefix(option, "--initial-memory=") ||
				option == "--initial-heap" || strings.HasPrefix(option, "--initial-heap=") {
				return nil
			}
		}
	}
	return []string{defaultWASIHeapFlag}
}
