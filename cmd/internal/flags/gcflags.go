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

package flags

import (
	"strings"

	"github.com/goplus/llgo/internal/build"
	"github.com/goplus/llgo/internal/optlevel"
)

// applyFrontendGCFlags maps the supported Go compiler flag subset to typed
// frontend configuration. Raw flags remain in GoBuildFlags for go/packages.
func applyFrontendGCFlags(conf *build.Config) {
	for _, buildFlag := range conf.GoBuildFlags {
		value, ok := strings.CutPrefix(buildFlag, "-gcflags=")
		if !ok {
			continue
		}
		fields := strings.Fields(value)
		if len(fields) != 0 {
			if _, compilerFlags, hasPattern := strings.Cut(fields[0], "="); hasPattern {
				fields[0] = compilerFlags
			}
		}
		for _, compilerFlag := range fields {
			switch {
			case strings.HasPrefix(compilerFlag, "-lang="):
				conf.GoVersion = strings.TrimPrefix(compilerFlag, "-lang=")
			case compilerFlag == "-N", compilerFlag == "-l", strings.HasPrefix(compilerFlag, "-l="):
				conf.OptLevel = optlevel.O0
			}
		}
	}
}
