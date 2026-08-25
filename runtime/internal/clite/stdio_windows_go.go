//go:build windows && !llgo

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

package c

const LLGoPackage = true

// The Go compiler cannot resolve LLGo's C.__acrt_iob_func linkname. Native Go
// tests only need the declarations to type-check; LLGo builds select
// stdio_windows.go and initialize these streams from the Universal CRT.
var (
	Stdin  FilePtr
	Stdout FilePtr
	Stderr FilePtr
)
