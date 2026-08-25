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

func init() {
	// QueryPerformanceFrequency is fixed for the lifetime of the system.
	// Match Go's runtime startup model and keep the timestamp hot path to a
	// single QueryPerformanceCounter call.
	if c_nanotimeInit() == 0 {
		panic("runtime: QueryPerformanceFrequency failed")
	}
}

func nanotime1() int64 {
	return c_nanotime()
}
