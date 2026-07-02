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

package pclnpost

import "testing"

func TestCanonicalOwner(t *testing.T) {
	elf := &binaryInfo{format: "elf"}
	macho := &binaryInfo{format: "macho"}
	id := fnv64("example.com/p.F")
	cases := []struct {
		info *binaryInfo
		name string
		want bool
	}{
		// ELF: symbol names are source-level.
		{elf, "example.com/p.F", true},
		{elf, "__llgo_stub.example.com/p.F", true},
		{elf, "example.com/p.G", false},
		// Mach-O: one C-mangling underscore, and debug/macho's suffix-shared
		// string table can surface one underscore more or less.
		{macho, "_example.com/p.F", true},
		{macho, "example.com/p.F", true},
		{macho, "___llgo_stub.example.com/p.F", true},
		{macho, "__llgo_stub.example.com/p.F", true},
		// An LTO inline copy: record id names F but the owner is the host.
		{macho, "_example.com/p.Host", false},
		{macho, "___llgo_stub.example.com/p.G", false},
	}
	for _, c := range cases {
		if got := canonicalOwner(c.info, c.name, id); got != c.want {
			t.Errorf("canonicalOwner(%s, %q) = %v, want %v", c.info.format, c.name, got, c.want)
		}
	}
}
