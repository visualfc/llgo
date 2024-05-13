/*
 * Copyright (c) 2024 The GoPlus Authors (goplus.org). All rights reserved.
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

package main

import (
	"github.com/goplus/llgo/c"
	"github.com/goplus/llgo/py"
	"github.com/goplus/llgo/py/inspect"
)

func main() {
	if c.Argc < 2 {
		c.Fprintf(c.Stderr, c.Str("Usage: llpyg <pythonLibPath> [<destDir>]\n"))
		return
	}

	pyLib := c.Index(c.Argv, 1)
	destDir := c.Str(".")
	if c.Argc > 2 {
		destDir = c.Index(c.Argv, 2)
	}
	c.Fprintf(c.Stderr, c.Str("pyLib: %s, destDir: %s\n"), pyLib, destDir)

	py.Initialize()
	mod := py.ImportModule(pyLib)
	keys := mod.ModuleGetDict().DictKeys()
	for i, n := uintptr(0), keys.ListLen(); i < n; i++ {
		key := keys.ListItem(i)
		val := mod.GetAttr(key)
		doc := val.GetAttrString(c.Str("__doc__"))
		c.Fprintf(c.Stderr, c.Str("-----------------------------------\n"))
		if val.Callable() != 0 {
			sig := inspect.Signature(val)
			c.Fprintf(c.Stderr, c.Str("%s: %s\n"), key.CStr(), sig.Str().CStr())
		} else {
			c.Fprintf(c.Stderr, c.Str("var: %s\n"), key.CStr())
		}
		c.Fprintf(c.Stderr, c.Str("%s\n"), doc.CStr())
	}
}
