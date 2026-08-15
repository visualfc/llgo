//go:build go1.25 && !go1.26

package maps

//llgo:skip typeString

import (
	"internal/abi"
	_ "unsafe"
)

//go:linkname typeString github.com/goplus/llgo/runtime/abi.(*Type).String
func typeString(typ *abi.Type) string
