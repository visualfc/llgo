//go:build go1.26

package maps

//llgo:skip mapKeyError typeString

import (
	"internal/abi"
	"unsafe"
)

//go:linkname typeString github.com/goplus/llgo/runtime/abi.(*Type).String
func typeString(typ *abi.Type) string

func mapKeyError(typ *abi.MapType, p unsafe.Pointer) error {
	return nil
}
