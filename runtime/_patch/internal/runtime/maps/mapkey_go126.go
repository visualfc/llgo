//go:build go1.26

package maps

//llgo:skip mapKeyError typeString

import (
	"internal/abi"
	"unsafe"
)

func typeString(typ *abi.Type) string {
	if typ == nil {
		return "<nil>"
	}
	return llgoTypeString(typ)
}

//go:linkname llgoTypeString github.com/goplus/llgo/runtime/abi.(*Type).String
func llgoTypeString(typ *abi.Type) string

func mapKeyError(typ *abi.MapType, p unsafe.Pointer) error {
	return nil
}
