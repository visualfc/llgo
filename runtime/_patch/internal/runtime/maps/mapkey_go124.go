//go:build !go1.26

package maps

//llgo:skip mapKeyError

import (
	"internal/abi"
	"unsafe"
)

func mapKeyError(typ *abi.SwissMapType, p unsafe.Pointer) error {
	return nil
}
