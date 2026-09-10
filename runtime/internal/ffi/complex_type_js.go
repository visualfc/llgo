//go:build js

package ffi

// Emscripten's vendored libffi is built without FFI_TARGET_HAS_COMPLEX_TYPE.
// ffi_prep_cif_core aborts on FFI_TYPE_COMPLEX. Go complex values have the
// same memory layout as a two-element float pair, so describe them as a
// public libffi struct (same approach as Windows).
func newComplexType(elem *Type, _ uintptr, _ uint16) *Type {
	return StructOf(elem, elem)
}
