// Copyright 2026 The XGo Authors (xgo.dev). All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

//go:build 386

package atomic

import "unsafe"

// The Go 386 implementation uses Plan 9 assembly for atomic operations. LLGo
// lowers these entry points to LLVM atomics instead. Keep the explicit 64-bit
// alignment checks required by the Go 386 implementation.

//go:linkname llgoLoad64 llgo.atomicLoad
func llgoLoad64(ptr *uint64) uint64

//go:linkname llgoStorePointer llgo.atomicStore
func llgoStorePointer(ptr *unsafe.Pointer, val unsafe.Pointer)

//go:linkname Xadd llgo.atomicAddReturnNew
//go:linkname Xadduintptr llgo.atomicAddReturnNew
//go:linkname Xaddint32 llgo.atomicAddReturnNew
//go:linkname Xchg llgo.atomicXchg
//go:linkname Xchg8 llgo.atomicXchg
//go:linkname Xchguintptr llgo.atomicXchg
//go:linkname Xchgint32 llgo.atomicXchg
//go:linkname And8 llgo.atomicAnd
//go:linkname Or8 llgo.atomicOr
//go:linkname And llgo.atomicAnd
//go:linkname Or llgo.atomicOr
//go:linkname And32 llgo.atomicAnd
//go:linkname Or32 llgo.atomicOr
//go:linkname And64 llgo.atomicAnd
//go:linkname Or64 llgo.atomicOr
//go:linkname Anduintptr llgo.atomicAnd
//go:linkname Oruintptr llgo.atomicOr
//go:linkname Cas llgo.atomicCmpXchgOK
//go:linkname Casp1 llgo.atomicCmpXchgOK
//go:linkname Casint32 llgo.atomicCmpXchgOK
//go:linkname Casuintptr llgo.atomicCmpXchgOK
//go:linkname CasRel llgo.atomicCmpXchgOK
//go:linkname Loaduintptr llgo.atomicLoad
//go:linkname Loaduint llgo.atomicLoad
//go:linkname Loadint32 llgo.atomicLoad
//go:linkname Store llgo.atomicStore
//go:linkname Store8 llgo.atomicStore
//go:linkname StoreRel llgo.atomicStore
//go:linkname StoreReluintptr llgo.atomicStore
//go:linkname Storeint32 llgo.atomicStore
//go:linkname Storeuintptr llgo.atomicStore

//go:nosplit
//go:noinline
func StorepNoWB(ptr unsafe.Pointer, val unsafe.Pointer) {
	llgoStorePointer((*unsafe.Pointer)(ptr), val)
}

func check64(ptr unsafe.Pointer) {
	if uintptr(ptr)&7 != 0 {
		panicUnaligned()
	}
}

//go:linkname llgoXadd64 llgo.atomicAddReturnNew
func llgoXadd64(ptr *uint64, delta int64) uint64

//go:nosplit
func Xadd64(ptr *uint64, delta int64) uint64 {
	check64(unsafe.Pointer(ptr))
	return llgoXadd64(ptr, delta)
}

//go:nosplit
func Xaddint64(ptr *int64, delta int64) int64 {
	return int64(Xadd64((*uint64)(unsafe.Pointer(ptr)), delta))
}

//go:linkname llgoXchg64 llgo.atomicXchg
func llgoXchg64(ptr *uint64, val uint64) uint64

//go:nosplit
func Xchg64(ptr *uint64, val uint64) uint64 {
	check64(unsafe.Pointer(ptr))
	return llgoXchg64(ptr, val)
}

//go:nosplit
func Xchgint64(ptr *int64, val int64) int64 {
	return int64(Xchg64((*uint64)(unsafe.Pointer(ptr)), uint64(val)))
}

//go:nosplit
func Load64(ptr *uint64) uint64 {
	check64(unsafe.Pointer(ptr))
	return llgoLoad64(ptr)
}

//go:nosplit
func Loadint64(ptr *int64) int64 {
	return int64(Load64((*uint64)(unsafe.Pointer(ptr))))
}

//go:linkname llgoStore64 llgo.atomicStore
func llgoStore64(ptr *uint64, val uint64)

//go:nosplit
func Store64(ptr *uint64, val uint64) {
	check64(unsafe.Pointer(ptr))
	llgoStore64(ptr, val)
}

//go:nosplit
func Storeint64(ptr *int64, val int64) {
	Store64((*uint64)(unsafe.Pointer(ptr)), uint64(val))
}

//go:linkname llgoCas64 llgo.atomicCmpXchgOK
func llgoCas64(ptr *uint64, old, new uint64) bool

//go:nosplit
func Cas64(ptr *uint64, old, new uint64) bool {
	check64(unsafe.Pointer(ptr))
	return llgoCas64(ptr, old, new)
}

//go:nosplit
func Casint64(ptr *int64, old, new int64) bool {
	return Cas64((*uint64)(unsafe.Pointer(ptr)), uint64(old), uint64(new))
}
