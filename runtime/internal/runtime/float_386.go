// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

//go:build 386

package runtime

import "unsafe"

const float386Sign64 = 1 << 63

func Float64ToInt64(d float64) int64 {
	return int64(float64ToUint64Bits386(d))
}

func Float64ToUint64(d float64) uint64 {
	return float64ToUint64Bits386(d)
}

// float64ToUint64Bits386 is adapted from the Go runtime's _d2v helper in
// runtime/vlrt.go. The 386 compiler uses this same bit-level conversion for
// both signed and unsigned 64-bit results.
func float64ToUint64Bits386(d float64) uint64 {
	x := *(*uint64)(unsafe.Pointer(&d))
	xhi := uint32(x>>32)&0xfffff | 0x100000
	xlo := uint32(x)
	shift := 1075 - int32(uint32(x>>52)&0x7ff)

	var ylo, yhi uint32
	if shift >= 0 {
		shift := uint32(shift)
		if shift < 32 {
			if shift == 0 {
				ylo = xlo
				yhi = xhi
			} else {
				ylo = xlo>>shift | xhi<<(32-shift)
				yhi = xhi >> shift
			}
		} else if shift == 32 {
			ylo = xhi
		} else if shift < 64 {
			ylo = xhi >> (shift - 32)
		}
	} else {
		shift := uint32(-shift)
		if shift <= 11 {
			ylo = xlo << shift
			yhi = xhi<<shift | xlo>>(32-shift)
		}
		// For larger shifts gc/386's uint32 overflow conversion yields zero.
		// Leaving both words zero preserves that behavior without recursively
		// lowering another float-to-uint conversion inside this helper.
	}
	if x&float386Sign64 != 0 {
		if ylo != 0 {
			ylo = -ylo
			yhi = ^yhi
		} else {
			yhi = -yhi
		}
	}
	return uint64(yhi)<<32 | uint64(ylo)
}
