//go:build go1.23

// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

package runtime

import "unsafe"

//go:linkname addCoverageMeta internal/coverage/rtcov.AddMeta
func addCoverageMeta(
	meta unsafe.Pointer,
	size uint32,
	hash [16]byte,
	pkgpath string,
	pkgid int,
	mode uint8,
	granularity uint8,
) uint32

//go:linkname coverage_getCovCounterList internal/coverage/cfile.getCovCounterList
func coverage_getCovCounterList() []covCounterBlob {
	return coverageCounters
}
