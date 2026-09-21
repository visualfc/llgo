//go:build !go1.23

// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

package runtime

import "unsafe"

// Go 1.20–1.22 store metadata in runtime rather than rtcov.AddMeta. Mirror
// rtcov.CovMetaBlob here because this replacement package cannot import Go's
// internal package directly. Go 1.23+ uses the standard registry instead.
type coverageMetaBlob struct {
	P                  *byte
	Len                uint32
	Hash               [16]byte
	PkgPath            string
	PkgID              int
	CounterMode        uint8
	CounterGranularity uint8
}

var coverageMetaList []coverageMetaBlob
var coveragePkgMap map[int]int

func addCoverageMeta(
	meta unsafe.Pointer,
	size uint32,
	hash [16]byte,
	pkgpath string,
	pkgid int,
	mode uint8,
	granularity uint8,
) uint32 {
	slot := len(coverageMetaList)
	coverageMetaList = append(coverageMetaList, coverageMetaBlob{
		P:                  (*byte)(meta),
		Len:                size,
		Hash:               hash,
		PkgPath:            pkgpath,
		PkgID:              pkgid,
		CounterMode:        mode,
		CounterGranularity: granularity,
	})
	if pkgid != -1 {
		if coveragePkgMap == nil {
			coveragePkgMap = make(map[int]int)
		}
		if _, exists := coveragePkgMap[pkgid]; exists {
			return 0
		}
		coveragePkgMap[pkgid] = slot
	}
	return uint32(slot + 1)
}

//go:linkname coverageGetMetaList runtime/coverage.getCovMetaList
func coverageGetMetaList() []coverageMetaBlob {
	return coverageMetaList
}

//go:linkname coverageGetPkgMap runtime/coverage.getCovPkgMap
func coverageGetPkgMap() map[int]int {
	return coveragePkgMap
}

//go:linkname coverage_getCovCounterList runtime/coverage.getCovCounterList
func coverage_getCovCounterList() []covCounterBlob {
	return coverageCounters
}
