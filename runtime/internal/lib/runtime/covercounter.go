// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

package runtime

import "unsafe"

type covCounterBlob struct {
	Counters *uint32
	Len      uint64
}

var coverageCounters []covCounterBlob

// Only a covered test main installs this callback. Keep the normal runtime
// independent of internal/coverage and its substantial reporting dependency tree.
var coverageExitHook func(int)

func setCoverageExitHook(hook func(int)) {
	coverageExitHook = hook
}

func runCoverageExitHook(code int) {
	if coverageExitHook != nil {
		coverageExitHook(code)
	}
}

// registerCoverage is called once per instrumented package, before its user
// variable initializers. Native LLVM links do not have Go moduledata counter
// sections, so generated static descriptors provide the same counter ranges.
// Only package initialization writes this registry; readers run after init.
func registerCoverage(
	meta unsafe.Pointer,
	size uint32,
	hash [16]byte,
	pkgpath string,
	pkgid int,
	mode uint8,
	counters []covCounterBlob,
) uint32 {
	id := addCoverageMeta(meta, size, hash, pkgpath, pkgid, mode, 1)
	if id == 0 {
		panic("runtime.addCovMeta: coverage package map collision")
	}
	coverageCounters = append(coverageCounters, counters...)
	return id
}
