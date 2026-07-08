// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	_ "unsafe"
)

type covCounterBlob struct {
	Counters *uint32
	Len      uint64
}

//go:linkname coverage_getCovCounterList internal/coverage/cfile.getCovCounterList
func coverage_getCovCounterList() []covCounterBlob {
	return nil
}
