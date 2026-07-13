// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm

package chacha8rand

func block(seed *[4]uint64, blocks *[32]uint64, counter uint32) {
	block_generic(seed, blocks, counter)
}
