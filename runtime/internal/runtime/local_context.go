//go:build llgo

/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package runtime

import "unsafe"

// LocalContext is rooted by the outer Go entry stack frame. The current
// one-thread-per-goroutine backend maps both logical locality kinds to this one
// physical package store.
type LocalContext struct {
	blocks *localBlock
}

type localBlock struct {
	next *localBlock
	key  unsafe.Pointer
}

func EnterLocalContext(ctx *LocalContext) uintptr {
	previous := currentLocalContext
	if previous == 0 {
		if ctx == nil {
			panic("runtime: nil local context")
		}
		currentLocalContext = uintptr(unsafe.Pointer(ctx))
	}
	return previous
}

func LeaveLocalContext(ctx *LocalContext, previous uintptr) {
	if previous != 0 {
		if currentLocalContext != previous {
			panic("runtime: local context changed by nested entry")
		}
		return
	}
	if currentLocalContext != uintptr(unsafe.Pointer(ctx)) {
		panic("runtime: leaving inactive local context")
	}
	currentLocalContext = 0
	releaseLocalBlocks(ctx)
}

func leaveCurrentLocalContext() {
	ctx := (*LocalContext)(unsafe.Pointer(currentLocalContext))
	if ctx == nil {
		return
	}
	currentLocalContext = 0
	releaseLocalBlocks(ctx)
}

func releaseLocalBlocks(ctx *LocalContext) {
	block := ctx.blocks
	ctx.blocks = nil
	for block != nil {
		next := block.next
		// Do not free block here: an address of a local variable may outlive its
		// owner. Breaking the links lets the GC retain only escaped blocks.
		block.next = nil
		block = next
	}
}

// LocalPackage returns stable, zeroed storage for one package in the current
// physical owner. Repeated access to the most recently used package takes the
// head fast path; other accesses move the matching block to the front.
func LocalPackage(key unsafe.Pointer, size, align uintptr) unsafe.Pointer {
	ctx := (*LocalContext)(unsafe.Pointer(currentLocalContext))
	if ctx != nil {
		first := ctx.blocks
		if first != nil && first.key == key && align != 0 && align&(align-1) == 0 {
			return localBlockData(first, align)
		}
	}
	return localPackageSlow(ctx, key, size, align)
}

//go:noinline
func localPackageSlow(ctx *LocalContext, key unsafe.Pointer, size, align uintptr) unsafe.Pointer {
	if ctx == nil {
		panic("runtime: local variable accessed outside a Go entry context")
	}
	if key == nil {
		panic("runtime: nil local package key")
	}
	if align == 0 || align&(align-1) != 0 {
		panic("runtime: invalid local package alignment")
	}
	first := ctx.blocks
	var previous *localBlock
	for block := first; block != nil; block = block.next {
		if block.key == key {
			previous.next = block.next
			block.next = first
			ctx.blocks = block
			return localBlockData(block, align)
		}
		previous = block
	}
	block := newLocalBlock(key, size, align)
	block.next = first
	ctx.blocks = block
	return localBlockData(block, align)
}

func newLocalBlock(key unsafe.Pointer, size, align uintptr) *localBlock {
	header := unsafe.Sizeof(localBlock{})
	padding := align - 1
	if size == 0 {
		size = 1
	}
	if header > ^uintptr(0)-padding || header+padding > ^uintptr(0)-size {
		panic("runtime: local package size overflow")
	}
	block := (*localBlock)(AllocZ(header + padding + size))
	if block == nil {
		panic("runtime: failed to allocate local package")
	}
	block.key = key
	return block
}

func localBlockData(block *localBlock, align uintptr) unsafe.Pointer {
	padding := align - 1
	data := (uintptr(unsafe.Pointer(block)) + unsafe.Sizeof(localBlock{}) + padding) &^ padding
	return unsafe.Pointer(data)
}
