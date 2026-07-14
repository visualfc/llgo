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
	// blocks points at the payload of the most recently used package block.
	// Keeping the aligned payload at the head makes the common lookup return it
	// directly; the block header is stored immediately before the payload.
	blocks unsafe.Pointer
}

type localBlock struct {
	// next points at the next block's payload, not its header.
	next unsafe.Pointer
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
	data := ctx.blocks
	ctx.blocks = nil
	for data != nil {
		block := localBlockHeader(data)
		next := block.next
		// Do not free block here: an address of a local variable may outlive its
		// owner. Breaking the links lets the GC retain only escaped blocks.
		block.next = nil
		data = next
	}
}

// LocalPackage returns stable, zeroed storage for one package in the current
// physical owner. Repeated access to the most recently used package takes the
// head fast path; other accesses move the matching block to the front.
func LocalPackage(key unsafe.Pointer, size, align uintptr) unsafe.Pointer {
	ctx := (*LocalContext)(unsafe.Pointer(currentLocalContext))
	if ctx != nil {
		firstData := ctx.blocks
		if firstData != nil && localBlockHeader(firstData).key == key {
			return firstData
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
	firstData := ctx.blocks
	var previous *localBlock
	for data := firstData; data != nil; {
		block := localBlockHeader(data)
		next := block.next
		if block.key == key {
			previous.next = next
			block.next = firstData
			ctx.blocks = data
			return data
		}
		previous = block
		data = next
	}
	data := newLocalBlock(key, size, align)
	localBlockHeader(data).next = firstData
	ctx.blocks = data
	return data
}

func newLocalBlock(key unsafe.Pointer, size, align uintptr) unsafe.Pointer {
	header := unsafe.Sizeof(localBlock{})
	padding := align - 1
	if size == 0 {
		size = 1
	}
	if header > ^uintptr(0)-padding || header+padding > ^uintptr(0)-size {
		panic("runtime: local package size overflow")
	}
	allocation := AllocZ(header + padding + size)
	if allocation == nil {
		panic("runtime: failed to allocate local package")
	}
	data := unsafe.Pointer((uintptr(allocation) + header + padding) &^ padding)
	block := localBlockHeader(data)
	block.key = key
	return data
}

func localBlockHeader(data unsafe.Pointer) *localBlock {
	return (*localBlock)(unsafe.Pointer(uintptr(data) - unsafe.Sizeof(localBlock{})))
}
