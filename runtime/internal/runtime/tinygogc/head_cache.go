/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 */

package tinygogc

// markHeadCache memoizes large object intervals during a single collection.
// Store complemented block indexes, not raw heap addresses or indexes that
// could accidentally look like heap addresses to conservative root scanning.
// This follows weak-key encoding; conservative false positives remain possible.
type markHeadCache struct {
	ranges [4]markHeadRange
	next   uint8
}

type markHeadRange struct {
	head, end uintptr // complemented indexes; end is exclusive
}

// Four KiB on wasm32 and eight KiB on wasm64 amortize interval lookup while
// keeping cheap small-object lookups from displacing large allocations.
const markHeadMinBlocks = 256

func (c *markHeadCache) reset() { *c = markHeadCache{} }

func (c *markHeadCache) lookup(block uintptr) (uintptr, bool) {
	for _, r := range c.ranges {
		if r.end != 0 && block >= ^r.head && block < ^r.end {
			return ^r.head, true
		}
	}
	return 0, false
}

// remember accepts either a complete object range or a proven prefix ending
// just after an interior pointer. Later observations may extend that prefix.
func (c *markHeadCache) remember(head, end uintptr) {
	// Favor large objects, including the GC-owned Fiber stack storage.
	if end <= head || end-head < markHeadMinBlocks || end == ^uintptr(0) {
		return
	}
	for i := range c.ranges {
		r := &c.ranges[i]
		if r.end != 0 && ^r.head == head {
			if end > ^r.end {
				r.end = ^end
			}
			return
		}
	}
	c.ranges[c.next] = markHeadRange{head: ^head, end: ^end}
	c.next = (c.next + 1) % uint8(len(c.ranges))
}
