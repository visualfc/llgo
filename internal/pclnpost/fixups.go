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

package pclnpost

import (
	"encoding/binary"
	"fmt"
)

// Mach-O dyld chained fixups surgery.
//
// The on-disk pointer slots of the rewritten sections participate in dyld's
// chained-fixup page chains. If the chains are left untouched, dyld walks
// them at load time and rebases 8-byte slots inside our freshly written
// table, corrupting it — and terminating a chain early inside a zeroed
// section would also skip later fixups in the same page, corrupting
// unrelated data. unchainRanges removes every chain node that falls inside
// the given file-offset ranges: predecessors' next links (or the page_start
// table) are repointed to the first surviving successor.

const (
	lcDyldChainedFixups = 0x80000034
	lcSegment64         = 0x19

	chainedPtrStartNone  = 0xFFFF
	chainedPtrStartMulti = 0x8000

	// pointer_format values with a 12-bit next field at bit 51, stride 4.
	chainedPtr64       = 2
	chainedPtr64Offset = 6
)

type segRange struct {
	fileOff uint64
	fileSz  uint64
}

// unchainRanges edits raw in place. ranges are file-offset [start, end) pairs.
func unchainRanges(raw []byte, ranges [][2]uint64) error {
	inRange := func(off uint64) bool {
		for _, r := range ranges {
			if off >= r[0] && off < r[1] {
				return true
			}
		}
		return false
	}

	// Locate LC_DYLD_CHAINED_FIXUPS and the segment table.
	if len(raw) < 32 || binary.LittleEndian.Uint32(raw) != 0xFEEDFACF {
		return fmt.Errorf("not a 64-bit little-endian Mach-O")
	}
	ncmds := binary.LittleEndian.Uint32(raw[16:])
	off := uint64(32)
	var fixOff, fixSize uint64
	var segs []segRange
	for i := uint32(0); i < ncmds; i++ {
		cmd := binary.LittleEndian.Uint32(raw[off:])
		size := binary.LittleEndian.Uint32(raw[off+4:])
		switch cmd {
		case lcDyldChainedFixups:
			fixOff = uint64(binary.LittleEndian.Uint32(raw[off+8:]))
			fixSize = uint64(binary.LittleEndian.Uint32(raw[off+12:]))
		case lcSegment64:
			segs = append(segs, segRange{
				fileOff: binary.LittleEndian.Uint64(raw[off+40:]),
				fileSz:  binary.LittleEndian.Uint64(raw[off+48:]),
			})
		}
		off += uint64(size)
	}
	if fixOff == 0 {
		return nil // no chained fixups (classic dyld info); nothing to do
	}
	_ = fixSize

	hdr := raw[fixOff:]
	startsOff := fixOff + uint64(binary.LittleEndian.Uint32(hdr[4:]))
	segCount := binary.LittleEndian.Uint32(raw[startsOff:])
	if int(segCount) != len(segs) {
		// seg_count counts all segments incl. ones without fixups; trust it.
	}
	for si := uint32(0); si < segCount; si++ {
		segInfoOff := binary.LittleEndian.Uint32(raw[startsOff+4+uint64(si)*4:])
		if segInfoOff == 0 {
			continue
		}
		sOff := startsOff + uint64(segInfoOff)
		pageSize := uint64(binary.LittleEndian.Uint16(raw[sOff+4:]))
		ptrFormat := binary.LittleEndian.Uint16(raw[sOff+6:])
		pageCount := uint64(binary.LittleEndian.Uint16(raw[sOff+20:]))
		if ptrFormat != chainedPtr64 && ptrFormat != chainedPtr64Offset {
			// Only reject if this segment's pages intersect our ranges.
			segFile := segs[si].fileOff
			touches := false
			for _, r := range ranges {
				if r[0] < segFile+pageCount*pageSize && r[1] > segFile {
					touches = true
				}
			}
			if !touches {
				continue
			}
			return fmt.Errorf("unsupported pointer_format %d", ptrFormat)
		}
		segFileOff := segs[si].fileOff
		for pi := uint64(0); pi < pageCount; pi++ {
			psOff := sOff + 22 + pi*2
			pStart := binary.LittleEndian.Uint16(raw[psOff:])
			if pStart == chainedPtrStartNone {
				continue
			}
			if pStart&chainedPtrStartMulti != 0 {
				return fmt.Errorf("multi-start pages not supported")
			}
			pageFile := segFileOff + pi*pageSize
			// Collect the chain.
			var nodes []uint64
			node := pageFile + uint64(pStart)
			for {
				nodes = append(nodes, node)
				val := binary.LittleEndian.Uint64(raw[node:])
				next := (val >> 51) & 0xFFF
				if next == 0 {
					break
				}
				node += next * 4
			}
			// Rebuild keeping only out-of-range nodes.
			var kept []uint64
			removed := 0
			for _, n := range nodes {
				if inRange(n) {
					removed++
				} else {
					kept = append(kept, n)
				}
			}
			if removed == 0 {
				continue
			}
			if len(kept) == 0 {
				binary.LittleEndian.PutUint16(raw[psOff:], chainedPtrStartNone)
				continue
			}
			binary.LittleEndian.PutUint16(raw[psOff:], uint16(kept[0]-pageFile))
			for i, n := range kept {
				val := binary.LittleEndian.Uint64(raw[n:])
				var next uint64
				if i+1 < len(kept) {
					delta := kept[i+1] - n
					if delta%4 != 0 || delta/4 > 0xFFF {
						return fmt.Errorf("chain gap %d not encodable", delta)
					}
					next = delta / 4
				}
				val = (val &^ (uint64(0xFFF) << 51)) | (next << 51)
				binary.LittleEndian.PutUint64(raw[n:], val)
			}
		}
	}
	return nil
}
