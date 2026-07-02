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

// Package pclnpost implements the P1/P2 prototype of link-phase ftab/findfunctab
// generation (doc/design/pclntab-linkphase.md). It parses a linked LLGo
// binary's funcinfo site sections, deduplicates LTO inline copies against the
// symbol table, sorts the entries, builds the Go-layout findfunctab via
// internal/pclntab, and prints what the P2 build integration would write
// back. It performs no writes; its purpose is to prove the risky steps on
// real binaries.
package pclnpost

import (
	"debug/elf"
	"debug/macho"
	"encoding/binary"
	"fmt"
	"os"
	"sort"

	"github.com/goplus/llgo/internal/pclntab"
)

type siteRecord struct {
	pc       uint64
	symbolID uint64
}

type textSym struct {
	addr uint64
	size uint64
	name string
}

type secInfo struct {
	vmaddr  uint64
	size    uint64
	fileOff uint64
}

type binaryInfo struct {
	format    string
	raw       []byte
	entrySec  []byte
	stubSec   []byte
	textStart uint64
	textEnd   uint64
	imageBase uint64
	syms      []textSym // sorted by addr, text symbols only
	secs      []secInfo

	entryVMAddr, entryVMSize, entryFileOff uint64
	stubVMSize, stubFileOff                uint64
}

// readVM returns n bytes at a link-time virtual address.
func readVM(info *binaryInfo, addr uint64, n int) []byte {
	for _, s := range info.secs {
		if addr >= s.vmaddr && addr+uint64(n) <= s.vmaddr+s.size {
			off := s.fileOff + (addr - s.vmaddr)
			return info.raw[off : off+uint64(n)]
		}
	}
	return make([]byte, n)
}

func load(path string) (*binaryInfo, error) {
	if mf, err := macho.Open(path); err == nil {
		defer mf.Close()
		info := &binaryInfo{format: "macho"}
		info.raw, _ = os.ReadFile(path)
		for _, s := range mf.Sections {
			info.secs = append(info.secs, secInfo{vmaddr: s.Addr, size: s.Size, fileOff: uint64(s.Offset)})
		}
		if s := mf.Section("__llgo_fie"); s != nil {
			info.entrySec, _ = s.Data()
			info.entryVMAddr, info.entryVMSize, info.entryFileOff = s.Addr, s.Size, uint64(s.Offset)
		}
		if s := mf.Section("__llgo_stub"); s != nil {
			info.stubSec, _ = s.Data()
			info.stubVMSize, info.stubFileOff = s.Size, uint64(s.Offset)
		}
		if s := mf.Section("__text"); s != nil {
			info.textStart, info.textEnd = s.Addr, s.Addr+s.Size
			info.imageBase = s.Addr &^ 0xFFFFFFF
		}
		if mf.Symtab != nil {
			for _, sym := range mf.Symtab.Syms {
				if sym.Value >= info.textStart && sym.Value < info.textEnd && sym.Name != "" {
					info.syms = append(info.syms, textSym{addr: sym.Value, name: sym.Name})
				}
			}
		}
		finish(info)
		return info, nil
	}
	ef, err := elf.Open(path)
	if err != nil {
		return nil, fmt.Errorf("not Mach-O and not ELF: %w", err)
	}
	defer ef.Close()
	info := &binaryInfo{format: "elf"}
	info.raw, _ = os.ReadFile(path)
	for _, s := range ef.Sections {
		if s.Type != elf.SHT_NOBITS && s.Addr != 0 {
			info.secs = append(info.secs, secInfo{vmaddr: s.Addr, size: s.Size, fileOff: s.Offset})
		}
	}
	if s := ef.Section("llgo_funcinfo_entry"); s != nil {
		info.entrySec, _ = s.Data()
		info.entryVMAddr, info.entryVMSize, info.entryFileOff = s.Addr, s.Size, s.Offset
	}
	if s := ef.Section("llgo_funcinfo_stubsite"); s != nil {
		info.stubSec, _ = s.Data()
		info.stubVMSize, info.stubFileOff = s.Size, s.Offset
	}
	if s := ef.Section(".text"); s != nil {
		info.textStart, info.textEnd = s.Addr, s.Addr+s.Size
		info.imageBase = s.Addr &^ 0xFFFFFFF
	}
	syms, _ := ef.Symbols()
	for _, sym := range syms {
		if elf.ST_TYPE(sym.Info) == elf.STT_FUNC && sym.Value >= info.textStart && sym.Value < info.textEnd {
			info.syms = append(info.syms, textSym{addr: sym.Value, size: sym.Size, name: sym.Name})
		}
	}
	finish(info)
	return info, nil
}

func finish(info *binaryInfo) {
	sort.Slice(info.syms, func(i, j int) bool { return info.syms[i].addr < info.syms[j].addr })
	// Collapse same-address aliases, then derive missing extents from the
	// next distinct symbol start (Mach-O nlist carries no sizes; Go's linker
	// uses the same next-start rule for its final ftab).
	dedup := info.syms[:0]
	for _, s := range info.syms {
		if len(dedup) > 0 && dedup[len(dedup)-1].addr == s.addr {
			continue
		}
		dedup = append(dedup, s)
	}
	info.syms = dedup
	for i := range info.syms {
		if info.syms[i].size == 0 {
			if i+1 < len(info.syms) {
				info.syms[i].size = info.syms[i+1].addr - info.syms[i].addr
			} else {
				info.syms[i].size = info.textEnd - info.syms[i].addr
			}
		}
	}
}

func parseRecords(info *binaryInfo, sec []byte) []siteRecord {
	var out []siteRecord
	for off := 0; off+16 <= len(sec); off += 16 {
		pc := binary.LittleEndian.Uint64(sec[off:])
		id := binary.LittleEndian.Uint64(sec[off+8:])
		if pc == 0 || id == 0 { // zero keep-alive record
			continue
		}
		// Mach-O pointer slots in the on-disk file hold dyld chained-fixup
		// encodings (DYLD_CHAINED_PTR_64: target in the low 36 bits, chain
		// metadata above); dyld rewrites them at load. Decode when the raw
		// value falls outside the text range but its low 36 bits fall
		// inside. The P2 write-back avoids the problem entirely by storing
		// anchor-relative offsets instead of pointers.
		if info.format == "macho" && (pc < info.textStart || pc >= info.textEnd) {
			if t := pc & (1<<36 - 1); t >= info.textStart && t < info.textEnd {
				pc = t
			}
		}
		out = append(out, siteRecord{pc: pc, symbolID: id})
	}
	return out
}

// owner returns the text symbol containing addr.
func owner(info *binaryInfo, addr uint64) (textSym, bool) {
	i := sort.Search(len(info.syms), func(i int) bool { return info.syms[i].addr > addr })
	if i == 0 {
		return textSym{}, false
	}
	s := info.syms[i-1]
	if addr >= s.addr+s.size {
		return textSym{}, false
	}
	return s, true
}

// fnv64 mirrors funcInfoSymbolID in internal/build/funcinfo_table.go.
func fnv64(name string) uint64 {
	const offset = uint64(14695981039346656037)
	const prime = uint64(1099511628211)
	h := offset
	for i := 0; i < len(name); i++ {
		h ^= uint64(name[i])
		h *= prime
	}
	if h == 0 {
		return 1
	}
	return h
}

const stubPrefix = "__llgo_stub."

// dedupe keeps exactly the canonical record per emitting function: a record
// is canonical when the symbol that owns its anchor PC is the function the
// symbolID names (id == fnv64(owner)) or that function's closure stub
// (owner "__llgo_stub.X" with id == fnv64(X) — stubs share the target's
// symbolID by design). Everything else with a known owner is an LTO inline
// copy: inlining duplicated the body-embedded record into a host function.
// Kept records are normalized to their owner's true entry address. Records
// whose owner cannot be determined are dropped conservatively.
func dedupe(info *binaryInfo, recs []siteRecord, verbose bool) (kept []siteRecord, droppedInline, droppedUnknown int) {
	seenOwner := make(map[uint64]bool, len(recs))
	for _, r := range recs {
		sym, ok := owner(info, r.pc)
		if !ok {
			droppedUnknown++
			continue
		}
		name := sym.name
		if info.format == "macho" && len(name) > 0 && name[0] == '_' {
			name = name[1:]
		}
		target := name
		if len(name) > len(stubPrefix) && name[:len(stubPrefix)] == stubPrefix {
			target = name[len(stubPrefix):]
		}
		if fnv64(target) != r.symbolID {
			droppedInline++
			if verbose {
				fmt.Printf("  inline copy: id=%#x pc=%#x inside %s\n", r.symbolID, r.pc, sym.name)
			}
			continue
		}
		if seenOwner[sym.addr] {
			continue
		}
		seenOwner[sym.addr] = true
		kept = append(kept, siteRecord{pc: sym.addr, symbolID: r.symbolID})
	}
	return kept, droppedInline, droppedUnknown
}

// buildFtab returns the sorted table plus the base PC (Go's minpc): offsets
// are relative to the first recorded function so ftab[0].EntryOff == 0, as
// internal/pclntab requires.
func buildFtab(info *binaryInfo, kept []siteRecord) ([]pclntab.FuncTabEntry, uint64) {
	sort.Slice(kept, func(i, j int) bool { return kept[i].pc < kept[j].pc })
	if len(kept) == 0 {
		return nil, info.textStart
	}
	base := kept[0].pc
	ftab := make([]pclntab.FuncTabEntry, 0, len(kept)+1)
	prev := uint64(0)
	for i, r := range kept {
		if r.pc == prev {
			continue // two symbolIDs at one entry (aliases); keep first
		}
		prev = r.pc
		ftab = append(ftab, pclntab.FuncTabEntry{EntryOff: uint32(r.pc - base), FuncOff: uint32(i)})
	}
	// Go-style sentinel at end of text.
	ftab = append(ftab, pclntab.FuncTabEntry{EntryOff: uint32(info.textEnd - base), FuncOff: ^uint32(0)})
	return ftab, base
}
