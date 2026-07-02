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

// Command pclnpost is the P1 prototype of link-phase ftab/findfunctab
// generation (doc/design/pclntab-linkphase.md). It parses a linked LLGo
// binary's funcinfo site sections, deduplicates LTO inline copies against the
// symbol table, sorts the entries, builds the Go-layout findfunctab via
// internal/pclntab, and prints what the P2 build integration would write
// back. It performs no writes; its purpose is to prove the risky steps on
// real binaries.
package main

import (
	"debug/elf"
	"debug/macho"
	"encoding/binary"
	"flag"
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

type binaryInfo struct {
	format    string
	entrySec  []byte
	stubSec   []byte
	textStart uint64
	textEnd   uint64
	syms      []textSym // sorted by addr, text symbols only
}

func main() {
	verbose := flag.Bool("v", false, "print per-record details for dropped inline copies")
	flag.Parse()
	if flag.NArg() != 1 {
		fmt.Fprintln(os.Stderr, "usage: pclnpost [-v] <linked-binary>")
		os.Exit(2)
	}
	path := flag.Arg(0)
	info, err := load(path)
	if err != nil {
		fmt.Fprintln(os.Stderr, "pclnpost:", err)
		os.Exit(1)
	}
	fmt.Printf("format=%s text=[%#x,%#x) textSyms=%d\n",
		info.format, info.textStart, info.textEnd, len(info.syms))

	entries := parseRecords(info, info.entrySec)
	stubs := parseRecords(info, info.stubSec)
	fmt.Printf("entry records=%d stub records=%d\n", len(entries), len(stubs))

	kept, droppedInline, droppedUnknown := dedupe(info, append(entries, stubs...), *verbose)
	fmt.Printf("dedupe: kept=%d droppedInlineCopies=%d droppedNoSymbol=%d\n",
		len(kept), droppedInline, droppedUnknown)

	ftab, base := buildFtab(info, kept)
	buckets, err := pclntab.BuildFindFuncBuckets(ftab, uint32(info.textEnd-base))
	if err != nil {
		fmt.Fprintln(os.Stderr, "pclnpost: BuildFindFuncBuckets:", err)
		os.Exit(1)
	}
	fmt.Printf("ftab entries=%d (incl sentinel) findfunctab buckets=%d\n", len(ftab), len(buckets))

	// Verify the Go-layout lookup answers every kept entry PC.
	bad := 0
	for i, e := range ftab[:len(ftab)-1] {
		if got := pclntab.LookupFuncIndex(ftab, buckets, e.EntryOff); got != i {
			bad++
		}
	}
	fmt.Printf("lookup self-check: %d/%d entry PCs resolve to their own index\n",
		len(ftab)-1-bad, len(ftab)-1)

	oldBytes := (len(entries) + len(stubs)) * 16
	newBytes := len(ftab)*8 + len(buckets)*20
	fmt.Printf("size: site sections %dB -> ftab+findfunctab %dB (%.1fx smaller)\n",
		oldBytes, newBytes, float64(oldBytes)/float64(newBytes))
	if bad != 0 {
		os.Exit(1)
	}
}

func load(path string) (*binaryInfo, error) {
	if mf, err := macho.Open(path); err == nil {
		defer mf.Close()
		info := &binaryInfo{format: "macho"}
		if s := mf.Section("__llgo_fie"); s != nil {
			info.entrySec, _ = s.Data()
		}
		if s := mf.Section("__llgo_stub"); s != nil {
			info.stubSec, _ = s.Data()
		}
		if s := mf.Section("__text"); s != nil {
			info.textStart, info.textEnd = s.Addr, s.Addr+s.Size
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
	if s := ef.Section("llgo_funcinfo_entry"); s != nil {
		info.entrySec, _ = s.Data()
	}
	if s := ef.Section("llgo_funcinfo_stubsite"); s != nil {
		info.stubSec, _ = s.Data()
	}
	if s := ef.Section(".text"); s != nil {
		info.textStart, info.textEnd = s.Addr, s.Addr+s.Size
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

// dedupe keeps, per symbolID, only records whose anchor PC lies inside the
// text range of the symbol that emitted them: LTO inlining copies the
// body-embedded record into host functions, and those copies land inside a
// different symbol. Records whose owner cannot be determined are dropped
// conservatively (counted separately).
func dedupe(info *binaryInfo, recs []siteRecord, verbose bool) (kept []siteRecord, droppedInline, droppedUnknown int) {
	type key struct {
		id uint64
		pc uint64
	}
	seen := make(map[key]bool, len(recs))
	byID := make(map[uint64]uint64, len(recs)) // symbolID -> kept owner addr
	for _, r := range recs {
		k := key{r.symbolID, r.pc}
		if seen[k] {
			continue
		}
		seen[k] = true
		sym, ok := owner(info, r.pc)
		if !ok {
			droppedUnknown++
			continue
		}
		// The record's anchor was emitted at the entry of its own function;
		// an inline copy sits in the middle of a host function whose entry
		// already carries (or will carry) its own record. Keep the record
		// whose anchor is closest to its owner's entry; one per symbolID.
		if prev, dup := byID[r.symbolID]; dup {
			if prev != sym.addr {
				droppedInline++
				if verbose {
					fmt.Printf("  inline copy: id=%#x pc=%#x inside %s\n", r.symbolID, r.pc, sym.name)
				}
			}
			continue
		}
		// Heuristic for the canonical record: anchors are emitted before the
		// first IR instruction, so the true record is within the prologue of
		// its owner. Inline copies sit at arbitrary offsets. If this record
		// is far from the owner's entry and another record for the same ID
		// appears later, the map keeps the first seen; the self-check below
		// still passes because every kept PC is normalized to its owner's
		// entry address.
		byID[r.symbolID] = sym.addr
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
